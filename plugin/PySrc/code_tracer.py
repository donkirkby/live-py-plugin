from ast import (fix_missing_locations, iter_fields, parse, Assign, AST, 
                 Attribute, BinOp, Call, ExceptHandler, Expr, Index, List, Load,
                 Mod, Name, NodeTransformer, Num, Raise, Return, Slice, Store,
                 Str, Subscript, Tuple, Yield)

try:
    # Import some classes that are only available in Python 3.
    from ast import arg #@UnresolvedImport
except:
    arg = None # If we're in Python 2, we just won't use them.

try:
    # Import some classes that are only available in Python 3.3.
    from ast import Try #@UnresolvedImport
except:
    from ast import TryExcept
    Try = TryExcept

import sys
import traceback

from canvas import Canvas
from mock_turtle import MockTurtle
from report_builder import ReportBuilder

CONTEXT_NAME = '__live_coding_context__'
RESULT_NAME = '__live_coding_result__'
CANVAS_NAME = '__live_canvas__'
TURTLE_NAME = '__live_turtle__'
PSEUDO_FILENAME = '<live coding source>'
MODULE_NAME = '__live_coding__'

class TraceAssignments(NodeTransformer):

    def _set_statement_line_numbers(self, 
                                    statements, 
                                    previous_line_number=None):
        """ Make sure that a series of statements have line numbers in order.
        previous_line_number is the line number to start with, or None."""
        for statement in statements:
            line_number = getattr(statement, 'lineno', None)
            if (line_number is None and statement is not None and 
                previous_line_number is not None):
                statement.lineno = previous_line_number
            else:
                line_numbers = set()
                self._find_line_numbers(statement, line_numbers)
                previous_line_number = max(line_numbers)

    def visit(self, node):
        new_node = super(TraceAssignments, self).visit(node)
        body = getattr(new_node, 'body', None)
        if body is not None:
            previous_line_number = getattr(new_node, 'lineno', None)
            try:
                statements = iter(body)
            except TypeError:
                # body doesn't contain statements
                statements = []
            self._set_statement_line_numbers(statements, previous_line_number)
        return new_node
        
    def _trace_print_function(self, existing_node):
        values = existing_node.args
        message_format = 'print(' + ', '.join(['%r']*len(values)) + ') '
        return self._create_bare_context_call('add_message', 
                                              [BinOp(left=Str(message_format),
                                                     op=Mod(),
                                                     right=Tuple(elts=values,
                                                                 ctx=Load())), 
                                               Num(existing_node.lineno)])
        

    def _get_attribute_names(self, attribute_node):
        names = []
        while isinstance(attribute_node, Attribute):
            names.insert(0, attribute_node.attr)
            attribute_node = attribute_node.value
        
        if not names:
            return None
        
        names.insert(0, getattr(attribute_node, 'id', '<?>'))
        return names
    
    def _get_subscript_repr(self, subscript):
        value = subscript.value
        if isinstance(value, Name):
            value_text = value.id
        elif isinstance(value, Subscript):
            value_text = self._get_subscript_repr(value)
        elif isinstance(value, Attribute):
            value_text = '.'.join(self._get_attribute_names(value))
        else:
            value_text = "..."
        return '{}[{}]'.format(value_text, self._get_slice_repr(subscript.slice))
    
    def _get_slice_repr(self, sliceNode):
        if isinstance(sliceNode, Index):
            return sliceNode.value.n
        if isinstance(sliceNode, Slice):
            return '{}:{}'.format(sliceNode.lower and sliceNode.lower.n or '',
                                  sliceNode.upper and sliceNode.upper.n or '')
        return '...'
        

    def visit_Call(self, node):
        existing_node = self.generic_visit(node)
        value_node = existing_node.func
        
        if isinstance(value_node, Name) and value_node.id == 'print':
            return self._trace_print_function(existing_node)
        
        names = self._get_attribute_names(value_node)
        if names is None:
            return existing_node
        
        args = [Str(s='.'.join(names[:-1])),
                Call(func=Name(id='repr', ctx=Load()),
                     args=[existing_node.func.value],
                     keywords=[],
                     starargs=None,
                     kwargs=None),
                existing_node,
                Call(func=Name(id='repr', ctx=Load()),
                     args=[existing_node.func.value],
                     keywords=[],
                     starargs=None,
                     kwargs=None),
                Num(n=existing_node.lineno)]
        new_node = self._create_bare_context_call('record_call', args)
        return new_node

    def visit_Print(self, node):
        existing_node = self.generic_visit(node)
        values = existing_node.values
        message_format = 'print' + ','.join([' %r']*len(values)) + ' '
        return self._create_context_call('add_message', 
                                         [BinOp(left=Str(message_format),
                                                op=Mod(),
                                                right=Tuple(elts=values,
                                                            ctx=Load())), 
                                          Num(existing_node.lineno)])
    
    def visit_Assign(self, node):
        existing_node = self.generic_visit(node)
        new_nodes = [existing_node]
        existing_node.value = self._create_bare_context_call(
            'assign', 
            [Str(s=self._get_assignment_targets(existing_node.targets)), 
             existing_node.value, 
             Num(n=existing_node.lineno)])

        return new_nodes
    
    def visit_AugAssign(self, node):
        existing_node = self.generic_visit(node)
        new_nodes = [existing_node]
        new_nodes.append(self._trace_assignment(existing_node.target))
        return new_nodes
    
    def _find_line_numbers(self, node, line_numbers):
        """ Populates a set containing all line numbers used by the node and its
        descendants.
        
        line_numbers is a set that all the line numbers will be added to."""
        line_number = getattr(node, 'lineno', None)
        if line_number is not None:
            line_numbers.add(line_number)
        for _, value in iter_fields(node):
            if isinstance(value, list):
                for item in value:
                    if isinstance(item, AST):
                        self._find_line_numbers(item, line_numbers)
            elif isinstance(value, AST):
                self._find_line_numbers(value, line_numbers)
    
    def visit_For(self, node):
        new_node = self.generic_visit(node)
        
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_body = [self._create_context_call('start_block', args)]
        new_body.extend(self._trace_assignment_list(new_node.target))
        new_body.extend(new_node.body)
        new_node.body = new_body
        return new_node
    
    def visit_While(self, node):
        new_node = self.generic_visit(node)
        
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_node.body.insert(0,
                             self._create_context_call('start_block', args))
        return new_node
    
    def visit_FunctionDef(self, node):
        """ Instrument a function definition by creating a new report builder
        for this stack frame and putting it in a local variable. The local
        variable has the same name as the global variable so all calls can
        use the same CONTEXT_NAME symbol, but it means that I had to use this:
        x = globals()['x'].start_frame()
        Kind of ugly, but I think it was worth it to handle recursive calls.
        """
        if node.name == '__repr__':
            return node
        
        new_node = self.generic_visit(node)
        
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        first_line_number = min(line_numbers)
        last_line_number = max(line_numbers)
        args = [Num(n=first_line_number),
                Num(n=last_line_number)]
        try_body = new_node.body
        globals_call = Call(func=Name(id='globals', ctx=Load()), 
                            args=[], 
                            keywords=[], 
                            starargs=None, 
                            kwargs=None)
        global_context = Subscript(value=globals_call, 
                                   slice=Index(value=Str(s=CONTEXT_NAME)), 
                                   ctx=Load())
        start_frame_call = Call(func=Attribute(value=global_context, 
                                               attr='start_frame', 
                                               ctx=Load()), 
                                args=args, 
                                keywords=[], 
                                starargs=None, 
                                kwargs=None)
        context_assign = Assign(targets=[Name(id=CONTEXT_NAME, ctx=Store())],
                                value=start_frame_call)
        new_node.body = [context_assign]
        
        # trace function parameter values
        for target in new_node.args.args:
            if isinstance(target, Name) and target.id == 'self':
                continue
            if arg and isinstance(target, arg) and target.arg == 'self':
                continue
            new_node.body.append(self._trace_assignment(target, node.lineno))

        handler_body = [self._create_context_call('exception'),
                        Raise()]
        new_node.body.append(
            Try(body=try_body,
                handlers=[ExceptHandler(body=handler_body)],
                orelse=[],
                finalbody=[]))
        self._set_statement_line_numbers(try_body, first_line_number)
        self._set_statement_line_numbers(handler_body, last_line_number)
        return new_node

    def visit_Module(self, node):
        new_node = self.generic_visit(node)
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        if line_numbers:
            first_line_number = min(line_numbers)
            last_line_number = max(line_numbers)
        else:
            first_line_number = last_line_number = 1
        try_body = new_node.body
        handler_body = [self._create_context_call('exception')]
        handler = ExceptHandler(body=handler_body,
                                lineno=last_line_number)
        if not try_body:
            # empty module
            new_node.body = try_body
        else:
            new_node.body = [Try(body=try_body,
                                 handlers=[handler],
                                 orelse=[],
                                 finalbody=[])]
        self._set_statement_line_numbers(try_body, first_line_number)
        self._set_statement_line_numbers(handler_body, last_line_number)
        return new_node

    def visit_Lambda(self, node):
        """ Instrument a lambda expression by displaying the parameter values.
        
        We create calls to trace assignment to each argument, then wrap them
        all in a tuple together with the original expression, and pull the
        original expression out of the tuple.
        """
        
        new_node = self.generic_visit(node)
        
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        
        # trace lambda argument values
        calls = [getattr(self._trace_assignment(target, node.lineno), 
                         'value', 
                         None)
                 for target in new_node.args.args
                 if getattr(target, 'id', 'self') != 'self' or 
                 getattr(target, 'arg', 'self') != 'self']

        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        calls.insert(0, self._create_context_call('start_block', args).value)
        calls.append(new_node.body)
        new_node.body = Subscript(value=Tuple(elts=calls,
                                              ctx=Load()),
                                  slice=Index(value=Num(n=-1)),
                                  ctx=Load())
        return new_node
    
    def visit_Return(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        if value is None:
            value = Name(id='None', ctx=Load())
        
        return [Assign(targets=[Name(id=RESULT_NAME, ctx=Store())],
                       value=value),
                self._create_context_call('return_value', 
                                          [Name(id=RESULT_NAME, ctx=Load()),
                                           Num(n=existing_node.lineno)]),
                Return(value=Name(id=RESULT_NAME, ctx=Load()))]
    
    def visit_Yield(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        if value is None:
            value = Name(id='None', ctx=Load())
        
        return Yield(value=self._create_bare_context_call(
                    'yield_value', 
                    [value, Num(n=existing_node.lineno)]))
    
    def _trace_assignment_list(self, targets):
        """ Build a list of assignment calls based on the contents of targets.
        If targets is a single name, then return a list with one call.
        If targets is a Tuple or a List, then make recursive calls for each
        item, combine the results into a list, and return it."""
        
        new_nodes = []
        # Tuple and List hold their contents in elts.
        todo = getattr(targets, 'elts', targets)
        try:
            todo = list(todo)
        except TypeError:
            # todo wasn't iterable, treat it as a single item
            trace = self._trace_assignment(targets)
            if trace:
                new_nodes.append(trace)
            return new_nodes
        for target in todo:
            new_nodes.extend(self._trace_assignment_list(target))
        return new_nodes

    def _trace_assignment(self, target, default_lineno=None):
        lineno = getattr(target, 'lineno', default_lineno)
        #name, value, line number
        if isinstance(target, Name):
            args = [Str(s=target.id), 
                    Name(id=target.id, ctx=Load()),
                    Num(n=lineno)]
        elif arg and isinstance(target, arg):
            args=[Str(s=target.arg),
                  Name(id=target.arg, ctx=Load()),
                  Num(n=lineno)]
        else:
            raise TypeError('Unknown target: %s' % target)
            
        return self._create_context_call('assign', args)
        
    def _get_assignment_target(self, target):
        if isinstance(target, Name):
            target_name = target.id
        elif isinstance(target, Subscript):
            target_name = self._get_subscript_repr(target)
        elif isinstance(target, Tuple) or isinstance(target, List):
            target_names = map(self._get_assignment_target, target.elts)
            target_name = '({})'.format(', '.join(target_names))
        elif isinstance(target, Attribute):
            names = self._get_attribute_names(target)
            target_name = '.'.join(names)
        else:
            raise TypeError('Unknown target: %s' % target)
        return target_name

    def _get_assignment_targets(self, targets):
        """ Build a string representation of assignment targets.
        
        For example, "x = " for a single target, or "x = y = " for multiple
        targets.
        """
        result = []
        for target in targets:
            target_name = self._get_assignment_target(target)
            result.append(target_name)
        return ' = '.join(result)
        
    def _create_context_call(self, function_name, args=None):
        return Expr(value=self._create_bare_context_call(function_name, args))
        
    def _create_bare_context_call(self, function_name, args=None):
        if args is None:
            args = []
        context_name = Name(id=CONTEXT_NAME, ctx=Load())
        function = Attribute(value=context_name,
                             attr=function_name,
                             ctx=Load())
        return Call(func=function,
                    args=args,
                    keywords=[],
                    starargs=None,
                    kwargs=None)

class CodeTracer(object):
    def __init__(self, turtle=None):
        self.message_limit = 10000
        self.max_width = None
        self.keepalive = False
        self.turtle = turtle if turtle else MockTurtle()
        self.environment = {'__name__': MODULE_NAME, 
                            CANVAS_NAME: self.turtle.screen.cv, 
                            TURTLE_NAME: self.turtle}
        
    def trace_canvas(self, source):
        exec(source, self.environment, self.environment)
        
        return '\n'.join(self.turtle.screen.cv.report)
        
    def trace_turtle(self, source):
        exec(source, self.environment, self.environment)
        
        return '\n'.join(self.turtle.report)
        
    def trace_code(self, source):
        builder = ReportBuilder(self.message_limit)
        builder.max_width = self.max_width

        try:
            tree = parse(source)
    
            visitor = TraceAssignments()
            new_tree = visitor.visit(tree)
            fix_missing_locations(new_tree)
#             from ast import dump
#             print(dump(new_tree, include_attributes=False))
            code = compile(new_tree, PSEUDO_FILENAME, 'exec')
            
            self.environment[CONTEXT_NAME] = builder
            exec(code, self.environment, self.environment)
        except SyntaxError:
            ex = sys.exc_info()[1]
            messages = traceback.format_exception_only(type(ex), ex)
            builder.add_message(messages[-1].strip() + ' ', ex.lineno)
        except:
            etype, value, tb = sys.exc_info()
            is_reported = False
            builder.message_limit = None # make sure we don't hit limit
            builder.max_width = None # make sure we don't hit limit
            messages = traceback.format_exception_only(etype, value)
            message = messages[-1].strip() + ' '
            entries = traceback.extract_tb(tb)
            for filename, line_number, _, _ in entries:
                if filename == PSEUDO_FILENAME:
                    builder.add_extra_message(message, line_number)
                    is_reported = True
            if not is_reported:
                builder.add_message(message, 1)
#                print('=== Unexpected Exception in tracing code ===')
#                traceback.print_exception(etype, value, tb)
                
        return builder.report()
    
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Trace Python code.')
    parser.add_argument('-c', 
                        '--canvas', 
                        action='store_true', 
                        help='Should canvas commands be printed?')
    parser.add_argument('-x', 
                        '--width', 
                        type=int,
                        default=800,
                        help='width of the canvas in pixels')
    parser.add_argument('-y', 
                        '--height', 
                        type=int,
                        default=600,
                        help='height of the canvas in pixels')
    
    args = parser.parse_args()
    code = sys.stdin.read()
    canvas = Canvas(args.width, args.height)
    turtle = MockTurtle(canvas=canvas)
    tracer = CodeTracer(turtle)
    tracer.max_width = 200000
    code_report = tracer.trace_code(code)
    turtle_report = tracer.turtle.report
    if turtle_report and args.canvas:
        print('start_canvas')
        print('\n'.join(turtle_report))
        print('end_canvas')
        print('.')
    print(code_report)

elif __name__ == '__live_coding__':
    import unittest
    def test_something(self):
        # SETUP
        code = """\
f = lambda n: n + 1
x = f(10)
"""
        expected_report = """\
n = 10 
x = 11 """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY        
        self.assertMultiLineEqual(expected_report, report)

    class DummyTest(unittest.TestCase):
        def test_delegation(self):
            test_something(self)

    suite = unittest.TestSuite()
    suite.addTest(DummyTest("test_delegation"))
    test_results = unittest.TextTestRunner().run(suite)

    print(test_results.errors)
    print(test_results.failures)
    
    
    
    