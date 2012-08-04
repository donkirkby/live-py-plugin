from ast import (fix_missing_locations, iter_fields, parse, Assign, AST, 
                 Attribute, Call, Expr, Load, Name, NodeTransformer, Num, 
                 Return, Store, Str, Subscript)
from turtle import Turtle
import sys

from report_builder import ReportBuilder
import traceback

CONTEXT_NAME = '__live_coding_context__'
RESULT_NAME = '__live_coding_result__'
PSEUDO_FILENAME = '<live coding source>'

class TraceAssignments(NodeTransformer):
    def visit(self, node):
        new_node = super(TraceAssignments, self).visit(node)
        body = getattr(new_node, 'body', None)
        if body is not None:
            previous_line_number = getattr(new_node, 'lineno', None)
            for statement in body:
                line_number = getattr(statement, 'lineno', None)
                if line_number is None and previous_line_number is not None:
                    statement.lineno = previous_line_number
                else:
                    previous_line_number = line_number
        return new_node
        
    def visit_Call(self, node):
        existing_node = self.generic_visit(node)
        value_node = existing_node.func
        
        names = []
        while isinstance(value_node, Attribute):
            names.insert(0, value_node.attr)
            value_node = value_node.value
        if not names or not hasattr(value_node, 'id'):
            return existing_node
        names.insert(0, value_node.id)
        
        args = [Str(s='.'.join(names[0:-1])),
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

    def visit_Assign(self, node):
        existing_node = self.generic_visit(node)
        new_nodes = [existing_node]
        for target in existing_node.targets:
            trace = self._trace_assignment(target)
            if trace:
                new_nodes.append(trace)

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
        new_node.body.insert(0, 
                             self._trace_assignment(new_node.target))
        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_node.body.insert(0,
                             self._create_context_call('start_block', args))
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
        if node.name == '__repr__':
            return node
        
        new_node = self.generic_visit(node)
        
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        
        # trace function parameter values
        argument_count = 0
        for target in new_node.args.args:
            if isinstance(target, Name) and target.id == 'self':
                continue
            new_node.body.insert(argument_count, 
                                 self._trace_assignment(target))
            argument_count += 1

        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_node.body.insert(0,
                             self._create_context_call('start_block', args))
        return new_node
    
    def visit_Return(self, node):
        existing_node = self.generic_visit(node)
        value = existing_node.value
        
        return [Assign(targets=[Name(id=RESULT_NAME, ctx=Store())],
                       value=value),
                self._create_context_call('return_value', 
                                          [Name(id=RESULT_NAME, ctx=Load()),
                                           Num(n=existing_node.lineno)]),
                Return(value=Name(id=RESULT_NAME, ctx=Load()))]
    
    def _trace_assignment(self, target):
        #name, value, line number
        if isinstance(target, Name):
            args = [Str(s=target.id), 
                    Name(id=target.id, ctx=Load()),
                    Num(n=target.lineno)]
        elif isinstance(target, Subscript):
            subtarget = target
            while isinstance(subtarget, Subscript):
                subtarget = subtarget.value
            args = [Str(s=subtarget.id),
                    Name(id=subtarget.id, ctx=Load()),
                    Num(n=target.lineno)]
        elif isinstance(target, Attribute):
            args = [Str(s='%s.%s' % (target.value.id, target.attr)),
                    Attribute(value=target.value, 
                              attr=target.attr, 
                              ctx=Load()),
                    Num(n=target.lineno)]
        else:
            return None
            
        return self._create_context_call('assign', args)
        
    def _create_context_call(self, function_name, args):
        return Expr(value=self._create_bare_context_call(function_name, args))
        
    def _create_bare_context_call(self, function_name, args):
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
    def __init__(self):
        self.message_limit = 1000
        self.keepalive = False
        self.environment = {}
        
    def trace_code(self, source):
        builder = ReportBuilder(self.message_limit)

        try:
            tree = parse(source)
        
            visitor = TraceAssignments()
            new_tree = visitor.visit(tree)
            fix_missing_locations(new_tree)
            
            code = compile(new_tree, PSEUDO_FILENAME, 'exec')
            
            if not self.keepalive:
                self.environment = {}
            self.environment[CONTEXT_NAME] = builder
            self.environment['__name__'] = '__live_coding__'
        
            exec code in self.environment
        except SyntaxError, ex:
            messages = traceback.format_exception_only(type(ex), ex)
            builder.add_message(messages[-1].strip() + ' ', ex.lineno)
        except:
            exc_info = sys.exc_info()
            try:
                is_reported = False
                builder.message_limit = None # make sure we don't hit limit
                etype, value, tb = exc_info
                messages = traceback.format_exception_only(etype, value)
                message = messages[-1].strip() + ' '
                entries = traceback.extract_tb(tb)
                for filename, line_number, _, _ in entries:
                    if filename == PSEUDO_FILENAME:
                        builder.add_message(message, line_number)
                        is_reported = True
                if not is_reported:
                    builder.add_message(message, 1)
            finally:
                del tb
                del exc_info # prevents circular reference
                
        return builder.report()
    
    def encode(self, source):
        return source.replace('%', 
                              '%25').replace('\r', 
                                             '%0d').replace('\n', 
                                                            '%0a')
    
    def decode(self, encoded):
        return encoded.replace('%0a', 
                               '\n').replace('%0d',
                                             '\r').replace('%25', 
                                                           '%')
    
if __name__ == '__main__':
    tracer = CodeTracer()
    t = None
    if '-t' in sys.argv:
        t = Turtle()
        tracer.environment['turtle'] = t
    if '-k' in sys.argv:
        line = ''
        tracer.keepalive = True
        while not line is None:
            line = sys.stdin.readline()
            if not line is None:
                if t:
                    t.tracer(100000)
                    t.reset()
                code = tracer.decode(line)
                print tracer.encode(tracer.trace_code(code))
                sys.stdout.flush()
                if t:
                    t.tracer(1)
    else:
        code = sys.stdin.read()
        code = code.strip().replace('\r\n', '\n').replace('\r', '\n') + '\n'

        print tracer.trace_code(code)
