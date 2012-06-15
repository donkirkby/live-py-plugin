from ast import (fix_missing_locations, iter_fields, parse, Assign, AST, 
                 Attribute, Call, Expr, Load, Name, NodeTransformer, Num, 
                 Return, Store, Str, Subscript)
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
        if not isinstance(existing_node.func, Attribute):
            return existing_node
        
        args = [Str(s=existing_node.func.value.id),
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
        
    def trace_code(self, source):
        builder = ReportBuilder(self.message_limit)

        try:
            tree = parse(source)
        
            visitor = TraceAssignments()
            new_tree = visitor.visit(tree)
            line_numbers = set()
            visitor._find_line_numbers(new_tree, line_numbers)
            fix_missing_locations(new_tree)
            fixed_line_numbers = set()
            visitor._find_line_numbers(new_tree, fixed_line_numbers)
            code = compile(new_tree, PSEUDO_FILENAME, 'exec')
            
            env = {CONTEXT_NAME: builder, '__name__': '__live_coding__'}
        
            exec code in env
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
    
if __name__ == '__main__':
    code = sys.stdin.read()
    
    print CodeTracer().trace_code(code)