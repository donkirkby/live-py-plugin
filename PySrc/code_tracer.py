from ast import (fix_missing_locations, iter_fields, parse, AST, Attribute, Call, Expr, Load, 
                 Name, 
                 NodeTransformer, Num, Str, Subscript)
import sys

from report_builder import ReportBuilder

CONTEXT_NAME = '__live_coding_context__'

class TraceAssignments(NodeTransformer):
    def visit_Assign(self, node):
        existing_node = self.generic_visit(node)
        new_nodes = [existing_node]
        for target in existing_node.targets:
            new_nodes.append(self._trace_assignment(target))
                
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
                             self._create_context_call('start_block', 
                                                       args, 
                                                       new_node))
        new_node.body.append(self._create_context_call('end_block', 
                                                       [], 
                                                       new_node))
        return new_node
    
    def visit_FunctionDef(self, node):
        new_node = self.generic_visit(node)
        
        line_numbers = set()
        self._find_line_numbers(new_node, line_numbers)
        
        # trace function parameter values
        argument_count = 0
        for target in new_node.args.args:
            new_node.body.insert(argument_count, 
                                 self._trace_assignment(target))
            argument_count += 1

        args = [Num(n=min(line_numbers)),
                Num(n=max(line_numbers))]
        new_node.body.insert(0,
                             self._create_context_call('start_block', 
                                                       args, 
                                                       new_node))
        new_node.body.append(self._create_context_call('end_block', 
                                                       [], 
                                                       new_node))
        return new_node
    
    def visit_Return(self, node):
        existing_node = self.generic_visit(node)
        
        return [self._trace_return(existing_node.value), existing_node]
    
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
            
        return self._create_context_call('assign', args, target)
        
    def _trace_return(self, value):
        #name, value, line number
        if isinstance(value, Name):
            args = [Name(id=value.id, ctx=Load()),
                    Num(n=value.lineno)]
        elif isinstance(value, Subscript):
            args = [value,
                    Num(n=value.lineno)]
            
        return self._create_context_call('return_value', args, value)
        
    def _create_context_call(self, function_name, args, old_node):
        context_name = Name(id=CONTEXT_NAME, ctx=Load())
        function = Attribute(value=context_name,
                             attr=function_name,
                             ctx=Load())
        call = Call(func=function,
                    args=args,
                    keywords=[],
                    starargs=None,
                    kwargs=None)
        return Expr(value=call)

class CodeTracer(object):
    def trace_code(self, code):
        tree = parse(code)
        
        new_tree = TraceAssignments().visit(tree)
        fix_missing_locations(new_tree)
        
        code = compile(new_tree, '<string>', 'exec')
        
        builder = ReportBuilder()
        builder.start_block(1, 1000)
        env = {CONTEXT_NAME: builder}
        
        exec code in env
        
        return builder.report()
    
if __name__ == '__main__':
    code = sys.stdin.read()
    
    print CodeTracer().trace_code(code)