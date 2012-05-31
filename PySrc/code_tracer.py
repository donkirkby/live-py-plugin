from ast import (copy_location, parse, Attribute, Call, Expr, Load, Name, Num, 
                 NodeTransformer, Str)
import sys

from report_builder import ReportBuilder

CONTEXT_NAME = '__live_coding_context__'

class TraceAssignments(NodeTransformer):
    def visit_Assign(self, node):
        existing_node = self.generic_visit(node)
        new_nodes = [existing_node]
        for target in existing_node.targets:
            #name, value, linenumber
            args = [copy_location(Str(s=target.id), target),
                    copy_location(Name(id=target.id, ctx=Load()), target),
                    copy_location(Num(n=target.lineno, lineno=5, col_offset=31), 
                                  target)]
            context_name = copy_location(Name(id=CONTEXT_NAME, ctx=Load()),
                                         target)
            function = copy_location(Attribute(value=context_name,
                                               attr='assign', 
                                               ctx=Load()),
                                     target)
            new_nodes.append(copy_location(Expr(value=copy_location(Call(func=function, 
                                                            args=args, 
                                                            keywords=[], 
                                                            starargs=None, 
                                                            kwargs=None),
                                                      target)),
                                           target))
        return new_nodes

class CodeTracer(object):
    def trace_code(self, code):
        tree = parse(code)
        
        new_tree = TraceAssignments().visit(tree)
        
#        print ast.dump(new_tree)
#        for s in new_tree.body:
#            print '    ' + ast.dump(s, include_attributes=True)
        
        code = compile(new_tree, '<string>', 'exec')
        
        builder = ReportBuilder()
        builder.start_block(1, 1000)
        env = {'__live_coding_context__': builder}
        
        exec code in env
        
        return builder.report().strip('\n')
    
if __name__ == '__main__':
    code = sys.stdin.read()
    
    print CodeTracer().trace_code(code)