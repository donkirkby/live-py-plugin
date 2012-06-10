import ast

class PrintAssignments(ast.NodeTransformer):
    def visit_Assign(self, node):
        existing_node = self.generic_visit(node)
        values = []
        for target in existing_node.targets:
            values.append(ast.copy_location(ast.Name(id=target.id, 
                                                     ctx=ast.Load()),
                                            target))
        print_node = ast.Print(dest=None,
                               values=values, 
                               nl=True)
        return [existing_node, ast.copy_location(print_node, existing_node)]

code = """\
class Dog(object):
    pass

dog = Dog()
dog.name = "Spot"
"""

tree = ast.parse(code)

#    Print(dest=None, values=[Name(id='x', ctx=Load(), lineno=1, col_offset=13)], nl=True, lineno=1, col_offset=7)
#tree.body.insert(1, ast.Print(dest=None, values=[ast.Name(id='x', ctx=ast.Load(), lineno=1, col_offset=0)], nl=True, lineno=1, col_offset=0))
#    Print(dest=None, values=[BinOp(left=Str(s='x = %r', lineno=2, col_offset=6), op=Mod(), right=Name(id='x', ctx=Load(), lineno=2, col_offset=17), lineno=2, col_offset=6)], nl=True, lineno=2, col_offset=0)
#    Global(names=['__live_coding_context__'], lineno=1, col_offset=0)
#    Expr(value=Call(func=Attribute(value=Name(id='__live_coding_context__', ctx=Load(), lineno=3, col_offset=0), attr='append', ctx=Load(), lineno=3, col_offset=0), args=[Name(id='x', ctx=Load(), lineno=3, col_offset=31)], keywords=[], starargs=None, kwargs=None, lineno=3, col_offset=0), lineno=3, col_offset=0)

new_tree = tree
#new_tree = PrintAssignments().visit(tree)

print ast.dump(new_tree)
for s in new_tree.body:
    print '    ' + ast.dump(s)

code = compile(new_tree, '<string>', 'exec')

env = {'__live_coding_context__': []}

exec code in env

print env['__live_coding_context__']
