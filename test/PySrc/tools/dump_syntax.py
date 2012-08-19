""" This isn't part of the project, just a helper script for dumping syntax
trees from code snippets.
"""

import ast

code = """\
f = (lambda n,
            x: (n + 
                x))
x = f(10)
"""

tree = ast.parse(code)

print ast.dump(tree)
print
for s in tree.body:
    print '    ' + ast.dump(s, include_attributes=True)
