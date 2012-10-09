""" This isn't part of the project, just a helper script for dumping syntax
trees from code snippets.
"""

import ast
import sys

code = """\
f = lambda n: n + 1
x = f(10)
"""

tree = ast.parse(code)

print(sys.version)
print(ast.dump(tree))
print('')
for s in tree.body:
    print('    ' + ast.dump(s, include_attributes=True))
