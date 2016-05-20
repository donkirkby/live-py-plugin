""" This isn't part of the project, just a helper script for dumping syntax
trees from code snippets.
"""

import ast
import sys

code = """\
a[2] = 9
a[i] = 10
"""

tree = ast.parse(code)

print(sys.version)
print(ast.dump(tree, include_attributes=True))
print('')
for s in tree.body:
    print('    ' + ast.dump(s, include_attributes=False))
