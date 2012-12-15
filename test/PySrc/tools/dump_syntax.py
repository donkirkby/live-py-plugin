""" This isn't part of the project, just a helper script for dumping syntax
trees from code snippets.
"""

import ast
import sys

code = """\
import sys
from ..turtle import Turtle

sys.exit(3)
"""

tree = ast.parse(code)

print(sys.version)
print(ast.dump(tree))
print('')
for s in tree.body:
    print('    ' + ast.dump(s, include_attributes=False))
