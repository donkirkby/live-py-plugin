""" This isn't part of the project, just a helper script for dumping syntax
trees from code snippets.
"""

import ast
import sys

code = """\
try:
    def f(n):
        try:
            m = n - 1
            if m == 0:
                raise RuntimeError('Invalid n.')
            return f(m)
        except:
            dump()
            raise
    
    r = f(2)
except:
    dump()
"""

tree = ast.parse(code)

print(sys.version)
print(ast.dump(tree, include_attributes=True))
print('')
for s in tree.body:
    print('    ' + ast.dump(s, include_attributes=True))

