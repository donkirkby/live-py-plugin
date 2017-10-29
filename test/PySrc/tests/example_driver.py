import sys

from example_package.lib_in_package import add_message
from example_source import foo

assert 'fail' not in sys.argv, sys.argv[1:]

if 'skip' not in sys.argv:
    foo(42)
    add_message('from driver')
