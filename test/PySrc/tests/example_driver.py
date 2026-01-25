import sys

from example_package.lib_in_package import add_message
from example_source import foo

assert 'fail' not in sys.argv, sys.argv[1:]

try:
    foo(42)
finally:
    if 'driver-exit' in sys.argv:
        exit('Exit requested.')
add_message('from driver')

sys.exit(0)  # Some drivers exit successfully like this.
