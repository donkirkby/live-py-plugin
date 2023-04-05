import os
import sys
from subprocess import check_output, CalledProcessError

if sys.version_info >= (3, 0):
    print('This is modern Python. Nothing to validate.')
    exit()

source_path = os.path.abspath(os.path.join(__file__, '../example_printing.py'))
lib_path = os.path.abspath(os.path.join(__file__, '../../../../plugin/PySrc'))
args = [sys.executable, '-mspace_tracer', source_path]

try:
    check_output(args, cwd=lib_path).decode('utf8')
    exit('Space Tracer should fail under Python 2.')
except CalledProcessError as ex:
    assert ex.output == 'Python 2 is no longer supported.\n', ex.output

print('OK')
