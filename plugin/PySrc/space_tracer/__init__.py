import sys

try:
    from .main import main, web_main, traced
    from .about import __title__, __version__, __url__
except (ImportError, SyntaxError):
    if sys.version_info < (3,):
        print('Python 2 is no longer supported.')
        exit(1)
