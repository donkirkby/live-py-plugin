from .main import main
from .about import __title__, __version__, __url__


def traced(target):
    """ A decorator for a function that should be traced. """
    return target
