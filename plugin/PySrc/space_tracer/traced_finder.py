from ast import parse, NodeVisitor

PSEUDO_FILENAME = 'live_source.py'
DEFAULT_MODULE_NAME = '__main__'
LIVE_MODULE_NAME = '__live_coding__'


class TracedFinder(object):
    """ Find which nodes to trace in a module. """
    def __init__(self, source_code, traced, filename=None, parse=True):
        """ Initialize the finder.

        :param str source_code: the source code that will be traced, or None if
            the source code should be read from the normal path.
        :param str traced: the module, method, or function name to trace
        :param str filename: the file the source code was read from
        :param bool parse: True if the source code should be parsed immediately
        """
        self.source_code = source_code
        self.traced = traced
        self.traced_node = None
        self.filename = filename or PSEUDO_FILENAME
        self.source_tree = None
        self.is_tracing = False
        if parse:
            self.parse()

    def parse(self):
        self.source_tree = parse(self.source_code, self.filename)
        visitor = TreeVisitor(self)
        visitor.visit(self.source_tree)
        self.is_tracing = self.traced_node is not None


# noinspection PyPep8Naming
class TreeVisitor(NodeVisitor):
    def __init__(self, finder):
        self.finder = finder
        self.target = finder.traced.split('.')
        self.context = []

    def visit_FunctionDef(self, node):
        self.visit_node(node)

    def visit_ClassDef(self, node):
        self.visit_node(node)

    def visit_node(self, node):
        name = node.name
        self.context.append(name)
        self.generic_visit(node)
        if self.target == self.context:
            self.finder.traced_node = node
        self.context.pop()
