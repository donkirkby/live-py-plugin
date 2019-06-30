from ast import parse, NodeVisitor

PSEUDO_FILENAME = '<live coding source>'
DEFAULT_MODULE_NAME = '__main__'
LIVE_MODULE_NAME = '__live_coding__'


class TracedFinder(object):
    """ Find which nodes to trace in a module. """
    def __init__(self, source_code, traced, is_suffix_allowed=False, is_live=False):
        """ Initialize the finder.

        :param str source_code: the source code that will be traced, or None if
            the source code should be read from the normal path.
        :param str traced: the module, method, or function name to trace
        :param bool is_suffix_allowed: True if a suffix of traced can match a
            node, otherwise the whole path needs to match.
        :param bool is_live: True if the main module is named with
            LIVE_MODULE_NAME, otherwise it will use DEFAULT_MODULE_NAME.
        """
        self.main_module_name = LIVE_MODULE_NAME if is_live else DEFAULT_MODULE_NAME
        self.traced_node = self.traced_module = None
        self.traced = traced
        self.is_suffix_allowed = is_suffix_allowed
        if traced is None:
            self.is_own_driver = True
            self.source_tree = parse(source_code, PSEUDO_FILENAME)
            self.traced_module = self.main_module_name
        elif source_code is not None:
            self.source_tree = parse(source_code, PSEUDO_FILENAME)
            visitor = TreeVisitor(self)
            visitor.visit(self.source_tree)
            if self.traced_module:
                self.is_own_driver = False
            else:
                self.is_own_driver = True
                self.traced_module = self.main_module_name
        else:
            self.is_own_driver = False
            self.source_tree = None
            self.traced_module = None


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
        if self.finder.is_suffix_allowed:
            active_target = self.target[-len(self.context):]
        else:
            active_target = self.target
        if active_target == self.context:
            self.finder.traced_module = '.'.join(self.target[:-len(self.context)])
            self.finder.traced_node = node
        self.context.pop()
