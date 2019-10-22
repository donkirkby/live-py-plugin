import io
import sys
from ast import parse
from importlib import import_module
from base64 import standard_b64encode

try:
    from importlib.abc import MetaPathFinder, Loader
    from importlib.machinery import ModuleSpec
    from importlib.util import find_spec
    imp = None
except ImportError:
    # Stub out the classes for older versions of Python.
    class MetaPathFinder(object):
        pass

    Loader = ModuleSpec = object
    find_spec = None
    import imp
try:
    builtins = import_module('__builtin__')
except ImportError:
    import builtins

from .code_tracer import trace_source_tree
from .mock_turtle import MockTurtle, monkey_patch_pyglet
from .traced_finder import DEFAULT_MODULE_NAME, LIVE_MODULE_NAME, \
    PSEUDO_FILENAME, TracedFinder


class DelegatingModuleFinder(MetaPathFinder):
    def find_spec(self, fullname, path, target):
        for finder in self.following_finders:
            finder_find_spec = getattr(finder, 'find_spec', None)
            if finder_find_spec:
                spec = finder_find_spec(fullname, path, target)
                if spec is not None:
                    return spec

    # find_module() and load_module() are used in Python 2.
    def find_module(self, fullname, path):
        for finder in self.following_finders:
            loader = finder.find_module(fullname, path)
            if loader is not None:
                return loader

    @property
    def following_finders(self):
        is_after = False
        for finder in sys.meta_path:
            if not is_after:
                is_after = finder is self
                continue
            yield finder


# noinspection PyAbstractClass
class TracedModuleImporter(DelegatingModuleFinder, Loader):
    def __init__(self,
                 source_code,
                 traced,
                 environment,
                 filename,
                 driver_module):
        """ Import the code that has been instrumented for live coding.

        :param str source_code: source code to load for traced module
        :param environment: global variables for the module
        :param filename: the name of the file this code came from
        :param str driver_module: module name, if the driver is a module
        """
        self.source_code = source_code
        self.traced = traced
        self.environment = environment
        self.filename = filename
        self.driver_module = driver_module
        self.source_finder = None
        self.driver_finder = None
        self.module_files = {}

    def find_spec(self, fullname, path, target=None):
        if (fullname == self.traced or
                fullname in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME) and
                self.traced.startswith(fullname)):
            return ModuleSpec(fullname, self, origin=self.filename)
        spec = super(TracedModuleImporter, self).find_spec(fullname,
                                                           path,
                                                           target)
        if spec is not None:
            if spec.origin == self.filename:
                return ModuleSpec(fullname, self, origin=self.filename)
            return spec
        return None

    def exec_module(self, module):
        module_spec = getattr(module, '__spec__', None)
        if module_spec:
            module_file = module_spec.origin
        else:
            module_file = self.filename
        parsed_filename = module_file
        if (self.traced.startswith(DEFAULT_MODULE_NAME) or
                self.traced.startswith(LIVE_MODULE_NAME)):
            source_code = self.source_code
            parsed_filename = PSEUDO_FILENAME
        elif self.filename is not None and module_file == self.filename:
            if self.source_code is None:
                with open(self.filename) as source_file:
                    self.source_code = source_file.read()
            source_code = self.source_code
        else:
            with open(module_file) as source_file:
                source_code = source_file.read()
        module_name = module.__name__
        is_module_traced = False
        source_tree = None
        if self.traced == module_name:
            is_module_traced = True
            self.source_finder = TracedFinder(source_code, '', parsed_filename)
        else:
            if self.traced.startswith(module_name):
                traced_child = self.traced[len(module_name)+1:]
            elif self.traced in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME):
                traced_child = self.traced
            else:
                traced_child = None
            if traced_child:
                source_finder = TracedFinder(source_code,
                                             traced_child,
                                             parsed_filename)
                source_tree = source_finder.source_tree
                if source_finder.traced_node is not None:
                    is_module_traced = True
                    self.source_finder = source_finder
        if source_tree is None:
            source_tree = parse(source_code, parsed_filename)
        if is_module_traced:
            source_tree = trace_source_tree(source_tree)
        if (module_name in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME) and
                self.driver_module):
            target_module = self.driver_module
        else:
            target_module = module_name
        if '.' in target_module:
            package_name, child_name = target_module.rsplit('.', 1)
        else:
            package_name = None
        module.__package__ = package_name
        module.__file__ = module_file
        module.__builtins__ = builtins
        module.__dict__.update(self.environment)
        self.environment = module.__dict__
        # from ast import dump
        # print(dump(source_tree, include_attributes=True))
        compiled_code = compile(source_tree, PSEUDO_FILENAME, 'exec')

        exec(compiled_code, self.environment)

    # find_module() and load_module() are used in Python 2.
    def find_module(self, fullname, path=None):
        if (fullname == self.traced or
                fullname in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME) and
                self.traced.startswith(fullname)):
            return self

    def load_module(self, fullname):
        # noinspection PyDeprecation
        new_mod = imp.new_module(fullname)
        sys.modules[fullname] = new_mod

        self.exec_module(new_mod)
        return new_mod


class PatchedModuleFinder(DelegatingModuleFinder):
    is_desperate = False

    def __init__(self, is_zoomed):
        self.is_zoomed = is_zoomed

    def find_spec(self, fullname, path, target=None):
        if fullname not in ('matplotlib',
                            'matplotlib.pyplot',
                            'numpy.random',
                            'random',
                            'pyglet'):
            return None
        spec = super(PatchedModuleFinder, self).find_spec(fullname, path, target)
        if spec is not None:
            spec.loader = PatchedModuleLoader(fullname,
                                              spec.loader,
                                              self.is_zoomed)
            return spec

    # find_module() and load_module() are used in Python 2.
    def find_module(self, fullname, path=None):
        if fullname not in ('matplotlib',
                            'matplotlib.pyplot',
                            'numpy.random',
                            'random',
                            'pyglet'):
            return None
        loader = super(PatchedModuleFinder, self).find_module(fullname, path)
        if loader is not None:
            return PatchedModuleLoader(fullname, loader, self.is_zoomed)
        if sys.version_info < (3, 0) and not PatchedModuleFinder.is_desperate:
            # Didn't find anyone to load the module, get desperate.
            PatchedModuleFinder.is_desperate = True
            return PatchedModuleLoader(fullname, None, self.is_zoomed)


# noinspection PyAbstractClass
class PatchedModuleLoader(Loader):
    def __init__(self, fullname, main_loader, is_zoomed):
        self.fullname = fullname
        self.main_loader = main_loader
        self.is_zoomed = is_zoomed
        self.plt = None

    def exec_module(self, module):
        if self.main_loader is not None:
            self.main_loader.exec_module(module)
        if self.fullname in ('numpy.random', 'random'):
            module.seed(0)
        elif self.fullname == 'matplotlib':
            module.use('Agg')
        elif self.fullname == 'matplotlib.pyplot':
            self.plt = module
            # noinspection PyProtectedMember
            turtle_screen = MockTurtle._screen
            screen_width = turtle_screen.cv.cget('width')
            screen_height = turtle_screen.cv.cget('height')
            module.show = self.mock_show
            module.live_coding_size = (screen_width, screen_height)
            module.live_coding_zoom = self.live_coding_zoom
            if self.is_zoomed:
                self.live_coding_zoom()
        elif self.fullname == 'pyglet':
            # noinspection PyProtectedMember
            monkey_patch_pyglet(MockTurtle._screen.cv)

    def load_module(self, fullname):
        if self.main_loader is not None:
            module = self.main_loader.load_module(fullname)
        else:
            module = import_module(fullname)
            PatchedModuleFinder.is_desperate = False
        self.exec_module(module)
        return module

    def mock_show(self, *_args, **_kwargs):
        figure = self.plt.gcf()
        # noinspection PyProtectedMember
        turtle_screen = MockTurtle._screen
        screen_width = turtle_screen.cv.cget('width')
        screen_height = turtle_screen.cv.cget('height')
        figure_width = figure.get_figwidth()*figure.dpi
        figure_height = figure.get_figheight()*figure.dpi
        if figure_width < screen_width:
            x = (screen_width - figure_width) // 2
        else:
            x = 0
        if figure_height < screen_height:
            y = (screen_height - figure_height) // 2
        else:
            y = 0
        # noinspection PyUnresolvedReferences
        data = io.BytesIO()
        self.plt.savefig(data, format='PNG')

        image = data.getvalue()
        encoded = standard_b64encode(image)
        image_text = str(encoded.decode('UTF-8'))
        MockTurtle.display_image(x, y, image=image_text)

    def live_coding_zoom(self):
        screen_width, screen_height = self.plt.live_coding_size
        fig = self.plt.gcf()
        fig_width, fig_height = fig.get_figwidth(), fig.get_figheight()
        x_dpi = screen_width/fig_width
        y_dpi = screen_height/fig_height
        fig.dpi = min(x_dpi, y_dpi)
