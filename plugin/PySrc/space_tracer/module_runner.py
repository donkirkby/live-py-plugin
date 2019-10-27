import inspect
import os
import types

import sys

from .code_tracer import trace_source_tree
from .module_importers import builtins
from .traced_finder import DEFAULT_MODULE_NAME, LIVE_MODULE_NAME, \
    PSEUDO_FILENAME, TracedFinder


class ModuleRunner(object):
    def __init__(self, report_builder, environment):
        self.report_builder = report_builder
        self.environment = environment

    def run_python_file(self,
                        filename,
                        package=None,
                        traced=None,
                        source_code=None,
                        module_importer=None):
        """Run a python file as if it were the main program on the command line.

        :param str filename: the path to the file to execute.
        :param str package: the package name to set on the module.
        :param str traced: the full path to a module, function, or method to
            trace.
        :param str source_code: custom source code to replace the file contents.
        :param module_importer: where to record the TracedFinder used by the
            driver.
        """
        call_stack_files = [frame[0].f_code.co_filename
                            for frame in inspect.stack()]
        top_file = call_stack_files[-1]
        if os.path.basename(top_file) == 'runpy.py':
            # Exclude runpy.py, used for python -m.
            call_stack_files = [
                frame_filename
                for frame_filename in call_stack_files
                if os.path.basename(frame_filename) != 'runpy.py']
            top_file = os.path.dirname(call_stack_files[-1])
        expected_path0 = os.path.abspath(os.path.dirname(top_file))
        # Check that sys.path is as expected, otherwise leave it alone.
        if os.path.abspath(sys.path[0]) == expected_path0:
            # Set sys.path to target script's folder instead of space_tracer.
            sys.path[0] = os.path.abspath(os.path.dirname(filename))

        # Create a module to serve as __main__
        # noinspection PyUnresolvedReferences
        module_name = (LIVE_MODULE_NAME
                       if traced == LIVE_MODULE_NAME
                       else DEFAULT_MODULE_NAME)
        main_mod = types.ModuleType(module_name)
        sys.modules[module_name] = main_mod
        main_mod.__file__ = filename
        main_mod.__builtins__ = builtins
        if package:
            main_mod.__package__ = package

        code = self.make_code_from_py(filename,
                                      traced,
                                      source_code,
                                      module_importer)

        if module_importer.driver_finder.is_tracing:
            main_mod.__dict__.update(self.environment)
            module_importer.environment = main_mod.__dict__
        # Execute the code object.
        exec(code, main_mod.__dict__)

    @staticmethod
    def make_code_from_py(filename, traced, source, module_importer):
        """Get source from `filename` and make a code object of it."""
        if source is None:
            if (module_importer is not None and
                    module_importer.traced_file is not None and
                    (os.path.abspath(module_importer.traced_file) ==
                     os.path.abspath(filename)) and
                    module_importer.source_code is not None):
                source = module_importer.source_code
                traced = traced or DEFAULT_MODULE_NAME
            else:
                with open(filename, 'rU') as f:
                    source = f.read()

        if traced:
            if traced.startswith(DEFAULT_MODULE_NAME):
                traced = traced[len(DEFAULT_MODULE_NAME)+1:]
            elif traced.startswith(LIVE_MODULE_NAME):
                traced = traced[len(LIVE_MODULE_NAME)+1:]
            parsed_file = PSEUDO_FILENAME if traced == '' else filename
            module_importer.driver_finder = TracedFinder(source,
                                                         traced,
                                                         parsed_file)
            to_compile = module_importer.driver_finder.source_tree
            if (traced == '' or
                    module_importer.driver_finder.traced_node is not None):
                to_compile = trace_source_tree(to_compile)
                module_importer.driver_finder.is_tracing = True
        else:
            module_importer.driver_finder = TracedFinder(source,
                                                         '',
                                                         PSEUDO_FILENAME)
            to_compile = module_importer.driver_finder.source_tree
        code = compile(to_compile, filename or PSEUDO_FILENAME, "exec")

        return code
