import __future__
import argparse
import io
import os
import os.path
import re
import sys
import traceback
import types
from contextlib import contextmanager
from inspect import currentframe
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

try:
    # noinspection PyUnresolvedReferences
    from js import document, window
    IS_PYODIDE = True
except ImportError:
    IS_PYODIDE = False
    document = window = None

try:
    from importlib.abc import MetaPathFinder, Loader
    from importlib.machinery import ModuleSpec
    from importlib.util import find_spec
except ImportError:
    # Stub out the classes for older versions of Python.
    class MetaPathFinder(object):
        pass

    Loader = ModuleSpec = object
    find_spec = None

try:
    from itertools import izip_longest
except ImportError:
    from itertools import zip_longest as izip_longest

from space_tracer.canvas import Canvas
from space_tracer.code_tracer import CONTEXT_NAME, find_line_numbers
from space_tracer.mock_turtle import MockTurtle
from space_tracer.module_importers import imp, TracedModuleImporter, \
    PatchedModuleFinder
from space_tracer.module_runner import ModuleRunner
from space_tracer.report_builder import ReportBuilder
from space_tracer.traced_finder import DEFAULT_MODULE_NAME, LIVE_MODULE_NAME, \
    PSEUDO_FILENAME


def parse_args(command_args=None):
    if command_args is None:
        command_args = sys.argv
    launcher = command_args[0]
    if launcher.endswith("__main__.py"):
        executable = os.path.basename(sys.executable)
        launcher = executable + " -m " + __package__
    parser = argparse.ArgumentParser(
        launcher,
        description='Trace Python code.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-c',
                        '--canvas',
                        action='store_true',
                        help='Should canvas commands be printed?')
    parser.add_argument('-x',
                        '--width',
                        type=int,
                        default=800,
                        help='width of the canvas in pixels')
    parser.add_argument('-y',
                        '--height',
                        type=int,
                        default=600,
                        help='height of the canvas in pixels')
    parser.add_argument('-z',
                        '--zoomed',
                        action='store_true',
                        help='matplotlib is zoomed to fit the canvas size')
    parser.add_argument('--source_width',
                        type=int,
                        help='Width of source code - use 0 to hide or '
                             'negative numbers to trim columns from the end, '
                             'None to fit source code.')
    parser.add_argument('-n',
                        '--source_indent',
                        type=int,
                        default=0,
                        help='Number of spaces to indent source code. '
                             'Negative to skip first columns of source code.')
    parser.add_argument('-b',
                        '--bad_driver',
                        help="message to display if driver doesn't call module")
    parser.add_argument('-i',
                        '--stdin',
                        help="file to redirect stdin from")
    parser.add_argument('--traced_file',
                        help='file to replace with source code from stdin')
    parser.add_argument('--traced',
                        help='module, function, or method to display trace '
                             'for. Default: %%(default)s to trace %s, %s,'
                             'or whatever is replaced by --traced_file.' %
                             (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME))
    parser.add_argument('--live',
                        action='store_true',
                        help='load main module as %s instead of %s.' %
                             (LIVE_MODULE_NAME, DEFAULT_MODULE_NAME))
    parser.add_argument('-m',
                        dest='is_module',
                        action='store_true',
                        help='driver is an importable module, not a script')
    parser.add_argument('driver',
                        nargs=argparse.REMAINDER,
                        help='script to call traced code, plus any arguments')
    return parser.parse_args(command_args[1:])


def main():
    tracer = TraceRunner()
    code_report = tracer.trace_command()
    print(code_report)
    if tracer.return_code:
        exit(tracer.return_code)


def analyze(source_code):
    tracer = TraceRunner()
    tracer.max_width = 200000
    code_report = tracer.trace_code(source_code)
    return code_report


def web_main():
    window.analyze = analyze


@contextmanager
def swallow_output(stdin_path=None):
    old_main_mod = sys.modules.get(DEFAULT_MODULE_NAME, None)
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    old_stdin = sys.stdin
    # noinspection PyUnresolvedReferences
    old_string_io = io.StringIO
    try:
        sys.stdout = FileSwallower(old_stdout)
        sys.stderr = FileSwallower(old_stderr, target_name='sys.stderr')
        sys.stdin = stdin_path and open(stdin_path) or io.StringIO()
        with sys.stdin:
            io.StringIO = TracedStringIO
            yield
    finally:
        if old_main_mod is not None:
            sys.modules[DEFAULT_MODULE_NAME] = old_main_mod
        else:
            sys.modules.pop(DEFAULT_MODULE_NAME, None)
        sys.stdout = old_stdout
        sys.stderr = old_stderr
        sys.stdin = old_stdin
        io.StringIO = old_string_io


@contextmanager
def replace_input(stdin_text=None):
    old_stdin = sys.stdin
    sys.stdin = StringIO(stdin_text)
    try:
        yield
    finally:
        sys.stdin = old_stdin


class TraceRunner(object):
    def __init__(self):
        self.canvas = None
        self.message_limit = 10000
        self.max_width = 200000
        self.keepalive = False
        self.environment = {}
        self.return_code = None

    @staticmethod
    def run_python_module(modulename, module_importer):
        """Run a python module, as though with ``python -m name args...``.

        `modulename` is the name of the module, possibly a dot-separated name.

        This is based on code from coverage.py, by Ned Batchelder.
        https://bitbucket.org/ned/coveragepy
        """
        if find_spec:
            spec = find_spec(modulename)
            if spec is not None:
                pathname = spec.origin
                packagename = spec.name
            elif (module_importer.traced in (DEFAULT_MODULE_NAME,
                                             LIVE_MODULE_NAME) and
                  module_importer.source_code):
                pathname = module_importer.filename
                packagename = module_importer.driver_module
            else:
                raise ImportError(modulename)
            if pathname.endswith("__init__.py") and not modulename.endswith("__init__"):
                mod_main = modulename + ".__main__"
                spec = find_spec(mod_main)
                if not spec:
                    raise ImportError(
                        "No module named %s; "
                        "%r is a package and cannot be directly executed"
                        % (mod_main, modulename))
                pathname = spec.origin
                packagename = spec.name
            packagename = packagename.rpartition(".")[0]
        else:
            openfile = None
            glo, loc = globals(), locals()
            try:
                # Search for the module - inside its parent package, if any -
                # using standard import mechanics.
                try:
                    if '.' in modulename:
                        packagename, name = modulename.rsplit('.', 1)
                        package = __import__(packagename, glo, loc, ['__path__'])
                        searchpath = package.__path__
                    else:
                        packagename, name = None, modulename
                        searchpath = None  # "top-level search" in imp.find_module()
                    # noinspection PyDeprecation
                    openfile, pathname, _ = imp.find_module(name, searchpath)

                    # If `modulename` is actually a package, not a mere module,
                    # then we pretend to be Python 2.7 and try running its
                    # __main__.py script.
                    if openfile is None:
                        packagename = modulename
                        name = '__main__'
                        package = __import__(packagename, glo, loc, ['__path__'])
                        searchpath = package.__path__
                        # noinspection PyDeprecation
                        openfile, pathname, _ = imp.find_module(name, searchpath)
                except ImportError:
                    if (module_importer.traced in (DEFAULT_MODULE_NAME,
                                                   LIVE_MODULE_NAME) and
                            module_importer.source_code):
                        pathname = module_importer.filename
                        packagename = module_importer.driver_module
                        packagename = packagename.rpartition(".")[0]
                    else:
                        raise
            finally:
                if openfile:
                    openfile.close()

        # Finally, hand the file off to run_python_file for execution.
        pathname = os.path.abspath(pathname)
        module_runner = module_importer.module_runner
        module_runner.run_python_file(
            pathname,
            package=packagename,
            traced=module_importer and module_importer.traced,
            module_importer=module_importer)

    @staticmethod
    def split_lines(messages):
        for message in messages:
            for line in message.splitlines():
                yield line

    def trace_turtle(self, source):
        with replace_input(source):
            self.trace_command(['space_tracer',
                                '--traced_file', PSEUDO_FILENAME,
                                '--source_width', '0',
                                '--width', '0',
                                '--height', '0',
                                '--live',
                                PSEUDO_FILENAME])

        return '\n'.join(MockTurtle.get_all_reports())

    def report_driver_result(self, builder, messages):
        messages = list(self.split_lines(messages))
        block_size = len(messages) + 2
        builder.start_block(1, block_size)
        message_width = 1
        for lineno, message in enumerate(messages, 2):
            message_width = max(len(message), message_width)
            builder.add_message(message, lineno)

        header = '-' * message_width + ' '
        builder.add_message(header, 1)
        builder.add_message(header, block_size)
        builder.start_block(1, block_size)

    def trace_code(self, source):
        """ Trace a module of source code.

        :param str source: source code to trace and run.
        """
        with replace_input(source):
            return self.trace_command(['space_tracer',
                                       '--traced_file', PSEUDO_FILENAME,
                                       '--source_width', '0',
                                       '--live',
                                       PSEUDO_FILENAME])

    def trace_command(self, command_args=None):
        """ Trace a module, based on arguments from the command line.
        :param command_args: list of strings, like sys.argv
        :return: the tracing report, but not the canvas report
        """
        args = parse_args(command_args)
        code = None
        if args.traced_file is not None:
            code = sys.stdin.read()
        if args.traced is None:
            args.traced = LIVE_MODULE_NAME if args.live else DEFAULT_MODULE_NAME
        if self.canvas is None:
            self.canvas = Canvas(args.width, args.height)
        if MockTurtle is not None:
            MockTurtle.monkey_patch(self.canvas)

        builder = ReportBuilder(self.message_limit)
        builder.max_width = self.max_width

        module_runner = ModuleRunner(builder, self.environment)
        traced_importer = TracedModuleImporter(
            code,
            args.traced,
            self.environment,
            args.traced_file,
            args.driver[0] if args.is_module and args.driver else None,
            module_runner)

        module_runner = traced_importer.module_runner
        builder = module_runner.report_builder
        patched_finder = PatchedModuleFinder(args.zoomed)
        self.return_code = 0

        try:
            # Set sys.argv properly.
            old_argv = sys.argv
            sys.argv = args.driver or [args.traced_file or args.traced]

            sys.meta_path.insert(0, patched_finder)
            sys.meta_path.insert(0, traced_importer)
            try:
                self.run_code(code,
                              builder,
                              args.traced,
                              args.is_module,
                              args.driver,
                              args.traced_file,
                              args.bad_driver,
                              args.stdin,
                              traced_importer)
            finally:
                # Restore the old argv and path
                sys.argv = old_argv

                # During testing, we import these modules for every test case,
                # so force a reload. This is only likely to happen during testing.
                to_delete = []
                for name in sys.modules:
                    if args.traced.startswith(name) or name == LIVE_MODULE_NAME:
                        to_delete.append(name)
                for target in to_delete:
                    del sys.modules[target]
                sys.meta_path.remove(traced_importer)
                sys.meta_path.remove(patched_finder)

            for value in self.environment.values():
                if isinstance(value, types.GeneratorType):
                    value.close()
        except SyntaxError:
            self.return_code = 1
            ex = sys.exc_info()[1]
            messages = traceback.format_exception_only(type(ex), ex)
            message = messages[-1].strip()
            if ex.filename == PSEUDO_FILENAME:
                line_number = ex.lineno
            else:
                line_number = 1
                message = '{} line {}: {}'.format(ex.filename,
                                                  ex.lineno,
                                                  message)
            builder.add_message(message, line_number)
        except BaseException as ex:
            self.return_code = getattr(ex, 'code', 1)
            etype, value, tb = sys.exc_info()
            is_reported = False
            entries = traceback.extract_tb(tb)
            for traced_file, _, _, _ in entries:
                if traced_file == PSEUDO_FILENAME:
                    is_reported = True
            space_tracer_folder = os.path.dirname(__file__)
            while not is_reported and tb is not None:
                frame = tb.tb_frame
                code = frame.f_code
                traced_file = code.co_filename
                traced_folder = os.path.dirname(traced_file)
                if traced_folder != space_tracer_folder:
                    break
                tb = tb.tb_next
            if not is_reported:
                if tb:
                    messages = traceback.format_exception(etype, value, tb)
                else:
                    messages = traceback.format_exception_only(etype, value)
                self.report_driver_result(builder, messages)

        report = builder.report()
        source_width = args.source_width
        if source_width != 0:
            if args.source_indent >= 0:
                indent = args.source_indent
                start_char = 0
            else:
                indent = 0
                start_char = -args.source_indent
            used_finder = traced_importer.source_finder or traced_importer.driver_finder
            source_lines = used_finder.source_code.splitlines()
            reported_source_lines = []
            for first_line, last_line in builder.reported_blocks:
                for line_number in range(first_line, last_line+1):
                    reported_source_lines.append(source_lines[line_number-1][start_char:])
            report_lines = report.splitlines()
            dump_lines = []
            max_width = max(map(len, reported_source_lines))
            if source_width is None:
                source_width = max_width + indent
            elif source_width < 0:
                source_width += max_width + indent
            for source_line, report_line in izip_longest(reported_source_lines,
                                                         report_lines,
                                                         fillvalue=''):
                padded_source_line = indent * ' ' + source_line
                padded_source_line += (source_width - len(source_line)) * ' '
                line = padded_source_line[:source_width] + ' |'
                if report_line:
                    line += ' ' + report_line
                dump_lines.append(line)
            report = '\n'.join(dump_lines)
        turtle_report = MockTurtle.get_all_reports()
        if turtle_report and args.canvas:
            report = ('start_canvas\n' +
                      '\n'.join(turtle_report) +
                      '\nend_canvas\n.\n' +
                      report)
        return report

    def run_code(self,
                 code,
                 builder,
                 traced,
                 is_module,
                 driver,
                 traced_file,
                 bad_driver,
                 stdin_path=None,
                 traced_importer=None):
        """ Run the traced module, plus its driver.

        :param code: the source code for the traced module, or None to load
        from the normal file
        :param builder: the report builder
        :param str traced: the module, method, or function name to trace
        :param bool is_module: True if the driver is a module name instead of a
        file name
        :param list driver: the driver script's file name or module name and args
        :param str traced_file: the file name of the source code
        :param str bad_driver: a message to display if the driver doesn't call
        the module
        :param str stdin_path: Path to redirect stdin from
        :param traced_importer: holds details of what to trace
        __main__.
        """
        module_runner = traced_importer.module_runner
        self.environment[CONTEXT_NAME] = builder
        for module_name in ('random', 'numpy.random'):
            random_module = sys.modules.get(module_name)
            if random_module is not None:
                random_module.seed(0)

        output_context = swallow_output(stdin_path)
        try:
            with output_context:
                try:
                    if not is_module:
                        module_runner.run_python_file(
                            driver and driver[0],
                            traced=traced,
                            source_code=(code
                                         if not driver or traced_file == driver[0]
                                         else None),
                            module_importer=traced_importer)
                    else:
                        module_name = driver[0]
                        self.run_python_module(module_name, traced_importer)
                    if sys.stdout.saw_failures:
                        self.report_driver_result(builder, ['Pytest reported failures.'])
                        self.return_code = 1
                except SystemExit as ex:
                    if ex.code:
                        self.return_code = ex.code
                        messages = traceback.format_exception_only(type(ex),
                                                                   ex)
                        message = messages[-1].strip()
                        self.report_driver_result(builder, [message])
            if traced not in sys.modules and traced not in (DEFAULT_MODULE_NAME,
                                                            LIVE_MODULE_NAME):
                driver_name = os.path.basename(driver[0])
                message = (bad_driver or "{} doesn't call the {} module."
                                         " Try a different driver.".format(driver_name,
                                                                           traced))
                self.report_driver_result(builder, [message])
        finally:
            self.environment = traced_importer.environment
            is_decorated = any(frame.is_decorated for frame in builder.history)
            used_finder = traced_importer.source_finder or traced_importer.driver_finder
            if used_finder and not is_decorated:
                line_numbers = set()
                if used_finder.traced_node:
                    find_line_numbers(used_finder.traced_node, line_numbers)
                    is_minimum = False
                else:
                    find_line_numbers(used_finder.source_tree, line_numbers)
                    is_minimum = True
                if line_numbers:
                    builder.trace_block(min(line_numbers),
                                        max(line_numbers),
                                        is_minimum)


class FileSwallower(object):
    def __init__(self,
                 target,
                 check_buffer=True,
                 target_name=None):
        self.target = target
        self.target_name = target_name
        self.saw_failures = False
        if check_buffer:
            buffer = getattr(target, 'buffer', None)
            if buffer is not None:
                self.buffer = FileSwallower(buffer, check_buffer=False)

    def write(self, *args, **_):
        text = args and str(args[0]) or ''
        if re.search(r'^=+\s*FAILURES\s*=+$', text):
            self.saw_failures = True
        frame = currentframe()
        while frame is not None:
            report_builder = frame.f_locals.get(CONTEXT_NAME)
            if report_builder is not None:
                has_print_function = (
                    sys.version_info >= (3, 0) or
                    __future__.print_function in frame.f_globals.values())
                report_builder.add_output(text,
                                          frame.f_lineno,
                                          has_print_function,
                                          target_name=self.target_name)
                break
            frame = frame.f_back

    def __getattr__(self, name):
        return getattr(self.target, name)


def find_string_io_targets(frame):
    for name, value in frame.f_locals.items():
        yield name, value
        if name == 'self':
            for attr_name, attr_value in value.__dict__.items():
                yield 'self.' + attr_name, attr_value


class TracedStringIO(io.StringIO):
    def write(self, text):
        super(TracedStringIO, self).write(text)
        frame = currentframe()
        while frame is not None:
            report_builder = frame.f_locals.get(CONTEXT_NAME)
            if report_builder is not None:
                for name, value in find_string_io_targets(frame):
                    if value is self:
                        report_builder.add_output(text,
                                                  frame.f_lineno,
                                                  target_name=name)
                        return
            frame = frame.f_back


if __name__ == '__main__':
    main()
