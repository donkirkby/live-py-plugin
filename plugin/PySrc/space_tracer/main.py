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
    from os import get_terminal_size
except ImportError:
    # Stub out the classes for older versions of Python.
    class MetaPathFinder(object):
        pass

    Loader = ModuleSpec = object
    find_spec = get_terminal_size = None

try:
    from itertools import izip_longest
except ImportError:
    from itertools import zip_longest as izip_longest

from .canvas import Canvas
from .code_tracer import CONTEXT_NAME, find_line_numbers
from .mock_turtle import MockTurtle
from .module_importers import TracedModuleImporter, PatchedModuleFinder
from .report_builder import ReportBuilder
from .traced_finder import DEFAULT_MODULE_NAME, LIVE_MODULE_NAME, \
    PSEUDO_FILENAME


def parse_args(command_args=None):
    if command_args is None:
        command_args = sys.argv
    launcher = command_args[0]
    if launcher.endswith("__main__.py"):
        executable = os.path.basename(sys.executable)
        launcher = executable + " -m " + __package__
    if get_terminal_size is None:
        terminal_width = 0
    else:
        try:
            terminal_width, _ = get_terminal_size()
        except OSError:
            terminal_width = 0
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
    parser.add_argument('--trace_width',
                        type=int,
                        default=terminal_width,
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
                        nargs='*',
                        help='script to call traced code, plus any arguments. '
                             'Default: %(default)s to use --traced_file.')
    args = parser.parse_args(command_args[1:])
    if not args.driver:
        if args.traced_file:
            args.driver = [args.traced_file]
        else:
            parser.error('one of the following arguments are required: '
                         'driver or traced_file')
    if args.traced is None:
        if args.traced_file is None or args.traced_file == args.driver[0]:
            args.traced = LIVE_MODULE_NAME if args.live else DEFAULT_MODULE_NAME
        elif sys.version_info < (3, 0):
            parser.error('the following argument is required with traced_file '
                         'in Python 2: traced')
        else:
            # Wait until the file is imported to see what module got traced.
            pass
    return args


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
        self.return_code = None

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
        :return: the tracing report, including the canvas report
        """
        args = parse_args(command_args)
        if self.canvas is None:
            self.canvas = Canvas(args.width, args.height)
        if MockTurtle is not None:
            MockTurtle.monkey_patch(self.canvas)

        builder = ReportBuilder(self.message_limit)
        builder.max_width = self.max_width

        traced_importer = TracedModuleImporter(
            args.traced,
            args.traced_file,
            args.driver,
            args.is_module,
            args.live,
            builder)

        patched_finder = PatchedModuleFinder(args.zoomed)
        self.return_code = 0

        try:
            # Set sys.argv properly.
            old_argv = sys.argv
            sys.argv = args.driver

            sys.meta_path.insert(0, patched_finder)
            sys.meta_path.insert(0, traced_importer)

            # During testing, we import these modules for every test case,
            # so force a reload. This is only likely to happen during testing.
            traced = traced_importer.traced
            for name, module in list(sys.modules.items()):
                if name == DEFAULT_MODULE_NAME:
                    continue
                module_file = getattr(module, '__file__', '')
                if (traced and traced.startswith(name) or
                        name == LIVE_MODULE_NAME or
                        module_file == traced_importer.traced_file):
                    del sys.modules[name]
            try:
                self.run_code(args.bad_driver, args.stdin, traced_importer)
            finally:
                # Restore the old argv and path
                sys.argv = old_argv
                sys.meta_path.remove(traced_importer)
                sys.meta_path.remove(patched_finder)

            for value in traced_importer.environment.values():
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
                traced_file = tb.tb_frame.f_code.co_filename
                traced_folder = os.path.dirname(traced_file)
                if traced_folder != space_tracer_folder:
                    break
                tb = tb.tb_next
            if not is_reported:
                if tb:
                    messages = traceback.format_exception(etype, value, tb)
                else:
                    messages = traceback.format_exception_only(etype, value)
                traced_importer.report_driver_result(messages)

        used_finder = (traced_importer.source_finder or
                       traced_importer.driver_finder)
        source_code = used_finder and used_finder.source_code
        source_lines = source_code and source_code.splitlines()
        if source_lines and traced_importer.is_live:
            total_lines = len(source_lines)
        else:
            total_lines = 0
        report = builder.report(total_lines)
        source_width = args.source_width
        if source_code is None or source_width == 0:
            reported_source_lines = []
            indent = 0
        else:
            if args.source_indent >= 0:
                indent = args.source_indent
                start_char = 0
            else:
                indent = 0
                start_char = -args.source_indent
            reported_source_lines = []
            for first_line, last_line in builder.reported_blocks:
                for line_number in range(first_line, last_line+1):
                    reported_source_lines.append(
                        source_lines[line_number-1][start_char:])
            max_source_width = max(map(len, reported_source_lines))
            if source_width is None:
                source_width = max_source_width + indent
            elif source_width < 0:
                source_width += max_source_width + indent

        trace_width = args.trace_width
        if trace_width or reported_source_lines:
            report_lines = report.splitlines()
            dump_lines = []
            if trace_width < 0:
                max_report_width = max(len(report_line)
                                       for report_line in report_lines)
                if source_width:
                    trace_width += source_width + 3
                trace_width += max_report_width
            for source_line, report_line in izip_longest(reported_source_lines,
                                                         report_lines,
                                                         fillvalue=''):
                padded_source_line = indent * ' ' + source_line
                padded_source_line += (source_width - len(source_line)) * ' '
                line = padded_source_line[:source_width]
                if line:
                    line += ' |'
                    if report_line:
                        line += ' '
                if report_line:
                    line += report_line
                if trace_width:
                    line = line[:trace_width]
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
                 bad_driver,
                 stdin_path,
                 traced_importer):
        """ Run the traced module, plus its driver.

        :param str bad_driver: a message to display if the driver doesn't call
        the module
        :param str stdin_path: Path to redirect stdin from
        :param traced_importer: holds details of what to trace
        __main__.
        """
        for module_name in ('random', 'numpy.random'):
            random_module = sys.modules.get(module_name)
            if random_module is not None:
                random_module.seed(0)

        builder = traced_importer.report_builder
        output_context = swallow_output(stdin_path)
        try:
            with output_context:
                try:
                    traced_importer.run_main()
                    if sys.stdout.saw_failures:
                        traced_importer.report_driver_result(
                            ['Pytest reported failures.'])
                        self.return_code = 1
                except SystemExit as ex:
                    if ex.code:
                        self.return_code = ex.code
                        messages = traceback.format_exception_only(type(ex),
                                                                   ex)
                        message = messages[-1].strip()
                        traced_importer.report_driver_result([message])
            traced = traced_importer.traced
            if ((not traced_importer.is_traced_module_imported) and
                    traced not in (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME)):
                driver_name = os.path.basename(traced_importer.driver[0])
                if bad_driver:
                    message = bad_driver
                elif traced is None:
                    traced_name = os.path.basename(traced_importer.traced_file)
                    message = ("{} doesn't call {}. Try a different "
                               "driver.").format(driver_name, traced_name)
                else:
                    message = ("{} doesn't call the {} module. Try a different "
                               "driver.").format(driver_name, traced)
                traced_importer.report_driver_result([message])
        finally:
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
