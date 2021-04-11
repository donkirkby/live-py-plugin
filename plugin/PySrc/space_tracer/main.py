import argparse
from contextlib import contextmanager
from functools import wraps
from inspect import currentframe, stack
import io
from io import StringIO
from pathlib import Path

from itertools import zip_longest as izip_longest
import os
import os.path
import re
import sys
import traceback
import types

try:
    # noinspection PyUnresolvedReferences
    from js import document, window
    IS_PYODIDE = True
    MockTurtle = get_terminal_size = None
except ImportError:
    IS_PYODIDE = False
    document = window = None
    from os import get_terminal_size
    try:
        from .mock_turtle import MockTurtle
    except ImportError:
        MockTurtle = None

from .canvas import Canvas
from .code_tracer import CONTEXT_NAME, find_line_numbers
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
        terminal_width = 0
        try:
            if get_terminal_size is not None:
                terminal_width, _ = get_terminal_size()
        except OSError:
            pass
    # noinspection PyTypeChecker
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
    parser.add_argument('--trace_offset',
                        type=int,
                        default=0,
                        help='Number of columns to skip at start of tracing '
                             'display.')
    parser.add_argument('--trace_width',
                        type=int,
                        default=terminal_width,
                        help='Number of columns to display, including source '
                             'code. Negative to trim columns from the end, 0 '
                             'for no limit.')
    parser.add_argument('-b',
                        '--bad_driver',
                        help="message to display if driver doesn't call module")
    parser.add_argument('-i',
                        '--stdin',
                        default=os.devnull,
                        help="file to read stdin from, or - for normal stdin")
    parser.add_argument('-o',
                        '--stdout',
                        default=os.devnull,
                        help="file to write stdout to (not tracing), "
                             "or - for normal stdout")
    parser.add_argument('-e',
                        '--stderr',
                        default=os.devnull,
                        help="file to write stderr to, or ! for normal stderr")
    parser.add_argument('-r',
                        '--report',
                        default='-',
                        help="file to write tracing to, or - for stdout, "
                             "or ! for stderr.")
    parser.add_argument('--traced_file',
                        help='file to replace with source code from stdin')
    parser.add_argument('--traced',
                        help='module, function, or method to display trace '
                             'for. Default: %%(default)s to trace %s, %s,'
                             'or whatever is replaced by --traced_file.' %
                             (DEFAULT_MODULE_NAME, LIVE_MODULE_NAME))
    parser.add_argument('--hide',
                        help='variable names to hide',
                        nargs='*')
    parser.add_argument('--start_line',
                        type=int,
                        help='first line number to trace')
    parser.add_argument('--end_line',
                        type=int,
                        help='last line number to trace')
    parser.add_argument('--line_numbers',
                        '-l',
                        action='store_true',
                        help='include line numbers with source code')
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
    if args.driver:
        if args.driver[0] == '-m':
            args.is_module = True
            args.driver = args.driver[1:]
    else:
        if args.traced_file:
            args.driver = [args.traced_file]
        else:
            parser.error('one of the following arguments are required: '
                         'driver or traced_file')
    if args.traced is None:
        if args.traced_file is None or args.traced_file == args.driver[0]:
            args.traced = LIVE_MODULE_NAME if args.live else DEFAULT_MODULE_NAME
        else:
            # Wait until the file is imported to see what module got traced.
            pass
    return args


def main():
    tracer = TraceRunner()
    code_report = tracer.trace_command()
    if code_report:
        print(code_report)
    if tracer.return_code:
        try:
            return_code = int(tracer.return_code)
        except ValueError:
            return_code = 1
        exit(return_code)


def analyze(source_code):
    """ Trace the source code for display in the browser.

    :param source_code: Source code to trace.
    :return: (tracing_report, output)
    """
    tracer = TraceRunner()
    tracer.standard_files.old_files['stderr'] = StringIO()
    tracer.max_width = 200000
    with replace_input(source_code):
        code_report = tracer.trace_command(['space_tracer',
                                            '--traced_file', PSEUDO_FILENAME,
                                            '--source_width', '0',
                                            '--live',
                                            '--stdout', '!',
                                            '--stderr', '!',
                                            PSEUDO_FILENAME])
    stdout = tracer.standard_files.old_files['stderr'].getvalue()
    return code_report, stdout


def web_main():
    window.analyze = analyze


class StandardFiles(dict):
    def __init__(self):
        super().__init__()
        self.old_files = dict(stdin=sys.stdin,
                              stdout=sys.stdout,
                              stderr=sys.stderr,
                              report=StringIO())
        self.new_files = {}  # {filename: file}

    def __setitem__(self, key, filename):
        if filename == '-':
            if key == 'stdin':
                file = self.old_files[key]
            else:
                file = self.old_files['stdout']
        elif filename == '!':
            file = self.old_files['stderr']
        else:
            if key == 'stdin':
                mode = 'r'
            else:
                mode = 'w'
            file_key = (filename, mode)
            file = self.new_files.get(file_key)
            if file is None:
                file = argparse.FileType(mode)(filename)
                self.new_files[file_key] = file
        super().__setitem__(key, file)

    def __missing__(self, key):
        return self.old_files[key]

    def close_all(self):
        for file in self.new_files.values():
            file.close()


def traced(target=None, hide=None):
    """ A decorator for a function or with block that should be traced. """
    def is_in_traced_module():
        """ Check if this was called directly by the traced module. """
        call_stack = stack()
        # expected frames in call stack:
        # 0. This function.
        # 1. traced() decorator
        # 2. Module that's being traced
        # 3. module_importers.py that executed the traced module.
        this_filepath = Path(__file__)
        module_importers_filepath = this_filepath.parent / "module_importers.py"
        expected_frame = call_stack[3]
        return expected_frame.filename == str(module_importers_filepath)

    if is_in_traced_module():
        ReportBuilder.is_using_traced_blocks = True

    @contextmanager
    def traced_options():
        ReportBuilder.is_tracing_next_block = True
        old_hide = ReportBuilder.hide
        ReportBuilder.hide = hide
        yield
        ReportBuilder.hide = old_hide
        ReportBuilder.is_tracing_next_block = False

    if target is None:
        # Not decorating a function, must be a with block.
        return traced_options()

    @wraps(target)
    def wrapped(*args, **kwargs):
        with traced_options():
            return target(*args, **kwargs)

    return wrapped


@contextmanager
def swallow_output(standard_files: StandardFiles):
    old_main_mod = sys.modules.get(DEFAULT_MODULE_NAME, None)
    # noinspection PyUnresolvedReferences
    old_string_io = io.StringIO
    try:
        sys.stdout = FileSwallower(standard_files['stdout'])
        sys.stderr = FileSwallower(standard_files['stderr'], target_name='sys.stderr')
        sys.stdin = standard_files['stdin']
        io.StringIO = TracedStringIO
        yield
    finally:
        if old_main_mod is not None:
            sys.modules[DEFAULT_MODULE_NAME] = old_main_mod
        else:
            sys.modules.pop(DEFAULT_MODULE_NAME, None)
        sys.stdout = standard_files.old_files['stdout']
        sys.stderr = standard_files.old_files['stderr']
        sys.stdin = standard_files.old_files['stdin']
        io.StringIO = old_string_io


@contextmanager
def replace_input(stdin_text=None):
    old_stdin = sys.stdin
    sys.stdin = StringIO(stdin_text)
    try:
        yield
    finally:
        sys.stdin = old_stdin


def display_error_on_canvas():
    tb = traceback.TracebackException(*sys.exc_info())
    tb_stack = tb.stack
    library_path = os.path.dirname(__file__)
    while tb_stack and tb_stack[0].filename.startswith(library_path):
        tb_stack.pop(0)
    del tb_stack[10:]
    message = ''.join(tb.format(chain=False))
    message_lines = message.splitlines(keepends=False)
    split_lines = []
    for line in message_lines:
        while line:
            split_lines.append(line[:80])
            line = line[80:]
    max_length = max(len(line) for line in split_lines)
    t = MockTurtle()
    t.up()
    screen = t.getscreen()
    window_width = screen.window_width()
    window_height = screen.window_height()
    line_height = min(window_height / len(split_lines),
                      window_width * 2 / max_length)
    font_size = round(line_height * 0.75)
    font = ('Arial', font_size, 'normal')
    t.goto(-window_width // 2, window_height // 2)
    t.setheading(-90)
    t.fillcolor('white')
    t.begin_fill()
    for _ in range(2):
        t.forward(line_height * len(split_lines))
        t.left(90)
        t.forward(window_width)
        t.left(90)
    t.end_fill()
    for line in split_lines:
        t.forward(line_height)
        t.write(line, font=font)


class TraceRunner(object):
    def __init__(self):
        self.canvas = None
        self.message_limit = 10000
        self.max_width = 200000
        self.keepalive = False
        self.return_code = None
        self.standard_files = StandardFiles()

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
        self.standard_files['stdin'] = args.stdin
        self.standard_files['stdout'] = args.stdout
        self.standard_files['stderr'] = args.stderr
        self.standard_files['report'] = args.report

        ReportBuilder.hide = args.hide
        ReportBuilder.is_using_traced_blocks = False
        builder = ReportBuilder(self.message_limit)
        builder.max_width = self.max_width
        if args.start_line or args.end_line:
            builder.trace_block(args.start_line, args.end_line)

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
            traced_target = traced_importer.traced
            for name, module in list(sys.modules.items()):
                if name == DEFAULT_MODULE_NAME:
                    continue
                module_file = getattr(module, '__file__', '')
                if (traced_target and traced_target.startswith(name) or
                        name == LIVE_MODULE_NAME or
                        module_file == traced_importer.traced_file):
                    del sys.modules[name]
            try:
                self.run_code(args.bad_driver, traced_importer)
            finally:
                # Restore the old argv and path
                sys.argv = old_argv
                sys.meta_path.remove(traced_importer)
                sys.meta_path.remove(patched_finder)
        except SyntaxError as ex:
            self.return_code = 1
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
            if args.canvas:
                display_error_on_canvas()
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
            if args.canvas:
                display_error_on_canvas()

        used_finder = (traced_importer.source_finder or
                       traced_importer.driver_finder)
        is_traced = (traced_importer.is_traced_module_imported or
                     (used_finder and used_finder.is_tracing))
        source_code = (is_traced and
                       used_finder and
                       used_finder.source_code) or ''
        source_lines = source_code.splitlines()
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
            if args.live:
                source_blocks = [(1, len(source_lines))]
            else:
                source_blocks = builder.reported_blocks
            last_line = max(last for first, last in source_blocks)
            number_width = len(str(last_line))
            for first_line, last_line in source_blocks:
                if first_line is None:
                    first_line = 1
                if last_line is None:
                    last_line = len(source_lines)
                for line_number in range(first_line, last_line+1):
                    if line_number > len(source_lines):
                        reported_source_lines.append('')
                    else:
                        line = source_lines[line_number - 1][start_char:]
                        if args.line_numbers:
                            line = '{:{}}) {}'.format(line_number,
                                                      number_width,
                                                      line)
                        reported_source_lines.append(line)
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
                    line += report_line[args.trace_offset:]
                if trace_width:
                    line = line[:trace_width]
                dump_lines.append(line)
            report = '\n'.join(dump_lines)
        if MockTurtle is None:
            turtle_report = None
        else:
            turtle_report = MockTurtle.get_all_reports()
        if turtle_report and args.canvas:
            report = ('start_canvas\n' +
                      '\n'.join(turtle_report) +
                      '\nend_canvas\n.\n' +
                      report)
        if args.report != '-':
            self.standard_files['report'].write(report)
            report = ''
        self.standard_files.close_all()
        return report

    def run_code(self,
                 bad_driver,
                 traced_importer):
        """ Run the traced module, plus its driver.

        :param str bad_driver: a message to display if the driver doesn't call
        the module
        :param traced_importer: holds details of what to trace
        __main__.
        """
        for module_name in ('random', 'numpy.random'):
            random_module = sys.modules.get(module_name)
            if random_module is not None:
                # noinspection PyUnresolvedReferences
                random_module.seed(0)

        builder = traced_importer.report_builder
        output_context = swallow_output(self.standard_files)
        try:
            with output_context:
                try:
                    traced_importer.run_main()
                    # noinspection PyUnresolvedReferences
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
                for value in traced_importer.environment.values():
                    if isinstance(value, types.GeneratorType):
                        value.close()
            if not traced_importer.is_traced_module_imported:
                traced_target = traced_importer.traced
                driver_name = os.path.basename(traced_importer.driver[0])
                if bad_driver:
                    message = bad_driver
                elif traced_target is None:
                    traced_name = os.path.basename(traced_importer.traced_file)
                    message = ("{} doesn't call {}. Try a different "
                               "driver.").format(driver_name, traced_name)
                else:
                    message = ("{} doesn't call the {} module. Try a different "
                               "driver.").format(driver_name, traced_target)
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

    def write(self, *args, **kwargs):
        self.target.write(*args, **kwargs)
        text = args and str(args[0]) or ''
        if re.search(r'^=+\s*FAILURES\s*=+$', text):
            self.saw_failures = True
        frame = currentframe()
        while frame is not None:
            report_builder = frame.f_locals.get(CONTEXT_NAME)
            if report_builder is not None:
                has_print_function = True
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
