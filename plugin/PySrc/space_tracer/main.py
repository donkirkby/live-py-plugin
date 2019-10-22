import argparse
import os.path
import sys

try:
    # noinspection PyUnresolvedReferences
    from js import document, window
    IS_PYODIDE = True
except ImportError:
    IS_PYODIDE = False
    document = window = None

from .canvas import Canvas
from .mock_turtle import MockTurtle
from .code_tracer import CodeTracer
from .traced_finder import DEFAULT_MODULE_NAME, LIVE_MODULE_NAME


def main():
    launcher = sys.argv[0]
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
                        dest='module',
                        action='store_true',
                        help='driver is an importable module, not a script')
    parser.add_argument('driver',
                        nargs=argparse.REMAINDER,
                        help='script to call traced code, plus any arguments')
    args = parser.parse_args()

    code = None
    if args.traced_file is not None:
        code = sys.stdin.read()
    if args.traced is None:
        args.traced = LIVE_MODULE_NAME if args.live else DEFAULT_MODULE_NAME

    canvas = Canvas(args.width, args.height)
    tracer = CodeTracer(canvas)
    tracer.max_width = 200000
    code_report = tracer.trace_code(code,
                                    source_width=args.source_width,
                                    source_indent=args.source_indent,
                                    traced=args.traced,
                                    is_module=args.module,
                                    driver=args.driver,
                                    traced_file=args.traced_file,
                                    stdin=args.stdin,
                                    bad_driver=args.bad_driver,
                                    is_zoomed=args.zoomed)
    turtle_report = MockTurtle.get_all_reports()
    if turtle_report and args.canvas:
        print('start_canvas')
        print('\n'.join(turtle_report))
        print('end_canvas')
        print('.')
    print(code_report)
    if tracer.return_code:
        exit(tracer.return_code)


def analyze(source_code):
    tracer = CodeTracer()
    tracer.max_width = 200000
    code_report = tracer.trace_code(source_code)
    return code_report


def web_main():
    window.analyze = analyze
