import doctest
import os
import re
import sys

from mock import call, DEFAULT, patch

from space_tracer.code_tracer import main
from test_report_builder import ReportTestCase

EXAMPLE_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                   'example_driver.py')
EXAMPLE_SOURCE_PATH = os.path.join(os.path.dirname(__file__),
                                   'example_source.py')
EXAMPLE_PATCHING_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                            'example_patching_driver.py')
EXAMPLE_DRIVER_SYNTAX_ERROR_PATH = os.path.join(os.path.dirname(__file__),
                                                'example_driver_syntax_error.py')
EXAMPLE_PYCHARM_FAILURES_PATH = os.path.join(os.path.dirname(__file__),
                                             'example_pycharm_failures.py')
EXAMPLE_SILENT_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                          'example_silent_driver.py')
patch.multiple = patch.multiple  # Avoids PyCharm warnings.


class CodeTracerMainTest(ReportTestCase):
    def setUp(self):
        super(CodeTracerMainTest, self).setUp()
        self.maxDiff = None
        for module_name in ('example_source',
                            'example_package',
                            'example_package.__main__',
                            'example_package.lib_in_package',
                            'example_driver'):
            if module_name in sys.modules:
                del sys.modules[module_name]

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-'])
    def test_main(self, stdin, stdout):
        code = """\
i = 1
name = __name__
"""
        expected_report = """\
i = 1
name = '__main__' """
        stdin.read.return_value = code

        main()

        self.assertReportEqual(expected_report,
                               stdout.write.call_args_list[0][0][0])

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_indent', '4',
        '--source', '-'])
    def test_dump_arg(self, stdin, stdout):
        code = """\
i = 1 + 1
"""
        expected_report = """\
    i = 1 + 1 | i = 2 """
        stdin.read.return_value = code

        main()

        self.assertEqual([call(expected_report), call('\n')],
                         stdout.write.call_args_list)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_indent', '2',
        '--source', '-'])
    def test_source_indent_small(self, stdin, stdout):
        code = """\
i = 1 + 1
"""
        expected_report = """\
  i = 1 + 1 | i = 2 """
        stdin.read.return_value = code

        main()

        self.assertEqual([call(expected_report), call('\n')],
                         stdout.write.call_args_list)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_indent', '-2',
        '--source', '-'])
    def test_source_indent_negative(self, stdin, stdout):
        code = """\
i = 1 + 1
"""
        expected_report = """\
= 1 + 1 | i = 2 """
        stdin.read.return_value = code

        main()

        self.assertEqual([call(expected_report), call('\n')],
                         stdout.write.call_args_list)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '8',
        '--source', '-'])
    def test_source_width_positive(self, stdin, stdout):
        code = """\
i = 1 + 1
"""
        expected_report = """\
i = 1 +  | i = 2 """
        stdin.read.return_value = code

        main()

        self.assertEqual([call(expected_report), call('\n')],
                         stdout.write.call_args_list)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '-2',
        '--source', '-'])
    def test_source_width_negative(self, stdin, stdout):
        code = """\
i = 1 + 1
"""
        expected_report = """\
i = 1 + | i = 2 """
        stdin.read.return_value = code

        main()

        self.assertEqual([call(expected_report), call('\n')],
                         stdout.write.call_args_list)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_indent', '4',
        '--live',
        EXAMPLE_SOURCE_PATH])
    def test_source_file_arg(self, stdin, stdout):
        expected_report = """\
    def foo(x):                       | x = 3
        return x + 1                  | return 4
                                      |
                                      |
    def bar(bucket):                  |
        bucket.add('bar')             |
                                      |
                                      |
    if __name__ == '__live_coding__': |
        y = foo(3)                    | y = 4
"""
        stdin.read.return_value = ""

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_DRIVER_PATH])
    def test_driver(self, stdin, stdout):
        source = """\
def foo(x):
    name = __name__
    return x + 1
"""
        expected_report = """\
x = 42
name = 'example_source'
return 43
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_DRIVER_PATH,
        '99'])
    def test_driver_args(self, stdin, stdout):
        source = """\
import sys
def foo(x):
    return sys.argv[1:]
"""
        expected_report = """\

x = 42
return ['99']
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', '__live_coding__',
        '--live',
        EXAMPLE_SOURCE_PATH,
        '99'])
    def test_args_no_driver(self, stdin, stdout):
        source = """\
import sys
x = sys.argv[1:]
"""
        expected_report = """\

x = ['99']
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        '-m',
        'example_driver',
        '99'])
    def test_driver_module(self, stdin, stdout):
        source = """\
import sys
def foo(x):
    return sys.argv[1:]
"""
        expected_report = """\

x = 42
return ['99']
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_package.lib_in_package',
        '-m',
        'example_driver'])
    def test_lib_in_package(self, stdin, stdout):
        source = """\
def add_message(s):
    package = __package__
    return s + ' Received'
"""
        expected_report = """\
s = 'from driver'
package = 'example_package'
return 'from driver Received'
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        '-m',
        'example_package.driver_in_package'])
    def test_driver_in_package(self, stdin, stdout):
        source = """\
def foo(x):
    return 42
"""
        expected_report = """\
x = 'from driver in package'
return 42
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_DRIVER_PATH,
        'fail',
        'badly'])
    def test_driver_fails(self, stdin, stdout):
        source = """\
foo = 'Hello, World!'
"""
        expected_report = """\
foo = 'Hello, World!' | ---------------------------------------------------- |
                      | Traceback (most recent call last):                   |
                      |   File "path/example_driver.py", line 6, in <module> |
                      |     assert 'fail' not in sys.argv, sys.argv[1:]      |
                      | AssertionError: ['fail', 'badly']                    |
                      | ---------------------------------------------------- |
"""

        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_PYCHARM_FAILURES_PATH])
    def test_driver_pycharm_failures(self, stdin, stdout):
        """ PyCharm's Pytest wrapper reports failures, but doesn't set exit code.

        Look for === FAILURES === report.
        """
        source = """\
foo = 'Hello, World!'
"""
        expected_report = """\
foo = 'Hello, World!' | ------------------------- |
                      | Pytest reported failures. |
                      | ------------------------- |
"""

        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'foo',
        'bogus_driver.py'])
    def test_unknown_driver(self, stdin, stdout):
        source = """\
s = 'Yo!'
"""
        expected_report = """\

FileNotFoundError: [Errno 2] No such file or directory: 'bogus_driver.py' |
"""

        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'different_source',
        EXAMPLE_DRIVER_PATH])
    def test_bad_driver(self, stdin, stdout):
        source = """\
def foo(x):
    name = __name__
    return x + 1

BAR = 'baz'
"""
        expected_report = """\
----------------------------------------------------------------------------------- |
example_driver.py doesn't call the different_source module. Try a different driver. |
----------------------------------------------------------------------------------- |

"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_PATCHING_DRIVER_PATH])
    def test_driver_imports_first(self, stdin, stdout):
        source = """\
# This will raise a TypeError, unless we patch the sum() function before
# importing this module. example_patching_driver.py does the patch, so
# it has to be imported before this module.
start = sum([1, 2, "3"])

def foo(x):
    return x + start
"""
        expected_report = """\



start = 99

x = 10
return 109
"""
        stdin.read.return_value = source

        try:
            main()
        except SystemExit:
            pass

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--bad_driver', "Run config 'example' is bad, try something else.",
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'different_source',
        EXAMPLE_DRIVER_PATH])
    def test_bad_driver_message(self, stdin, stdout):
        source = """\
def foo(x):
    name = __name__
    return x + 1

BAR = 'baz'
"""
        expected_report = """\
------------------------------------------------ |
Run config 'example' is bad, try something else. |
------------------------------------------------ |

"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'foo',
        '-m',
        'unittest',
        'foo'])
    def test_unittest_driver_passes(self, stdin, stdout):
        source = """\
from unittest import TestCase

def get_foo(x):
    return x + 5

class FooTest(TestCase):
    def test_get_foo(self):
        y = get_foo(10)
        self.assertEqual(15, y)
"""
        expected_report = """\


x = 10
return 15



y = 15
"""

        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'foo',
        '-m',
        'unittest',
        'foo'])
    def test_unittest_driver_fails(self, stdin, stdout):
        source = """\
from unittest import TestCase

def get_foo(x):
    return x + 500

class FooTest(TestCase):
    def test_get_foo(self):
        y = get_foo(10)
        self.fail(y)
"""
        expected_report = """\
---------------- |
SystemExit: True |
---------------- | | x = 10
                   | return 510



y = 510
AssertionError: 510
"""
        if sys.version_info < (3, 0):
            expected_report = expected_report.replace('(failures=1)',
                                                      'FAIL        ')

        stdin.read.return_value = source

        with self.assertRaises(SystemExit) as ctx:
            main()

        # noinspection PyUnresolvedReferences
        self.assertEqual(1, ctx.exception.code)
        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        '-m',
        'unittest',
        'example_silent_driver'])
    def test_silent_driver(self, stdin, stdout):
        """ Driver calls code, but doesn't generate messages. """
        source = """\
def bar(bucket):
    bucket.add('bar')
"""
        expected_report = """\

"""

        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'foo',
        '-m',
        'doctest',
        'foo.py'])
    def test_doctest_driver_fails(self, stdin, stdout):
        source = """\




def get_foo(x):
    ''' Example for doctest.
    
    >>> get_foo(42)
    942
    '''
    return x + 500
"""
        expected_report = """\
------------------------------------------------ |
SystemExit: 1                                    |
------------------------------------------------ |

x = 42





return 542
"""

        stdin.read.return_value = source
        stdout.encoding = None

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        expected_report = self.trim_exception(expected_report)
        report = self.trim_exception(report)
        self.assertReportEqual(expected_report, report)

    @staticmethod
    def trim_exception(report):
        report = re.sub(r"([ -])+\| *$", "", report, flags=re.MULTILINE)
        report = re.sub(r"line \d+", "line 9999", report)
        report = report.replace("IOError", "FileNotFoundError")
        report = report.replace('path/example_driver.py', EXAMPLE_DRIVER_PATH)
        report = report.replace('path/doctest.py',
                                str(doctest.__file__).strip('c'))
        return report

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        '-m',
        'example_package'])
    def test_driver_package(self, stdin, stdout):
        source = """\
def foo(x):
    return 42
"""
        expected_report = """\
x = 'from package __main__.py'
return 42
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        EXAMPLE_SOURCE_PATH])
    def test_dunder_file(self, stdin, stdout):
        source = """\
import os

filename = os.path.basename(__file__)
"""
        expected_report = """\


filename = 'example_source.py'
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '-m', 'example_source'])
    def test_dunder_file_for_module(self, stdin, stdout):
        source = """\
import os

filename = os.path.basename(__file__)
"""
        expected_report = """\


filename = 'example_source.py'
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--canvas'])
    def test_canvas_main(self, stdin, stdout):
        source = """\
from turtle import *
forward(100)
"""
        expected_report = """\
start_canvas
create_line
    400
    300
    500
    300
    fill='black'
    pensize=1
end_canvas
.


"""
        stdin.read.return_value = source

        main()

        report = ''.join(call_args[0][0]
                         for call_args in stdout.write.call_args_list)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_DRIVER_PATH])
    def test_exception_with_driver(self, stdin, stdout):
        source = """\
import sys
def foo(x):
    sys.exit('Bad stuff.')
"""
        expected_report = """\
---------------------- |
SystemExit: Bad stuff. | x = 42
---------------------- | SystemExit: Bad stuff.
"""
        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        EXAMPLE_SOURCE_PATH])
    def test_syntax_error(self, stdin, stdout):
        source = """\
def missing_body():
"""
        expected_report = """\
SyntaxError: unexpected EOF while parsing
"""
        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--source', '-',
        '--trace_module', 'example_source',
        EXAMPLE_DRIVER_SYNTAX_ERROR_PATH])
    def test_driver_syntax_error(self, stdin, stdout):
        source = """\
x = 'Hello, World!'
"""
        expected_report = """\
{} line 4: SyntaxError: invalid syntax
""".format(EXAMPLE_DRIVER_SYNTAX_ERROR_PATH)
        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)
