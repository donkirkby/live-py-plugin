import doctest
import os
import re
import sys

import pytest
from unittest.mock import DEFAULT, patch

from space_tracer import main
from space_tracer.main import replace_input, TraceRunner, analyze
from test_report_builder import ReportTestCase

EXAMPLE_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                   'example_driver.py')
EXAMPLE_SOURCE_PATH = os.path.join(os.path.dirname(__file__),
                                   'example_source.py')
EXAMPLE_PRINTING_PATH = os.path.join(os.path.dirname(__file__),
                                     'example_printing.py')
EXAMPLE_LIB_PATH = os.path.join(os.path.dirname(__file__),
                                'example_package',
                                'lib_in_package.py')
EXAMPLE_PATCHING_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                            'example_patching_driver.py')
EXAMPLE_DRIVER_SYNTAX_ERROR_PATH = os.path.join(os.path.dirname(__file__),
                                                'example_driver_syntax_error.py')
EXAMPLE_PYCHARM_FAILURES_PATH = os.path.join(os.path.dirname(__file__),
                                             'example_pycharm_failures.py')
EXAMPLE_SILENT_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                          'example_silent_driver.py')
patch.multiple = patch.multiple  # Avoids PyCharm warnings.


@pytest.fixture
def stdin():
    with patch('sys.stdin') as mocked:
        yield mocked


@pytest.fixture
def stdout(capsys):
    class MockIO(object):
        @staticmethod
        def getvalue():
            return capsys.readouterr().out
    yield MockIO()


@pytest.fixture
def stderr(capsys):
    class MockIO(object):
        @staticmethod
        def getvalue():
            return capsys.readouterr().err
    yield MockIO()


@pytest.fixture
def argv():
    mocked = []
    with patch('sys.argv', mocked):
        yield mocked


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
        '--traced_file', 'foo.py'])
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', '__live_coding__',
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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
        '--traced_file', EXAMPLE_LIB_PATH,
        '--traced', 'example_package.lib_in_package',
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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

    # noinspection DuplicatedCode
    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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

    # noinspection DuplicatedCode
    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source.foo',
        EXAMPLE_DRIVER_PATH,
        'fail',
        'badly'])
    def test_traced_does_not_hide_error(self, stdin, stdout):
        source = """\
def bar():
    # This would normally
    # be hidden because of
    # --traced, but the start
    # is shown so you can see
    # the error.
    return 42

def foo(x):
    # This is shown, as normal.
    return x

foo(1)
"""
        expected_report = """\
def bar():                      | ---------------------------------------------------- |
    # This would normally       | Traceback (most recent call last):                   |
    # be hidden because of      |   File "path/example_driver.py", line 6, in <module> |
    # --traced, but the start   |     assert 'fail' not in sys.argv, sys.argv[1:]      |
    # is shown so you can see   | AssertionError: ['fail', 'badly']                    |
    # the error.                | ---------------------------------------------------- |
def foo(x):                     | x = 1
    # This is shown, as normal. |
    return x                    | return 1"""

        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        assert report == expected_report

    # noinspection DuplicatedCode
    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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

    # noinspection DuplicatedCode
    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--traced_file', 'foo.py',
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
        '--traced_file', 'different_source.py',
        '--traced', 'different_source',
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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
        '--traced_file', 'different_source.py',
        '--traced', 'different_source',
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
        '--traced_file', 'foo.py',
        '--traced', 'foo',
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
        '--traced_file', 'foo.py',
        '--traced', 'foo',
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
        self.assertReportEqual(report, expected_report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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
        '--traced_file', 'foo.py',
        '--traced', 'foo',
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
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
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


def test_dunder_file(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        EXAMPLE_SOURCE_PATH])
    source = """\
import os

filename = os.path.basename(__file__)
"""
    expected_report = """\


filename = 'example_source.py'
"""
    stdin.read.return_value = source

    main()

    assert expected_report == stdout.getvalue()


def test_dunder_file_for_module(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced', 'example_source',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '-m', 'example_source'])
    source = """\
import os

filename = os.path.basename(__file__)
"""
    expected_report = """\


filename = 'example_source.py'
"""
    stdin.read.return_value = source

    main()

    assert expected_report == stdout.getvalue()


def test_canvas_main(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--canvas'])
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

    assert stdout.getvalue() == expected_report


def test_canvas_error(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--canvas'])
    source = """\
1/0
"""
    stdin.read.return_value = source

    with pytest.raises(SystemExit):
        main()

    report = stdout.getvalue()
    report_lines = report.splitlines()
    end = report_lines.index('end_canvas')
    assert report_lines[end-1] == "    text='ZeroDivisionError: division by zero'"


def test_canvas_syntax_error(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--canvas'])
    source = """\
x = 1
    y = 2
"""
    stdin.read.return_value = source

    with pytest.raises(SystemExit):
        main()

    report = stdout.getvalue()
    report_lines = report.splitlines()
    end = report_lines.index('end_canvas')
    assert report_lines[end-1] == "    text='IndentationError: unexpected indent'"


def test_exception_with_driver(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
        EXAMPLE_DRIVER_PATH])
    source = """\
def foo(x):
    exit('Bad stuff.')
"""
    expected_report = """\
---------------------- | x = 42
SystemExit: Bad stuff. | SystemExit: Bad stuff.
---------------------- |
"""
    stdin.read.return_value = source

    with pytest.raises(SystemExit) as ctx:
        main()

    assert expected_report == stdout.getvalue()
    assert ctx.value.code == 1


def test_exit_return_code(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
        EXAMPLE_DRIVER_PATH])
    source = """\
def foo(x):
    exit(x)
"""
    expected_report = """\
-------------- | x = 42
SystemExit: 42 | SystemExit: 42
-------------- |
"""
    stdin.read.return_value = source

    with pytest.raises(SystemExit) as ctx:
        main()

    assert expected_report == stdout.getvalue()
    assert ctx.value.code == 42


def test_syntax_error(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        EXAMPLE_SOURCE_PATH])
    source = """\
def missing_body():
"""
    if sys.version_info < (3, 9, 0):
        expected_report = """\
SyntaxError: unexpected EOF while parsing
"""
    elif sys.version_info < (3, 10, 0):
        expected_report = """\
IndentationError: expected an indented block
"""
    else:
        expected_report = """\
IndentationError: expected an indented block after function definition on line 1
"""
    stdin.read.return_value = source

    with pytest.raises(SystemExit):
        main()

    assert expected_report == stdout.getvalue()


def test_driver_syntax_error(stdin, stdout, argv):
    argv.extend([
        'dummy.py',
        '--source_width', '0',
        '--traced_file', EXAMPLE_SOURCE_PATH,
        '--traced', 'example_source',
        EXAMPLE_DRIVER_SYNTAX_ERROR_PATH])
    source = """\
x = 'Hello, World!'
"""
    expected_report = """\
{} line 4: SyntaxError: invalid syntax
""".format(EXAMPLE_DRIVER_SYNTAX_ERROR_PATH)
    stdin.read.return_value = source

    with pytest.raises(SystemExit):
        main()

    assert expected_report == stdout.getvalue()


def test_trace_default_module(stdout, argv):
    argv.extend([
        'dummy.py',
        EXAMPLE_PRINTING_PATH])
    expected_report = """\
from __future__ import print_function   |
                                        |
                                        |
def custom_print(text, suffix):         | text = 'Hello, example' | suffix = '!'
    print(text + suffix)                | print('Hello, example!')
                                        |
                                        |
if __name__ == '__main__':              |
    custom_print('Hello, example', '!') |
"""

    main()

    assert expected_report == stdout.getvalue()


def test_dump_whole_file(stdout, argv):
    argv.extend([
        'dummy.py',
        '--traced', '__main__',
        EXAMPLE_PRINTING_PATH])
    expected_report = """\
from __future__ import print_function   |
                                        |
                                        |
def custom_print(text, suffix):         | text = 'Hello, example' | suffix = '!'
    print(text + suffix)                | print('Hello, example!')
                                        |
                                        |
if __name__ == '__main__':              |
    custom_print('Hello, example', '!') |
"""

    main()

    assert expected_report == stdout.getvalue()


def test_traced(stdout, argv):
    argv.extend([
        'dummy.py',
        '--traced', '__main__.custom_print',
        EXAMPLE_PRINTING_PATH])
    expected_report = """\
def custom_print(text, suffix): | text = 'Hello, example' | suffix = '!'
    print(text + suffix)        | print('Hello, example!')
"""

    main()

    assert expected_report == stdout.getvalue()


def test_no_driver(capsys):
    expected_error = ('space_tracer: error: one of the following arguments '
                      'are required: driver or traced_file')
    with pytest.raises(SystemExit):
        TraceRunner().trace_command(['space_tracer'])
    error = capsys.readouterr().err.splitlines()[-1]
    assert error == expected_error


def test_main_from_stdin(stdin):
    stdin.read.return_value = """\
print(40+2)
"""
    expected_report = """\
print(40+2) | print('42')"""

    report = TraceRunner().trace_command(['space_tracer', '-'])

    assert report == expected_report


def test_traced_file_without_traced():
    code = '''\
def foo(n):
    return n + 20
'''
    expected_report = '''\
n = 42
return 62'''

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_width', '0',
            '--traced_file', EXAMPLE_SOURCE_PATH,
            EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_bad_driver_for_traced_file_without_traced():
    code = '''\
def foo(n):
    return n + 20
'''
    expected_report = '''\
--------------------------------------------------------------------------- |
example_driver.py doesn't call example_printing.py. Try a different driver. |
--------------------------------------------------------------------------- |







'''

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', EXAMPLE_PRINTING_PATH,
            EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_traced_driver_environment():
    code = '''\
try:
    sys.exit("Sys exists, but it wasn't imported!")
except NameError:
    pass
sys = None  # Would mess up driver script if they shared module name spaces.

def foo(x):
    return x + 20
'''
    expected_report = '''\

NameError: name 'sys' is not defined


sys = None

x = 42
return 62'''

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_width', '0',
            '--traced_file', EXAMPLE_SOURCE_PATH,
            '--traced', 'example_source',
            EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_top_level_error():
    code = '''\
exit('Failed')
'''
    expected_report = '''\
SystemExit: Failed'''

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_width', '0',
            '--traced_file', EXAMPLE_DRIVER_PATH,
            '--traced=__main__',
            EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_trace_child_package():
    expected_report = """\
def add_message(s):        | s = 'from driver'
    return s + ' received' | return 'from driver received'"""

    report = TraceRunner().trace_command([
        'space_tracer',
        '--traced=example_package.lib_in_package.add_message',
        EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_analyse():
    code = """\
import turtle as t

x = 100
t.forward(x)
"""
    expected_report = """\


x = 100
"""

    report, stdout = analyze(code)

    assert report == expected_report
    assert stdout == ''


def test_analyse_canvas():
    code = """\
import turtle as t

x = 100
t.forward(x)
"""
    canvas_size = (100, 100)
    expected_report = """\
start_canvas
create_line
    50
    50
    150
    50
    fill='black'
    pensize=1
end_canvas
.


x = 100
"""

    report, stdout = analyze(code, canvas_size)

    assert report == expected_report
    assert stdout == ''
