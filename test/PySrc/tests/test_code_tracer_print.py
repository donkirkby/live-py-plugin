from sys import version_info

from code_tracer import CodeTracer
from test_report_builder import trim_report


def test_print():
    code = """\
s = 'x'
print(s)
"""
    expected_report_python2 = """\
s = 'x'
print 'x'
"""
    expected_report_python3 = """\
s = 'x'
print('x')
"""
    expected_report = (expected_report_python3
                       if version_info.major >= 3
                       else expected_report_python2)
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_print_in_function():
    code = """\
from __future__ import print_function
def main():
    s = 'Hello, World!'
    print(s)

main()
"""
    expected_report = """\


s = 'Hello, World!'
print('Hello, World!')

"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_print_in_loop():
    code = """\
from __future__ import print_function
for i in range(3):
    print(i)
"""
    expected_report = """\

i = 0      | i = 1      | i = 2
print('0') | print('1') | print('2')
"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_stdout(capsys):
    code = """\
import sys
s = 'x'
sys.stdout.write(s)
"""
    expected_report = """\

s = 'x'
sys.stdout.write('x')
"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)
    std = capsys.readouterr()
    assert "" == std.out


def test_stderr(capsys):
    code = """\
import sys
s = 'x'
sys.stderr.write(s)
"""
    expected_report = """\

s = 'x'
sys.stderr.write('x')
"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)
    std = capsys.readouterr()
    assert "" == std.err


def test_string_io():
    code = """\
from __future__ import unicode_literals
from io import StringIO

f = StringIO()
f.write('x')
"""
    expected_report_python2 = """\




f.write(u'x')
"""
    expected_report_python3 = """\




f.write('x')
"""
    expected_report = (expected_report_python3
                       if version_info.major >= 3
                       else expected_report_python2)
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_print_with_sep():
    code = """\
from __future__ import print_function

p = 'Bob'
n = 23
s = '--'
print(p, n, sep=s)
"""
    expected_report = """\


p = 'Bob'
n = 23
s = '--'
print('Bob--23')
"""

    report = CodeTracer().trace_code(code)

    assert expected_report == trim_report(report)


def test_print_with_star():
    code = """\
from __future__ import print_function

args = ['Bob', 23]
print(*args)
"""
    expected_report = """\


args = ['Bob', 23]
print('Bob 23')
"""

    report = CodeTracer().trace_code(code)

    assert expected_report == trim_report(report)


def test_string_io_field():
    code = """\
from __future__ import unicode_literals
from io import StringIO


class Foo:
    def __init__(self, f):
        self.f = f

    def greet(self, name):
        self.f.write('Hello, ' + name + '.')

greetings = StringIO()
foo = Foo(greetings)
foo.greet('Alice')
"""
    expected_report_python2 = """\








name = u'Alice'
self.f.write(u'Hello, Alice.')



"""
    expected_report_python3 = """\








name = 'Alice'
self.f.write('Hello, Alice.')



"""
    expected_report = (expected_report_python3
                       if version_info.major >= 3
                       else expected_report_python2)

    report = CodeTracer().trace_code(code)

    assert expected_report == trim_report(report)


def test_no_input(tmpdir):
    code = """\
s = input()
"""
    expected_report = """\
EOFError: EOF when reading a line
"""

    report = CodeTracer().trace_code(code)

    assert expected_report == trim_report(report)


def test_input(tmpdir):
    input_text = """\
first line
second line
"""
    code = """\
s = input()
"""
    expected_report = """\
s = 'first line'
"""
    stdin_path = tmpdir.join('stdin.txt')
    with open(stdin_path, 'w') as f:
        f.write(input_text)

    report = CodeTracer().trace_code(code, stdin=stdin_path)

    assert expected_report == trim_report(report)


def test_prompt(tmpdir):
    input_text = """\
first line
second line
"""
    code = r"s = input('What comes first?\n')"
    expected_report = """\
print('What comes first?') | s = 'first line'
"""
    stdin_path = tmpdir.join('stdin.txt')
    with open(stdin_path, 'w') as f:
        f.write(input_text)

    report = CodeTracer().trace_code(code, stdin=stdin_path)

    assert expected_report == trim_report(report)
