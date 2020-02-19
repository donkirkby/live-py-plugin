from space_tracer.main import TraceRunner, replace_input
from test_report_builder import trim_report


def test_print():
    code = """\
s = 'x'
print(s)
"""
    expected_report_python = """\
s = 'x'
print('x')
"""
    expected_report = expected_report_python
    tracer = TraceRunner()

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
    tracer = TraceRunner()

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
    tracer = TraceRunner()

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
    tracer = TraceRunner()

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
    tracer = TraceRunner()

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
    expected_report_python = """\




f.write('x')
"""
    expected_report = expected_report_python
    tracer = TraceRunner()

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

    report = TraceRunner().trace_code(code)

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

    report = TraceRunner().trace_code(code)

    assert expected_report == trim_report(report)


def test_report_default():
    code = """\
print('Hello,')
print(6*7)
"""
    expected_report = """\
print('Hello,')
print('42')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--report', '-',
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_report_redirect(tmpdir):
    code = """\
print('Hello,')
print(6*7)
"""
    expected_report = """\
print('Hello,')
print('42')"""

    report_path = tmpdir.join('report.txt')
    with replace_input(code):
        stdout_report = TraceRunner().trace_command([
            'space_tracer',
            '--report', str(report_path),
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])
    report = report_path.read_text('utf8')

    assert stdout_report == ''
    assert report == expected_report


def test_report_redirect_to_stderr(capsys):
    code = """\
print('Hello,')
print(6*7)
"""
    expected_report = """\
print('Hello,')
print('42')"""

    with replace_input(code):
        stdout_report = TraceRunner().trace_command([
            'space_tracer',
            '--report', '!',
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])
    report = capsys.readouterr().err

    assert stdout_report == ''
    assert report == expected_report


def test_stdout_redirect(tmpdir):
    code = """\
print('Hello,')
print(6*7)
"""
    expected_report = """\
print('Hello,')
print('42')"""
    expected_redirect = """\
Hello,
42
"""

    stdout_path = tmpdir.join('stdout.txt')
    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--stdout', str(stdout_path),
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])
    stdout_redirect = stdout_path.read_text('utf8')

    assert report == expected_report
    assert stdout_redirect == expected_redirect


def test_stderr_redirect(tmpdir):
    code = """\
import sys
print('Hello,', file=sys.stderr)
print(6*7)
"""
    expected_report = """\

sys.stderr.write('Hello,\\n')
print('42')"""
    expected_redirect = """\
Hello,
"""

    stderr_path = tmpdir.join('stderr.txt')
    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--stderr', str(stderr_path),
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])
    stderr_redirect = stderr_path.read_text('utf8')

    assert report == expected_report
    assert stderr_redirect == expected_redirect


def test_redirect_both(tmpdir):
    code = """\
import sys
print('Hello,', file=sys.stderr)
print(6*7)
"""
    expected_report = """\

sys.stderr.write('Hello,\\n')
print('42')"""
    expected_redirect = """\
Hello,
42
"""

    out_path = tmpdir.join('out.txt')
    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--stdout', str(out_path),
            '--stderr', str(out_path),
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])
    out_redirect = out_path.read_text('utf8')

    assert report == expected_report
    assert out_redirect == expected_redirect


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
    expected_report_python = """\








name = 'Alice'
self.f.write('Hello, Alice.')



"""
    expected_report = expected_report_python

    report = TraceRunner().trace_code(code)

    assert expected_report == trim_report(report)


def test_no_input():
    code = """\
s = input()
"""
    expected_report = """\
EOFError: EOF when reading a line
"""

    report = TraceRunner().trace_code(code)

    assert expected_report == trim_report(report)


def test_input(tmpdir):
    input_text = """\
first line
second line
"""
    code = "s = input()"
    expected_report = """\
s = 'first line'
"""
    stdin_path = str(tmpdir.join('stdin.txt'))
    with open(stdin_path, 'w') as f:
        f.write(input_text)

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--stdin', stdin_path,
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == trim_report(report)


def test_input_stdin(tmpdir):
    input_text = """\
first line
second line
"""
    code = "s = input()"
    expected_report = """\
s = 'first line'
"""
    code_path = str(tmpdir.join('example.py'))
    with open(code_path, 'w') as f:
        f.write(code)

    with replace_input(input_text):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--stdin', '-',
            '--source_width', '0',
            code_path])

    assert expected_report == trim_report(report)


def test_default_input(tmpdir):
    code = "s = input()"
    expected_report = """\
EOFError: EOF when reading a line
"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == trim_report(report)


def test_prompt(tmpdir):
    input_text = """\
first line
second line
"""
    code = "s = input('What comes first?')"
    expected_report = """\
sys.stdout.write('What comes first?') | s = 'first line'
"""
    stdin_path = str(tmpdir.join('stdin.txt'))
    with open(stdin_path, 'w') as f:
        f.write(input_text)

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--stdin', stdin_path,
            '--source_width', '0',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == trim_report(report)
