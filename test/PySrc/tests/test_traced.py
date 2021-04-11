import os

from space_tracer.main import TraceRunner, replace_input
from test_code_tracer_main import EXAMPLE_DRIVER_PATH, EXAMPLE_PRINTING_PATH, EXAMPLE_SOURCE_PATH
from test_report_builder import trim_report


def test_one_function():
    code = """\
from space_tracer import traced


@traced
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s


def bar(num):
    s = 'a'
    for i in range(num):
        s += 'b'
    return s

print(foo(3))
print(bar(3))
"""
    expected_report = """\
    @traced                |
    def foo(n):            | n = 3
        s = 'x'            | s = 'x'
        for i in range(n): | i = 0    | i = 1     | i = 2
            s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s           | return 'xyyy' """

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced_file', 'example.py',
            'example.py'])

    assert trim_report(expected_report) == trim_report(report)


def test_two_functions():
    code = """\
from space_tracer import traced


@traced
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s


@traced
def bar(num):
    s = 'a'
    for i in range(num):
        s += 'b'
    return s


print(foo(3))
print(bar(3))
"""
    expected_report = """\
    @traced                  |
    def foo(n):              | n = 3
        s = 'x'              | s = 'x'
        for i in range(n):   | i = 0    | i = 1     | i = 2
            s += 'y'         | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s             | return 'xyyy'
    @traced                  |
    def bar(num):            | num = 3
        s = 'a'              | s = 'a'
        for i in range(num): | i = 0    | i = 1     | i = 2
            s += 'b'         | s = 'ab' | s = 'abb' | s = 'abbb'
        return s             | return 'abbb'
"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced_file', 'example.py',
            'example.py'])

    assert trim_report(expected_report) == trim_report(report)


def test_command_line():
    """ Specify a traced method with command-line option --traced. """
    code = """\
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s


def bar(num):
    s = 'a'
    for i in range(num):
        s += 'b'
    return s


print(foo(3))
print(bar(3))
"""
    expected_report = """\
def bar(num):            | num = 3
    s = 'a'              | s = 'a'
    for i in range(num): | i = 0    | i = 1     | i = 2
        s += 'b'         | s = 'ab' | s = 'abb' | s = 'abbb'
    return s             | return 'abbb'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced', '__main__.bar',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == report


def test_without_traced_file():
    expected_report = """\
def foo(x):                       | x = 42
    return x + 1                  | return 43
                                  |
                                  |
def bar(bucket):                  |
    bucket.add('bar')             |
                                  |
                                  |
if __name__ == '__live_coding__': |
    y = foo(3)                    |"""

    report = TraceRunner().trace_command([
        'space_tracer',
        '--traced', 'example_source',
        EXAMPLE_DRIVER_PATH])

    assert expected_report == report


def test_default_traced():
    expected_report = """\
from __future__ import print_function   |
                                        |
                                        |
def custom_print(text, suffix):         | text = 'Hello, example' | suffix = '!'
    print(text + suffix)                | print('Hello, example!')
                                        |
                                        |
if __name__ == '__main__':              |
    custom_print('Hello, example', '!') |"""

    report = TraceRunner().trace_command([
        'space_tracer',
        EXAMPLE_PRINTING_PATH])

    assert expected_report == report


def test_traced_main_without_traced_file():
    expected_report = """\
def custom_print(text, suffix): | text = 'Hello, example' | suffix = '!'
    print(text + suffix)        | print('Hello, example!')"""

    report = TraceRunner().trace_command([
        'space_tracer',
        '--traced=__main__.custom_print',
        EXAMPLE_PRINTING_PATH])

    assert report == expected_report


def test_traced_function():
    code = """\
def foo(n):
    return n + 1

def bar(n):
    return n - 1

foo(10)
bar(20)
"""
    expected_report = """\
def bar(n):      | n = 20
    return n - 1 | return 19"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced=bar',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == report


def test_other_decorator():
    """ Other decorators shouldn't affect tracing. """
    code = """\
from __future__ import print_function
class Foo(object):
    def foo(self, x):
        return x + 1
    
    @staticmethod
    def bar(x):
        return x + 2

f = Foo()
print(f.foo(10))
print(f.bar(20))
"""
    expected_report = """\
from __future__ import print_function |
class Foo(object):                    |
    def foo(self, x):                 | x = 10
        return x + 1                  | return 11
                                      |
    @staticmethod                     |
    def bar(x):                       | x = 20
        return x + 2                  | return 22
                                      |
f = Foo()                             |
print(f.foo(10))                      | print('11')
print(f.bar(20))                      | print('22')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == report


def test_attribute_decorator():
    """ The decorator is a module attribute. """
    code = """\
from __future__ import print_function
class Foo(object):
    def foo(self, x):
        return x + 1
    
    @__builtins__.staticmethod
    def bar(x):
        return x + 2

f = Foo()
print(f.foo(10))
print(f.bar(20))
"""
    expected_report = """\
from __future__ import print_function |
class Foo(object):                    |
    def foo(self, x):                 | x = 10
        return x + 1                  | return 11
                                      |
    @__builtins__.staticmethod        |
    def bar(x):                       | x = 20
        return x + 2                  | return 22
                                      |
f = Foo()                             |
print(f.foo(10))                      | print('11')
print(f.bar(20))                      | print('22')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == report


def test_traced_renamed():
    code = """\
from space_tracer import traced as space_traced


@space_traced
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    @space_traced          |
    def foo(n):            | n = 3
        s = 'x'            | s = 'x'
        for i in range(n): | i = 0    | i = 1     | i = 2
            s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s           | return 'xyyy' """

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced_file', 'example.py',
            'example.py'])

    assert trim_report(expected_report) == trim_report(report)


def test_traced_with_block():
    code = """\
from space_tracer import traced

def foo(n):
    s = 'x'
    with traced():
        t = 23
        for i in range(n):
            s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    with traced():         |
        t = 23             | t = 23
        for i in range(n): | i = 0    | i = 1     | i = 2
            s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_other_decorator_renamed():
    """ The decorator is a module attribute. """
    code = """\
traced = staticmethod

class Foo(object):
    def foo(self, x):
        return x + 1
    
    @traced
    def bar(x):
        return x + 2

f = Foo()
print(f.foo(10))
print(f.bar(20))
"""
    expected_report = """\
traced = staticmethod |
                      |
class Foo(object):    |
    def foo(self, x): | x = 10
        return x + 1  | return 11
                      |
    @traced           |
    def bar(x):       | x = 20
        return x + 2  | return 22
                      |
f = Foo()             |
print(f.foo(10))      | print('11')
print(f.bar(20))      | print('22')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert expected_report == report


def test_one_function_live_mode():
    """ Need to keep original vertical position to line up with editor. """
    code = """\
from space_tracer import traced


@traced
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s


def bar(num):
    s = 'a'
    for i in range(num):
        s += 'b'
    return s


print(foo(3))
print(bar(3))
"""
    expected_report = """\




n = 3
s = 'x'
i = 0    | i = 1     | i = 2
s = 'xy' | s = 'xyy' | s = 'xyyy'
return 'xyyy'










"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_width', '0',
            '--live',
            '--traced_file', 'example.py',
            'example.py'])

    assert trim_report(expected_report) == trim_report(report)


def test_relative_traced_file():
    code = """\
def foo(x):
    return x + 100
"""
    expected_report = """\
def foo(x):        | x = 42
    return x + 100 | return 142"""
    relative_path = os.path.relpath(EXAMPLE_SOURCE_PATH, os.getcwd())

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', relative_path,
            EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_unknown_traced_file():
    code = 'Yo!'
    expected_report = """\
--------------------------------------------------------------------- |
example_driver.py doesn't call bogus_file.py. Try a different driver. |
--------------------------------------------------------------------- |







"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--traced_file', 'bogus_path/bogus_file.py',
            EXAMPLE_DRIVER_PATH])

    assert report == expected_report


def test_hide_name():
    code = """\
from space_tracer import traced


@traced(hide='i')
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    @traced(hide='i')      |
    def foo(n):            | n = 3
        s = 'x'            | s = 'x'
        for i in range(n): |          |           |
            s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s           | return 'xyyy'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_hide_names_list():
    code = """\
from space_tracer import traced


@traced(hide=['i', 'n'])
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    @traced(hide=['i', 'n']) |
    def foo(n):              |
        s = 'x'              | s = 'x'
        for i in range(n):   |          |           |
            s += 'y'         | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s             | return 'xyyy'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_hide_garbage():
    """ Not a string or iterable collection of strings, ignore it. """
    code = """\
from space_tracer import traced


@traced(hide=42)
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    @traced(hide=42)       |
    def foo(n):            | n = 3
        s = 'x'            | s = 'x'
        for i in range(n): | i = 0    | i = 1     | i = 2
            s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s           | return 'xyyy'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_hide_from_command_line():
    code = """\
from space_tracer import traced


def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    def foo(n):            |
        s = 'x'            | s = 'x'
        for i in range(n): |          |           |
            s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
        return s           | return 'xyyy'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--source_indent', '4',
            '--traced', 'foo',
            '--hide', 'n', 'i',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_line_numbers():
    code = """\
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    for i in range(n): | i = 0    | i = 1     | i = 2
        s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
    return s           | return 'xyyy'"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--start_line', '3',
            '--end_line', '5',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_start_line_only():
    code = """\
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
    for i in range(n): | i = 0    | i = 1     | i = 2
        s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
    return s           | return 'xyyy'
                       |
print(foo(3))          | print('xyyy')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--start_line', '3',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_end_line_only():
    code = """\
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
def foo(n):            | n = 3
    s = 'x'            | s = 'x'
    for i in range(n): | i = 0    | i = 1     | i = 2"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--end_line', '3',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_show_line_numbers():
    code = """\
a = 1
b = 2
print(a + b)
"""
    expected_report = """\
2) b = 2        | b = 2
3) print(a + b) | print('3')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--start_line', '2',
            '--end_line', '3',
            '--line_numbers',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_show_line_numbers_pad_right():
    code = """\
a = 1
b = 2
c = 3
d = 4
e = 5
f = 6
g = 7
h = 8
i = 9
print(f*g)"""
    expected_report = """\
 9) i = 9      | i = 9
10) print(f*g) | print('42')"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--start_line', '9',
            '--end_line', '10',
            '--line_numbers',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_line_numbers_live():
    code = """\
def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s

print(foo(3))
"""
    expected_report = """\
def foo(n):            |
    s = 'x'            |
    for i in range(n): | i = 0    | i = 1     | i = 2
        s += 'y'       | s = 'xy' | s = 'xyy' | s = 'xyyy'
    return s           | return 'xyyy'
                       |
print(foo(3))          |"""

    with replace_input(code):
        report = TraceRunner().trace_command([
            'space_tracer',
            '--start_line', '3',
            '--end_line', '5',
            '--live',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_traced_decorator_avoids_message_limit():
    code = """\
from space_tracer import traced


def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s


@traced
def bar(n):
    s = 'a'
    for i in range(n):
        s += 'b'
    return s

print(foo(3000))
print(bar(3))
"""
    expected_report = """\
@traced                |
def bar(n):            | n = 3
    s = 'a'            | s = 'a'
    for i in range(n): | i = 0    | i = 1     | i = 2
        s += 'b'       | s = 'ab' | s = 'abb' | s = 'abbb'
    return s           | return 'abbb'"""

    tracer = TraceRunner()
    tracer.message_limit = 20

    with replace_input(code):
        report = tracer.trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_infinite_loop_before_traced_block():
    code = """\
from space_tracer import traced


def foo():
    while True:
        pass


@traced
def bar(n):
    s = 'a'
    for i in range(n):
        s += 'b'
    return s

foo()
print(bar(3))
"""
    expected_report = """\
from space_tracer import traced | -------------------------------- |
                                | Traced blocks were never called. |
                                | -------------------------------- |"""

    tracer = TraceRunner()
    tracer.message_limit = 20

    with replace_input(code):
        report = tracer.trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_traced_function_not_called():
    code = """\
from space_tracer import traced


def foo(n):
    s = 'x'
    for i in range(n):
        s += 'y'
    return s


@traced
def bar(n):
    s = 'a'
    for i in range(n):
        s += 'b'
    return s

print(foo(3))
"""
    expected_report = """\
from space_tracer import traced | -------------------------------- |
                                | Traced blocks were never called. |
                                | -------------------------------- |"""

    tracer = TraceRunner()

    with replace_input(code):
        report = tracer.trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report


def test_traced_decorator_in_another_file():
    code = """\
import example_traced

print('Done.')
"""
    expected_report = """\
import example_traced |
                      |
print('Done.')        | print('Done.')"""

    tracer = TraceRunner()

    with replace_input(code):
        report = tracer.trace_command([
            'space_tracer',
            '--traced_file', 'example.py',
            'example.py'])

    assert report == expected_report
