from space_tracer.main import TraceRunner
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

    report = TraceRunner().trace_code(code, source_width=None, source_indent=4)

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

    report = TraceRunner().trace_code(code, source_width=None, source_indent=4)

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

    report = TraceRunner().trace_code(code,
                                      traced='__main__.bar',
                                      source_width=None)

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

    report = TraceRunner().trace_code(code, source_width=None)

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

    report = TraceRunner().trace_code(code, source_width=None)

    assert expected_report == report
