from space_tracer.code_tracer import CodeTracer
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

    report = CodeTracer().trace_code(code, dump=True)

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

    report = CodeTracer().trace_code(code, dump=True)

    assert trim_report(expected_report) == trim_report(report)
