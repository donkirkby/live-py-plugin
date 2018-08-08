import pytest
import sys

from code_tracer import CodeTracer
from test_report_builder import trim_report


def test_assign_tuple():
    code = """\
from __future__ import print_function

a = (1,)
b, = (1,)
(c) = (1,)
(d,) = (1,)

print(a)
print(b)
print(c)
print(d)

a, b = 1, 2
(c, d) = (3, 4)
"""
    expected_report = """\


a = (1,)
(b,) = (1,)
c = (1,)
(d,) = (1,)

print('(1,)')
print('1')
print('(1,)')
print('1')

(a, b) = (1, 2)
(c, d) = (3, 4)
"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_tuple_tuple():
    code = """\
a, (b, c) = (1, (2, 3))
"""
    expected_report = """\
(a, (b, c)) = (1, (2, 3))
"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report, trim_report(report)


def test_assign_tuple_list():
    code = """\
a, [b, c] = (1, (2, 3))
"""
    expected_report = """\
(a, (b, c)) = (1, (2, 3)) """
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report, trim_report(report)


def test_assign_generator_unpacked():
    code = """\
a, b = (3*i for i in range(2))
"""
    expected_report = """\
(a, b) = (0, 3) """
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report, trim_report(report)


def test_assign_assignment():
    code = """\
a = b = 2
"""
    expected_report = """\
a = b = 2 """
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report, trim_report(report)


def test_assign_generator_assignment():
    """ Can't convert to tuple for more than one assignment. """
    code = """\
a, b = c = (3*i for i in range(2))
d = a, b
e = list(c)
"""
    expected_report = """\

d = (0, 3)
e = []
"""
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report, trim_report(report)


def test_assign_to_anonymous_attribute():
    code = """\
class Foo(object):
pass

Foo().x = 2
"""
    expected_report = """\



"""
    report = CodeTracer().trace_code(code)

    assert expected_report, trim_report(report)


def test_assign_to_expression():
    code = """\
a = [1, 2, 3]

(a or None)[1] = 20
"""
    expected_report = """\
a = [1, 2, 3]

"""
    report = CodeTracer().trace_code(code)

    assert expected_report, trim_report(report)


def test_augmented_assign_to_expression():
    code = """\
a = [1, 2, 3]

(a or None)[1] += 20
"""
    expected_report = """\
a = [1, 2, 3]

"""
    report = CodeTracer().trace_code(code)

    assert expected_report, trim_report(report)


@pytest.mark.skipif(
    sys.version_info < (3, 0),
    reason='starred assignment not supported before Python 3.0.')
def test_assign_starred():
    code = """\
a, *b = (1, 2, 3)
print(b)
"""
    expected_report = """\
(a, *b) = (1, 2, 3)
print('[2, 3]') """
    tracer = CodeTracer()

    report = tracer.trace_code(code)

    assert expected_report, trim_report(report)
