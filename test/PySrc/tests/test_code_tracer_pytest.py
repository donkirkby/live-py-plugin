import sys

from space_tracer.main import TraceRunner
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
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_tuple_tuple():
    code = """\
a, (b, c) = (1, (2, 3))
"""
    expected_report = """\
(a, (b, c)) = (1, (2, 3))
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_tuple_list():
    code = """\
a, [b, c] = (1, (2, 3))
"""
    expected_report = """\
(a, (b, c)) = (1, (2, 3))
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_generator_unpacked():
    code = """\
a, b = (3*i for i in range(2))
"""
    expected_report = """\
(a, b) = (0, 3)
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_assignment():
    code = """\
a = b = 2
"""
    expected_report = """\
a = b = 2
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


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
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_to_anonymous_attribute():
    code = """\
class Foo(object):
    pass

Foo().x = 2
"""
    expected_report = """\



"""
    report = TraceRunner().trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_to_expression():
    code = """\
a = [1, 2, 3]

(a or None)[1] = 20
"""
    expected_report = """\
a = [1, 2, 3]

"""
    report = TraceRunner().trace_code(code)

    assert expected_report == trim_report(report)


def test_augmented_assign_to_expression():
    code = """\
a = [1, 2, 3]

(a or None)[1] += 20
"""
    expected_report = """\
a = [1, 2, 3]

"""
    report = TraceRunner().trace_code(code)

    assert expected_report == trim_report(report)


def test_assign_starred():
    code = """\
a, *b = (1, 2, 3)
print(b)
"""
    expected_report = """\
(a, *b) = (1, 2, 3)
print('[2, 3]')
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_docstring():
    code = """\
''
"""
    expected_report = """\

"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_runtime_error():
    code = """\
x = 2
raise RuntimeError('Bad stuff happened.')
"""
    expected_report = """\
x = 2
RuntimeError: Bad stuff happened.
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_runtime_error_after_conditional():
    code = """\
if False:
    x = 2
else:
    x = 3
raise RuntimeError('Bad stuff happened.')
"""
    expected_report = """\



x = 3
RuntimeError: Bad stuff happened.
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_runtime_error_caught():
    code = """\
try:
    raise RuntimeError('Bad stuff happened.')
except Exception as e:
    f = e
"""
    expected_report = """\

RuntimeError: Bad stuff happened.
e = RuntimeError('Bad stuff happened.',)
f = RuntimeError('Bad stuff happened.',)
"""
    if sys.version_info >= (3, 7, 0):
        expected_report = expected_report.replace(',)', ')')

    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_runtime_error_caught_unnamed():
    code = """\
try:
    raise RuntimeError('Bad stuff happened.')
except:
    f = 'Worse stuff'
"""
    expected_report = """\

RuntimeError: Bad stuff happened.

f = 'Worse stuff'
"""

    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_multiline_error():
    code = """\
quality = 0
for c in ['1',
          'x']:
    quality += int(c)
"""
    expected_report = """\
quality = 0
c = '1'     | c = 'x'
            |
quality = 1 | ValueError: invalid literal for int() with base 10: 'x'
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_unwinding_exceptions():
    code = """\
def foo(n):
    raise RuntimeError('Bad stuff happened.')

x = foo(5)
"""
    expected_report = """\
n = 5
RuntimeError: Bad stuff happened.

RuntimeError: Bad stuff happened.
"""

    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_reraise():
    code = """\
x = 2
try:
    raise RuntimeError('Bad stuff happened.')
except:
    raise
"""
    expected_report = """\
x = 2

RuntimeError: Bad stuff happened.

RuntimeError: Bad stuff happened.
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)


def test_nested_reraise():
    code = """\
x = 2
try:
    try:
        raise RuntimeError('Bad stuff happened.')
    except:
        raise
except:
    raise
"""
    expected_report = """\
x = 2


RuntimeError: Bad stuff happened.

RuntimeError: Bad stuff happened.

RuntimeError: Bad stuff happened.
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert expected_report == trim_report(report)
