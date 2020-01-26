import sys

import pytest

from space_tracer.main import TraceRunner


def test_function():
    code = """\
def foo(x):
    a = x
    b = x + 1
    return b

m = 2
n = foo(m)
"""
    expected_report = """\
x = 2
a = 2
b = 3
return 3

m = 2
n = 3"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_empty_return():
    code = """\
def foo(x):
    return

n = foo(10)
"""
    expected_report = """\
x = 10


n = None"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_empty_function():
    code = '''\
def foo(x):
    """ No function body, just a docstring. """

n = foo(10)
'''
    expected_report = """\
x = 10


n = None"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_return_subscript():
    code = """\
def foo(x):
    a = [x, 3]
    return a[1]

n = foo(2)
"""
    expected_report = """\
x = 2
a = [2, 3]
return 3

n = 3"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


@pytest.mark.skipif(sys.version_info < (3, 8),
                    reason='Positional-only params not supported before 3.8.')
def test_positional_parameters():
    code = """\
def foo(a, /, b):
    print(a, b)

foo(1, 2)
"""
    expected_report = """\
a = 1 | b = 2
print('1 2')

"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_keyword_only_parameters():
    code = """\
def foo(a, *, b):
    print(a, b)

foo(1, b=2)
"""
    expected_report = """\
a = 1 | b = 2
print('1 2')

"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_method_call():
    code = """\
a = [2, 1]
a.sort()
a.sort() # second call makes no change, nothing printed
"""
    expected_report = """\
a = [2, 1]
a = [1, 2]
"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_nested_method_call():
    code = """\
class Foo(object):
    pass

f = Foo()
f.items = []
f.items.append(2)
"""
    expected_report = """\




f.items = []
f.items = [2]"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_method_call_output_param():
    code = """\
from heapq import heappush
l = []
heappush(l, 5)
"""
    expected_report = """\

l = []
l = [5]"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_method_call_output_param_keyword():
    code = """\
def grow(l):
    l.append(0)
a = []
grow(l=a)
"""
    expected_report = """\
l = []
l = [0]
a = []
a = [0]"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_incomplete_iterator():
    code = """\
def gen(n):
    state = 'Starting'
    try:
        for i in range(n):
            yield i
    finally:
        state = 'Done'


g = gen(999)
x = next(g)
"""

    expected_report = """\
n = 999
state = 'Starting'

i = 0
yield 0 GeneratorExit

state = 'Done'



x = 0"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_incomplete_iterator_prints():
    code = """\
def gen(n):
    print('Starting')
    try:
        for i in range(n):
            yield i
    finally:
        print('Done')


g = gen(999)
x = next(g)
"""

    expected_report = """\
n = 999
print('Starting')

i = 0
yield 0 GeneratorExit

print('Done')



x = 0"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report
