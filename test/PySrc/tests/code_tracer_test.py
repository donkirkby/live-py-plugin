import doctest
import os
import re
import sys
from sys import version_info
from unittest import TestCase, skipIf

from mock import call, DEFAULT, patch, Mock

from code_tracer import CodeTracer, main, FileSwallower
from mock_turtle import MockTurtle
from report_builder_test import ReportTestCase

EXAMPLE_DRIVER_PATH = os.path.join(os.path.dirname(__file__),
                                   'example_driver.py')
EXAMPLE_SOURCE_PATH = os.path.join(os.path.dirname(__file__),
                                   'example_source.py')


class CodeTracerTest(ReportTestCase):
    def setUp(self):
        super(CodeTracerTest, self).setUp()
        self.maxDiff = None

    def tearDown(self):
        MockTurtle.remove_monkey_patch()

    def test_empty(self):
        # EXEC
        report = CodeTracer().trace_code("")
        expected_report = ""

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assignment(self):
        # SETUP
        code = """\
i = 1
"""
        expected_report = """\
i = 1 """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_increment(self):
        # SETUP
        code = """\
i = 1
i += 1
"""
        expected_report = """\
i = 1
i = 2 """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_loop(self):
        # SETUP
        code = """\
i = 1
for j in range(3):
    i += j
"""
        expected_report = """\
i = 1
j = 0 | j = 1 | j = 2
i = 1 | i = 2 | i = 4 """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_loop_target_list(self):
        # SETUP
        code = """\
for a,b in [(1,2)]:
    c = a + b
"""
        expected_report = """\
a = 1 b = 2
c = 3
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_nested_loop(self):
        # SETUP
        code = """\
n = 0
for i in range(2):
    n += i
    for j in range(3):
        n += j
"""
        expected_report = """\
n = 0
i = 0                 | i = 1
n = 0                 | n = 4
j = 0 | j = 1 | j = 2 | j = 0 | j = 1 | j = 2
n = 0 | n = 1 | n = 3 | n = 4 | n = 5 | n = 7 """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_mutable(self):
        # SETUP
        code = """\
a = [1, 2, [3, 4]]
a[0] = 9
a[2][1] = 8
b = a
i, j = 2, 1
a[i][j] = 7
a[0:2] = list(reversed(a[0:2]))
b = a
d = -1
a[i:j:d] = [100]
b = a
"""
        expected_report = """\
a = [1, 2, [3, 4]]
a[0] = 9
a[2][1] = 8
b = [9, 2, [3, 8]]
(i, j) = (2, 1)
a[2][1] = 7
a[0:2] = [2, 9]
b = [2, 9, [3, 7]]
d = -1
a[2:1:-1] = [100]
b = [2, 9, 100]
"""
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_mutable_increment(self):
        # SETUP
        code = """\
a = [1, 2, [3, 4]]
a[0] += 9
a[2][1] += 8
b = a
i, j = 2, 1
a[i][j] += 7
b = a
"""
        expected_report = """\
a = [1, 2, [3, 4]]
a[0] = 10
a[2][1] = 12
b = [10, 2, [3, 12]]
(i, j) = (2, 1)
a[2][1] = 19
b = [10, 2, [3, 19]]
"""
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_slice(self):
        # SETUP
        code = """\
a = [1, 2, 3, 4, 5]
i, j = 1, 4
a[i:j] = [20, 30]
b = a
a[2:] = [300]
b = a
a[:2] *= 2
b = a
a[:2] += [21]
b = a
"""
        expected_report = """\
a = [1, 2, 3, 4, 5]
(i, j) = (1, 4)
a[1:4] = [20, 30]
b = [1, 20, 30, 5]
a[2:] = [300]
b = [1, 20, 300]
a[:2] *= 2
b = [1, 20, 1, 20, 300]
a[:2] += [21]
b = [1, 20, 21, 1, 20, 300] """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_slice_magic(self):
        """ All augmented assignments on slices, possible with mocks. """
        # SETUP
        code = """\
from mock import MagicMock


class Foo(MagicMock):
    def __repr__(self):
        return 'Foo()'

foo = Foo()
foo[1] = 3
foo[1:10] = 3
foo[1:10:2] = 3
foo[...] = 3
foo[1, 2:3] = 3
foo[1:10] += 3
foo[1:10] -= 3
foo[1:10] *= 3
foo[1:10] /= 3
foo[1:10] //= 3
foo[1:10] %= 3
foo[1:10] **= 3
foo[1:10] >>= 3
foo[1:10] <<= 3
foo[1:10] &= 3
foo[1:10] ^= 3
foo[1:10] |= 3
"""
        expected_report = """\







foo = Foo()
foo[1] = 3
foo[1:10] = 3
foo[1:10:2] = 3
foo[...] = 3
foo[1, 2:3] = 3
foo[1:10] += 3
foo[1:10] -= 3
foo[1:10] *= 3
foo[1:10] /= 3
foo[1:10] //= 3
foo[1:10] %= 3
foo[1:10] **= 3
foo[1:10] >>= 3
foo[1:10] <<= 3
foo[1:10] &= 3
foo[1:10] ^= 3
foo[1:10] |= 3 """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_method_call(self):
        # SETUP
        code = """\
a = [2, 1]
a.sort()
a.sort() # second call makes no change, nothing printed
"""
        expected_report = """\
a = [2, 1]
a = [1, 2] """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_nested_method_call(self):
        # SETUP
        code = """\
class Foo(object):
    pass

f = Foo()
f.items = []
f.items.append(2)
"""
        expected_report = """\




f.items = []
f.items = [2] """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_method_call_output_param(self):
        # SETUP
        code = """\
from heapq import heappush
l = []
heappush(l, 5)
"""
        expected_report = """\

l = []
l = [5] """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_loop_conditional(self):
        # SETUP
        code = """\
for i in range(3):
    if i == 1:
        c = 5
c = 2
"""
        expected_report = """\
i = 0 | i = 1 | i = 2
      |       |
      | c = 5 |
c = 2 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_function(self):
        # SETUP
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
n = 3 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_empty_return(self):
        # SETUP
        code = """\
def foo(x):
    return

n = foo(10)
"""
        expected_report = """\
x = 10


n = None """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_return_subscript(self):
        # SETUP
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

n = 3 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_chained_function(self):
        # SETUP
        code = """\
def foo(x):
    return x + 10

def bar(y):
    return foo(y) - 2

n = bar(3)
"""
        expected_report = """\
x = 3
return 13

y = 3
return 11

n = 11 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_call_on_literal(self):
        # SETUP
        code = """\
s = 'abc'.replace('a', 'A')
"""
        expected_report = """\
s = 'Abc' """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_function_called_twice(self):
        # SETUP
        code = """\
def foo():
    x = 2
    return x + 10

n = foo()
r = foo()
"""
        expected_report = """\
          |
x = 2     | x = 2
return 12 | return 12

n = 12
r = 12 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_import(self):
        # SETUP
        code = """\
from decimal import Decimal

n = Decimal('10')
"""
        expected_report = """\


n = Decimal('10') """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_runtime_error(self):
        # SETUP
        code = """\
x = 2
raise RuntimeError('Bad stuff happened.')
"""
        expected_report = """\
x = 2
RuntimeError: Bad stuff happened. """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_runtime_error_after_conditional(self):
        # SETUP
        code = """\
if False:
    x = 2
else:
    x = 3
raise RuntimeError('Bad stuff happened.')
"""
        expected_report = """\



x = 3
RuntimeError: Bad stuff happened. """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_runtime_error_caught(self):
        # SETUP
        code = """\
try:
    raise RuntimeError('Bad stuff happened.')
except Exception as e:
    f = e
"""
        expected_report = """\


e = RuntimeError('Bad stuff happened.',)
f = RuntimeError('Bad stuff happened.',) """

        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_runtime_error_caught_unnamed(self):
        # SETUP
        code = """\
try:
    raise RuntimeError('Bad stuff happened.')
except:
    f = 'Worse stuff'
"""
        expected_report = """\



f = 'Worse stuff' """

        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_multiline_error(self):
        # SETUP
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
quality = 1 | ValueError: invalid literal for int() with base 10: 'x' """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_unwinding_exceptions(self):
        # SETUP
        code = """\
def foo(n):
    raise RuntimeError('Bad stuff happened.')

x = foo(5)
"""
        expected_report = """\
n = 5
RuntimeError: Bad stuff happened.

RuntimeError: Bad stuff happened. """

        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_compile_error(self):
        # SETUP
        code = """\
n = 1

if n > 0:
n -= 1
"""
        expected_report = """\



IndentationError: expected an indented block """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_infinite_loop_by_count(self):
        # SETUP
        code = """\
n = 0
while True:
    n += 1
"""
        expected_report = """\
n = 0
      |       |
n = 1 | n = 2 | RuntimeError: live coding message limit exceeded """
        tracer = CodeTracer()
        tracer.message_limit = 4

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_infinite_loop_by_width(self):
        # SETUP
        code = """\
n = 0
while True:
    n += 1
"""
        expected_report = """\
n = 0
      |       |
n = 1 | n = 2 | RuntimeError: live coding message limit exceeded """
        tracer = CodeTracer()
        tracer.max_width = 20

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_infinite_recursion_by_width(self):
        # SETUP
        code = """\
def foo(n):
    foo(n+1)

foo(0)
"""
        expected_report = """\
n = 0                                            | n = 1                                            | n = 2                                            |
RuntimeError: live coding message limit exceeded | RuntimeError: live coding message limit exceeded | RuntimeError: live coding message limit exceeded |

RuntimeError: live coding message limit exceeded
"""
        tracer = CodeTracer()
        tracer.max_width = 20

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_infinite_loop_pass(self):
        # SETUP
        code = """\
while True:
    pass
"""
        expected_report = """\
RuntimeError: live coding message limit exceeded """
        tracer = CodeTracer()
        tracer.message_limit = 3

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_infinite_loop_pass_in_function(self):
        # SETUP
        code = """\
def foo():
    while True:
        pass

foo()
"""
        expected_report = """\

RuntimeError: live coding message limit exceeded


RuntimeError: live coding message limit exceeded """
        tracer = CodeTracer()
        tracer.message_limit = 3

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_set_attribute(self):
        # SETUP
        code = """\
class Dog(object):
    pass

dog = Dog()
dog.name = "Spot"
"""
        expected_report = """\




dog.name = 'Spot' """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_set_attribute_item(self):
        # SETUP
        code = """\
class Shelf(object):
    pass

shelf = Shelf()
shelf.counts = {}
shelf.counts[2] = 3
"""
        expected_report = """\




shelf.counts = {}
shelf.counts[2] = 3 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_set_attribute_attribute(self):
        # SETUP
        code = """\
class foo(object):
    pass

f = foo()
f.child = foo()
f.child.name = 'bob'
"""
        expected_report = """\





f.child.name = 'bob' """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_old_class(self):
        # SETUP
        code = """\
class Dog():
    pass

dog = Dog()
dog.name = "Spot"
"""
        expected_report = """\




dog.name = 'Spot' """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_repr(self):
        # SETUP
        code = """\
class Dog(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'Dog(%r)' % self.name

dog = Dog('Spot')
animal = dog
"""
        expected_report = """\

name = 'Spot'
self.name = 'Spot'




dog = Dog('Spot')
animal = Dog('Spot') """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_module_name(self):
        # SETUP
        code = """\
def foo(x):
    return x + 3

if __name__ == '__live_coding__':
    y = foo(10)
"""
        expected_report = """\
x = 10
return 13


y = 13 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_lambda(self):
        # SETUP
        code = """\
f = lambda n: n + 1
x = f(10)
"""
        expected_report = """\
n = 10
x = 11 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)
        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_turtle(self):
        # SETUP
        code = """\
from turtle import *

forward(100)
"""
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_turtle(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_mainloop(self):
        # SETUP
        code = """\
from turtle import *

mainloop()
"""
        expected_report = """\

"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_yield(self):
        # SETUP
        code = """\
def foo(x):
    a = x
    yield a
    a += 2
    yield a

n = foo(10)
s = sum(n)
"""
        expected_report = """\
x = 10
a = 10
yield 10
a = 12
yield 12


s = 22 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_print(self):
        # SETUP
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

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    @patch('sys.stdout')
    def test_stdout(self, mock_stdout):
        # SETUP
        code = """\
import sys
s = 'x'
sys.stdout.write(s)
"""
        expected_report = """\

s = 'x'
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)
        mock_stdout.write.assert_not_called()

    @patch('sys.stderr')
    def test_stderr(self, mock_stderr):
        # SETUP
        code = """\
import sys
s = 'x'
sys.stderr.write(s)
"""
        expected_report = """\

s = 'x'
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)
        mock_stderr.write.assert_not_called()

    def test_assign_tuple(self):
        # SETUP
        code = """\
b, c = 3, 42
"""
        expected_report = """\
(b, c) = (3, 42) """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_tuple_tuple(self):
        # SETUP
        code = """\
a, (b, c) = (1, (2, 3))
"""
        expected_report = """\
(a, (b, c)) = (1, (2, 3))
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_tuple_list(self):
        # SETUP
        code = """\
a, [b, c] = (1, (2, 3))
"""
        expected_report = """\
(a, (b, c)) = (1, (2, 3)) """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_assignment(self):
        # SETUP
        code = """\
a = b = 2
"""
        expected_report = """\
a = b = 2 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_to_anonymous_attribute(self):
        # SETUP
        code = """\
class Foo(object):
    pass

Foo().x = 2
"""
        expected_report = ""
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_to_expression(self):
        # SETUP
        code = """\
a = [1, 2, 3]

(a or None)[1] = 20
"""
        expected_report = """\
a = [1, 2, 3]
"""
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_augmented_assign_to_expression(self):
        # SETUP
        code = """\
a = [1, 2, 3]

(a or None)[1] += 20
"""
        expected_report = """\
a = [1, 2, 3]
"""
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_method_of_anonymous_object(self):
        # SETUP
        code = """\
def make_string():
    return 'abc'

s = make_string().upper()
"""
        expected_report = """\

return 'abc'

s = 'ABC'
"""
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_recursion(self):
        # SETUP
        code = """\
def f(n):
    r = 1
    for i in range(n):
        r += f(i)
    return r

r = f(2)
"""
        expected_report = """\
n = 2         | n = 0    | n = 1    | n = 0
r = 1         | r = 1    | r = 1    | r = 1
i = 0 | i = 1 |          | i = 0    |
r = 2 | r = 4 |          | r = 2    |
return 4      | return 1 | return 2 | return 1

r = 4 """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_recursion_exception(self):
        # SETUP
        code = """\
def f(n):
    m = n - 1
    if m == 0:
        raise RuntimeError('Invalid n.')
    return f(m)

r = f(2)
"""
        expected_report = """\
n = 2                    | n = 1
m = 1                    | m = 0
                         |
                         | RuntimeError: Invalid n.
RuntimeError: Invalid n. |

RuntimeError: Invalid n. """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_incomplete_iterator(self):
        # SETUP
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



x = 0
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_return_tuple(self):
        # SETUP
        code = """\
def f():
    return (1, 2)

x = f()
"""
        expected_report = """\

return (1, 2)

x = (1, 2) """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_multiline_exception(self):
        # SETUP
        code = """\
x = (
     '')
y = 2
raise RuntimeError('Bad stuff happened.')
"""
        expected_report = """\
x = ''

y = 2
RuntimeError: Bad stuff happened. """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_multivalue_yield(self):
        # SETUP
        code = """\
def foo():
    yield 1, 2

for x in foo():
    pass
"""
        expected_report = """\

yield 1, 2

x = (1, 2) """

        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_yield_none(self):
        # SETUP
        code = """\
def foo():
    yield

for x in foo():
    pass
"""
        expected_report = """\

yield None

x = None """

        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_yield_with_return(self):
        # SETUP
        code = """\
def foo():
    yield 1
    return

for x in foo():
    pass
"""
        expected_report = """\

yield 1


x = 1 """

        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_future(self):
        code = """\
from __future__ import print_function

print('x')
"""
        expected_report = """\


print('x') """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_future_after_docstring_and_comment(self):
        code = """\
''' My module '''

# some comment

from __future__ import print_function

print('x')
"""
        expected_report = """\






print('x') """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_print_with_sep(self):
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
print('Bob', 23, sep='--') """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_print_with_star(self):
        code = """\
from __future__ import print_function

args = ['Bob', 23]
print(*args)
"""
        expected_report = """\


args = ['Bob', 23]
print(*['Bob', 23]) """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_display_star_args(self):
        code = """\
def foo(*args):
    pass

foo('Bob', 1)
"""
        expected_report = """\
args = ('Bob', 1) """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_display_kwargs(self):
        code = """\
def foo(**kwargs):
    pass

foo(name='Bob')
"""
        expected_report = """\
kwargs = {'name': 'Bob'} """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_delete_item(self):
        code = """\
l = [0, 10, 20]
del l[1]
"""
        expected_report = """\
l = [0, 10, 20]
l = [0, 20] """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_delete_attribute(self):
        code = """\
class Foo(object):
    def __init__(self, name=None):
        self.name = name

    def __repr__(self):
        name = getattr(self, 'name', None)
        if name is None:
            return 'Foo()'
        return 'Foo({!r})'.format(name)

f = Foo('Bob')
del f.name
g = f
"""
        expected_report = """\

name = 'Bob'
self.name = 'Bob'







f = Foo('Bob')
f = Foo()
g = Foo() """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_delete_item_of_attribute(self):
        code = """\
class Foo(object):
    pass
f = Foo()
f.l = [0, 10, 20]
del f.l[1]
"""
        expected_report = """\



f.l = [0, 10, 20]
f.l = [0, 20] """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    @skipIf(sys.version_info < (3, 6),
            'f-strings not supported before Python 3.6.')
    def test_f_string(self):
        code = """\
x = 42
s = f'The answer is {x}.'
"""
        expected_report = """\
x = 42
s = 'The answer is 42.' """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    @skipIf(sys.version_info < (3, 6),
            'f-strings not supported before Python 3.6.')
    def test_f_string_in_function(self):
        code = """\
y = 42

def foo(x):
    return f'The answer is {x}.'

z = foo(y)
"""
        expected_report = """\
y = 42

x = 42
return 'The answer is 42.'

z = 'The answer is 42.' """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    @skipIf(sys.version_info < (3, 6),
            'f-strings not supported before Python 3.6.')
    def test_simple_f_string_in_function(self):
        code = """\
y = 42

def foo(x):
    return f'{x}'

z = foo(y)
"""
        expected_report = """\
y = 42

x = 42
return '42'

z = '42' """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)


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

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=['dummy.py'])
    def test_main(self, stdin, stdout):
        code = """\
i = 1
name = __name__
"""
        expected_report = """\
i = 1
name = '__live_coding__' """
        stdin.read.return_value = code

        main()

        self.assertReportEqual(expected_report,
                               stdout.write.call_args_list[0][0][0])

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=['dummy.py',
                                                                '--dump'])
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
        '--dump',
        EXAMPLE_SOURCE_PATH])
    def test_source_file_arg(self, stdin, stdout):
        expected_report = """\
    def foo(x):                       | x = 3
        return x + 1                  | return 4
                                      |
    if __name__ == '__live_coding__': |
        y = foo(3)                    | y = 4
"""

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '-',
        'example_source',
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
        '-',
        'example_source',
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
        '-',
        'example_source',
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
        '-',
        'example_package.lib_in_package',
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
        '-',
        'example_source',
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
        '-',
        'foo',
        EXAMPLE_DRIVER_PATH,
        'fail',
        'badly'])
    def test_driver_fails(self, stdin, stdout):
        source = """\
s = 'Hello, World!'
"""
        expected_report = """\
s = 'Hello, World!' | ---------------------------------------------------- |
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
        '-',
        'foo',
        'bogus_driver.py'])
    def test_bad_driver(self, stdin, stdout):
        source = """\
s = 'Yo!'
"""
        expected_report = """\
s = 'Yo!' | --------------------------------------------------------------- |
          | IOError: [Errno 2] No such file or directory: 'bogus_driver.py' |
          | --------------------------------------------------------------- |
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
        '-',
        'foo',
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
------------ |
unittest: OK |
------------ | | x = 10
               | return 15



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
        '-',
        'foo',
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
        self.assertEqual(15, y)
"""
        expected_report = """\
---------------------- |
unittest: (failures=1) |
---------------------- | | x = 10
                         | return 510



y = 510
AssertionError: 15 != 510
"""
        if sys.version_info < (3, 0):
            expected_report = expected_report.replace('(failures=1)',
                                                      'FAIL        ')

        stdin.read.return_value = source

        with self.assertRaises(SystemExit) as ctx:
            main()

        self.assertEqual(1, ctx.exception.code)
        report = stdout.write.call_args_list[0][0][0]
        report = self.trim_exception(report)
        expected_report = self.trim_exception(expected_report)
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '-',
        'foo',
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
Traceback (most recent call last):               |
  File "path/doctest.py", line 9999, in <module> |
    sys.exit(_test())                            |
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

    def trim_exception(self, report):
        report = re.sub(r"( |-)+\| *$", "", report, flags=re.MULTILINE)
        report = re.sub(r"line \d+", "line 9999", report)
        report = report.replace("IOError", "FileNotFoundError")
        report = report.replace('path/example_driver.py', EXAMPLE_DRIVER_PATH)
        report = report.replace('path/doctest.py',
                                str(doctest.__file__).strip('c'))
        return report

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
        '-',
        'example_source',
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
        '-f',
        '/path/to/foo.py'])
    def test_dunder_file(self, stdin, stdout):
        source = """\
filename = __file__
"""
        expected_report = """\
filename = '/path/to/foo.py'
"""
        stdin.read.return_value = source

        main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py'])
    def test_dunder_file_not_set(self, stdin, stdout):
        source = """\
filename = __file__
"""
        expected_report = """\
NameError: name '__file__' is not defined
"""
        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=[
        'dummy.py',
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
        '-',
        'example_source',
        EXAMPLE_DRIVER_PATH])
    def test_exception_with_driver(self, stdin, stdout):
        source = """\
import sys
def foo(x):
    sys.exit('Bad stuff.')
"""
        expected_report = """\

x = 42
SystemExit: Bad stuff.
"""
        stdin.read.return_value = source

        with self.assertRaises(SystemExit):
            main()

        report = stdout.write.call_args_list[0][0][0]
        self.assertReportEqual(expected_report, report)

    @patch.multiple('sys', stdin=DEFAULT, stdout=DEFAULT, argv=['dummy.py'])
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


class FileSwallowerTest(TestCase):
    def test_mock(self):
        mock_file = Mock()
        swallower = FileSwallower(mock_file)

        swallower.write()
        swallower.flush()

        mock_file.write.assert_not_called()
        mock_file.flush.assert_called_once()
