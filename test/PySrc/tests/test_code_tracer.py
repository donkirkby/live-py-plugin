import sys
from sys import version_info
from tempfile import TemporaryFile
from unittest import skipIf

from mock import patch

from code_tracer import CodeTracer, FileSwallower
from mock_turtle import MockTurtle
from report_builder import ReportBuilder
from test_report_builder import ReportTestCase


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
a = 1 | b = 2
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

    def test_empty_function(self):
        # SETUP
        code = '''\
def foo(x):
    """ No function body, just a docstring. """

n = foo(10)
'''
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
        tracer.message_limit = 8

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
n = 0                                            | n = 1                                            |
RuntimeError: live coding message limit exceeded | RuntimeError: live coding message limit exceeded |

RuntimeError: live coding message limit exceeded
"""
        tracer = CodeTracer()
        tracer.max_width = 13

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

    def test_infinite_loop_in_repr(self):
        # SETUP
        # __repr__() doesn't display messages, so message counter doesn't change.
        code = """\
class Foo(object):
    def __repr__(self):
        while True:
            x = 42

s = Foo()
s2 = 'x'
"""
        # The infinite loop fails silently inside __repr__().
        expected_report = """\






s2 = 'x'
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_infinite_recursion_in_repr(self):
        # SETUP
        code = """\
class Foo:
    def get_bar(self):
        self.do_foo()
        return 'bar'

    def __repr__(self):
        return 'Foo' + self.get_bar()

    def do_foo(self):
        pass

f = Foo()
f.get_bar()
"""
        expected_report = """\



return 'bar'







f = Foobar
"""
        tracer = CodeTracer()

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

    def test_repr_call(self):
        # SETUP
        code = """\
class Dog(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'Dog(%r)' % self.name

dog1 = Dog('Spot')
dog2 = Dog('Fido')
s = repr(dog2)
"""
        expected_report = """\

name = 'Spot'      | name = 'Fido'
self.name = 'Spot' | self.name = 'Fido'


return "Dog('Fido')"

dog1 = Dog('Spot')
dog2 = Dog('Fido')
s = "Dog('Fido')"
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_repr_return(self):
        # SETUP
        code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

def bar(x):
    return Foo(x)

y = bar(23)
"""
        expected_report = """\

x = 23
self.x = 23




x = 23
return Foo(23)

y = Foo(23)
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_repr_yield(self):
        # SETUP
        code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

def bar(x):
    yield Foo(x)

y = list(bar(23))
"""
        expected_report = """\

x = 23
self.x = 23




x = 23
yield Foo(23)

y = [Foo(23)]
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_repr_yield_tuple(self):
        # SETUP
        code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

def bar(x):
    yield x, Foo(x)

y = list(bar(23))
"""
        expected_report = """\

x = 23
self.x = 23




x = 23
yield 23, Foo(23)

y = [(23, Foo(23))]
"""
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_repr_lambda(self):
        # SETUP
        code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

y = list(map(lambda n: Foo(n), range(2)))
"""
        expected_report = """\

x = 0      | x = 1
self.x = 0 | self.x = 1




(0 => Foo(0)) | (1 => Foo(1)) | y = [Foo(0), Foo(1)] 
"""
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
(10 => 11)
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
done()  # alias for mainloop()
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

    def test_print_in_function(self):
        # SETUP
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

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_print_in_loop(self):
        # SETUP
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
sys.stdout.write('x')
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
sys.stderr.write('x')
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

    def test_assign_generator_unpacked(self):
        # SETUP
        code = """\
a, b = (3*i for i in range(2))
"""
        expected_report = """\
(a, b) = (0, 3) """
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

    def test_assign_generator_assignment(self):
        """ Can't convert to tuple for more than one assignment. """
        # SETUP
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
        expected_report = """\



"""
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

    @skipIf(sys.version_info < (3, 0),
            'starred assignment not supported before Python 3.0.')
    def test_assign_starred(self):
        # SETUP
        code = """\
a, *b = (1, 2, 3)
print(b)
"""
        expected_report = """\
(a, *b) = (1, 2, 3)
print('[2, 3]') """
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

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

    @skipIf(sys.version_info < (3, 3),
            'yield from not supported before Python 3.3.')
    def test_yield_from(self):
        # SETUP
        code = """\
def foo(n):
    yield 10 + n
    yield 20 + n

def bar():
    for i in range(2):
        yield from foo(i)

for x in bar():
    pass
"""
        expected_report = """\
n = 0    | n = 1
yield 10 | yield 11
yield 20 | yield 21


i = 0               | i = 1
yield 10 | yield 20 | yield 11 | yield 21

x = 10 | x = 20 | x = 11 | x = 21
       |        |        |
"""

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
print('Bob--23') """

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
print('Bob 23') """

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_display_star_args(self):
        code = """\
def foo(*args):
    pass

foo('Bob', 1)
"""
        expected_report = """\
args = ('Bob', 1)


"""

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_display_kwargs(self):
        code = """\
def foo(**kwargs):
    pass

foo(name='Bob')
"""
        expected_report = """\
kwargs = {'name': 'Bob'}


"""

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

    def test_random(self):
        code = """\
from random import randint

i = randint(1, 100)
j = randint(1, 100)
"""
        if sys.version_info < (3, 0):
            expected_report = """\


i = 85
j = 76
"""
        else:
            expected_report = """\


i = 50
j = 98
"""

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_builtins(self):
        code = """\
x = __builtins__.sum([1, 2])
"""
        expected_report = """\
x = 3
"""

        report = CodeTracer().trace_code(code)

        self.assertReportEqual(expected_report, report)

    def test_module_in_package(self):
        code = """\
from unittest import TestCase


class FooTest(TestCase):
    def test_foo(self):
        s = __package__
        self.assertEqual('example_package', s)
"""
        expected_report = """\





s = 'example_package'
"""

        report = CodeTracer().trace_code(code,
                                         load_as='example_package.foo',
                                         is_module=True,
                                         driver=['unittest',
                                                 'example_package.foo'])

        self.assertReportEqual(expected_report, report)

    def test_module_is_own_driver(self):
        code = """\
s = __package__
s = __name__
"""
        expected_report = """\
s = 'example_package'
s = '__live_coding__'
"""

        report = CodeTracer().trace_code(code,
                                         load_as='example_package.foo',
                                         is_module=True,
                                         driver=['example_package.foo'])

        self.assertReportEqual(expected_report, report)


class FileSwallowerTest(ReportTestCase):
    def test_temp_file(self):
        expected_contents = 'before\nafter\n'

        with TemporaryFile('w+') as real_file:
            real_file.write('before\n')

            swallower = FileSwallower(real_file)
            swallower.write('line 1\n')
            swallower.write('line 2\n')

            real_file.write('after\n')
            real_file.seek(0)
            real_contents = real_file.read()

        self.assertEqual(expected_contents, real_contents)

    def test_buffer(self):
        expected_contents = 'before\nafter\n'

        with TemporaryFile('w+') as real_file:
            real_file.write('before\n')

            swallower = FileSwallower(real_file)
            buffer = getattr(swallower, 'buffer', swallower)
            buffer.write(b'line 1\n')
            buffer.write(b'line 2\n')

            real_file.write('after\n')
            real_file.seek(0)
            real_contents = real_file.read()

        self.assertEqual(expected_contents, real_contents)

    def test_report(self):
        source = """\
import example_printing
__live_coding_context__ = globals()['report_builder']  # variable name is important!
example_printing.custom_print('42', 'xyz')
"""
        expected_report_python2 = """\


print '42xyz'
"""
        expected_report_python3 = """\


print('42xyz')
"""
        expected_report = (expected_report_python3
                           if version_info.major >= 3
                           else expected_report_python2)
        report_builder = ReportBuilder()
        environment = dict(report_builder=report_builder)
        swallower = FileSwallower(sys.stdout)

        with patch('sys.stdout', swallower):
            exec(source, environment, environment)
        report = report_builder.report()

        self.assertReportEqual(expected_report, report)

    def test_long_values(self):
        code="""\
y = [x for x in range(1,100)]
"""
        expected_report = """\
y = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, [312 chars]90, 91, 92, 93, 94, 95, 96, 97, 98, 99]
"""
        report = CodeTracer().trace_code(code)
        self.assertReportEqual(expected_report, report)
