from sys import version_info
import unittest

from code_tracer import CodeTracer
from mock_turtle import MockTurtle
from report_builder_test import ReportTestCase


class CodeTracerTest(ReportTestCase):
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
a[1:4] = [20, 30]
b = a
a[2:] = [300]
b = a
a[:2] = [2000]
b = a
"""
        expected_report = """\
a = [1, 2, 3, 4, 5]
a[1:4] = [20, 30]
b = [1, 20, 30, 5]
a[2:] = [300]
b = [1, 20, 300]
a[:2] = [2000]
b = [2000, 300] """
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
return None

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
#        self.maxDiff = None
#        self.assertEqual([], tracer.log)
        self.assertReportEqual(expected_report, report)

    def test_runtime_error(self):
        # SETUP
        code = """\
x = 2
x = 1/0
"""
        expected_report_python2 = """\
x = 2
ZeroDivisionError: integer division or modulo by zero """
        expected_report_python3 = """\
x = 2
ZeroDivisionError: division by zero """
        expected_report = (expected_report_python3
                           if version_info.major >= 3
                           else expected_report_python2)
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_multiline_error(self):
        # SETUP
        code = """\
quality = ''
for c in ['a',
          None]:
    quality += c
"""
        expected_report = """\
quality = ''
c = 'a'       | c = None
              |
quality = 'a' | TypeError:"""
        if version_info.major >= 3:
            expected_report += (
                " Can't convert 'NoneType' object to str implicitly")
        else:
            expected_report += (
                " cannot concatenate 'str' and 'NoneType' objects")
        tracer = CodeTracer()

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_unwinding_exceptions(self):
        # SETUP
        code = """\
def foo(n):
    return n/0

x = foo(5)
"""
        expected_report_python2 = """\
n = 5
ZeroDivisionError: integer division or modulo by zero

ZeroDivisionError: integer division or modulo by zero """
        expected_report_python3 = """\
n = 5
ZeroDivisionError: division by zero

ZeroDivisionError: division by zero """
        expected_report = (expected_report_python3
                           if version_info.major >= 3
                           else expected_report_python2)

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
        self.maxDiff = None
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
        self.maxDiff = None
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
        self.maxDiff = None
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




RuntimeError: live coding message limit exceeded """
        tracer = CodeTracer()
        tracer.message_limit = 3

        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.maxDiff = None
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
        self.maxDiff = None
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
        self.maxDiff = None
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
        self.maxDiff = None
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
        self.maxDiff = None
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

    def test_trace_canvas(self):
        # SETUP
        code = """\
from turtle import *

d = 100
forward(d)
"""
        expected_report = """\


d = 100
"""
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
def f():
    yield 1
    yield 2

r = f()
x = next(r)
y = next(r)
#z = next(r) #Don't complete the iterator, so the function never exits.
n = x
"""

        expected_report = """\

yield 1
yield 2


x = 1
y = 2

n = 1
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
z = 1/0
"""
        expected_report_python2 = """\
x = ''

y = 2
ZeroDivisionError: integer division or modulo by zero """
        expected_report_python3 = """\
x = ''

y = 2
ZeroDivisionError: division by zero """
        expected_report = (expected_report_python3
                           if version_info.major >= 3
                           else expected_report_python2)
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

if __name__ == '__main__':
    unittest.main()
