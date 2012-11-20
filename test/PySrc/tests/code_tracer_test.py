from sys import version_info
import unittest
from code_tracer import CodeTracer

class CodeTracerTest(unittest.TestCase):

    def test_empty(self):
        # EXEC
        report = CodeTracer().trace_code("")
        expected_report = ""

        # VERIFY        
        self.assertEqual(report, expected_report)

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
        self.assertEqual(report, expected_report)

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_mutable(self):
        # SETUP
        code = """\
a = [1, 2, [3, 4]]
a[0] = 9
a[2][1] = 8
"""
        expected_report = """\
a = [1, 2, [3, 4]] 
a = [9, 2, [3, 4]] 
a = [9, 2, [3, 8]] """
        # EXEC
        report = CodeTracer().trace_code(code)

        # VERIFY        
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
    
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_infinite_loop(self):
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
shelf.counts = {2: 3} """
        tracer = CodeTracer()
        
        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
    def test_trace_canvas(self):
        # SETUP
        code = """\
x = 100
y = 50
__live_canvas__.create_line(x, y, x+10, y+10)
"""
        expected_report = """\
x = 100 
y = 50 """
        tracer = CodeTracer()
        
        # EXEC
        report = tracer.trace_code(code)
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
    def test_canvas(self):
        # SETUP
        code = """\
__live_canvas__.create_line(0, 1, 100, 101)
"""
        expected_report = """\
create_line
    0
    1
    100
    101
"""
        tracer = CodeTracer()
        
        # EXEC
        report = tracer.trace_canvas(code)

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
    def test_turtle(self):
        # SETUP
        code = """\
__live_turtle__.forward(100)
"""
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
"""
        tracer = CodeTracer()
        
        # EXEC
        report = tracer.trace_turtle(code)

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        
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
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_assign_tuple(self):
        # SETUP
        code = """\
b, c = 3, 42
"""
        expected_report = """\
b = 3 c = 42 
"""
        tracer = CodeTracer()
        
        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

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
        self.assertEqual(expected_report.splitlines(), report.splitlines())
"""\
"""
if __name__ == '__main__':
    unittest.main()
