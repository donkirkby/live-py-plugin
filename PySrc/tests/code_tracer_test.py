import unittest
from code_tracer import CodeTracer

class CodeTracerTest(unittest.TestCase):

# Other things to test:
# multiline variable value.
# calling a second function
# imports
# making print work?

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
        expected_report = """\
x = 2 
ZeroDivisionError: integer division or modulo by zero"""
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



IndentationError: expected an indented block"""
        tracer = CodeTracer()
        
        # EXEC
        report = tracer.trace_code(code)

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

if __name__ == '__main__':
    unittest.main()
