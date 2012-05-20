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
j = 0 | j = 1 | j = 2 | 
      | i = 2 | i = 4 | """
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

if __name__ == '__main__':
    unittest.main()