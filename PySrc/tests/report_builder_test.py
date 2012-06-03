import unittest

from report_builder import ReportBuilder

class ReportBuilderTest(unittest.TestCase):
    def test_assign(self):
        # SETUP
        expected_report = """\

x = 5 
"""
        
        # EXEC
        builder = ReportBuilder()
        builder.assign(name='x', value=5, line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_return(self):
        # SETUP
        expected_report = """\

return 'bob' 
"""
        
        # EXEC
        builder = ReportBuilder()
        builder.return_value(value='bob', line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_multiple_lines(self):
        # SETUP
        expected_report = """\
a = 3 

b = 'xyz' """
        
        # EXEC
        builder = ReportBuilder()
        builder.assign(name='a', value=3, line_number=1)
        builder.assign(name='b', value="xyz", line_number=3)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_multiple_messages(self):
        # SETUP
        expected_report = """\
x = 1 y = 2 """
        
        # EXEC
        builder = ReportBuilder()
        builder.assign(name='x', value=1, line_number=1)
        builder.assign(name='y', value=2, line_number=1)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_multiple_visits(self):
        # SETUP
        expected_report = """\
x = 'first'    | x = 'second' 
y = 'continue' | """
        
        # EXEC
        builder = ReportBuilder()
        builder.start_block(1, 2)
        builder.assign(name='x', value='first', line_number=1)
        builder.assign(name='y', value='continue', line_number=2)
        builder.start_block(1, 2)
        builder.assign(name='x', value='second', line_number=1)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_nested_blocks(self):
        # SETUP
        expected_report = """\
x = 'first' | x = 'second' 
            | y = 1 | y = 2 """
        
        # EXEC
        builder = ReportBuilder()
        builder.start_block(1, 2)
        builder.assign(name='x', value='first', line_number=1)
        builder.start_block(1, 2)
        builder.assign(name='x', value='second', line_number=1)
        builder.start_block(2, 2)
        builder.assign(name='y', value=1, line_number=2)
        builder.start_block(2, 2)
        builder.assign(name='y', value=2, line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_separate_scopes(self):
        # SETUP
        expected_report = """\
x = 'child' | x = 'child again' 

y = 'main' """
        
        # EXEC
        builder = ReportBuilder()
        builder.assign(name='y', value='main', line_number=3)
        builder.start_block(1, 1)
        builder.assign(name='x', value='child', line_number=1)
        builder.start_block(1, 1)
        builder.assign(name='x', value='child again', line_number=1)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

if __name__ == '__main__':
    unittest.main()
