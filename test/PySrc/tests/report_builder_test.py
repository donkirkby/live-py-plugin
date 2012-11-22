import unittest

from report_builder import ReportBuilder

class ReportBuilderTest(unittest.TestCase):
    def test_assign(self):
        # SETUP
        expected_value = 5
        expected_report = """\

x = 5 
"""
        
        # EXEC
        builder = ReportBuilder()
        value = builder.assign(name='x', value=expected_value, line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        self.assertEqual(expected_value, value)

    def test_assign_object_without_repr(self):
        # SETUP
        expected_report = ''
        class class_without_repr(object):
            pass
        
        # EXEC
        builder = ReportBuilder()
        builder.assign(name='x', value=class_without_repr(), line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_assign_function(self):
        # SETUP
        expected_report = ''
        f = lambda n: n + 1
        
        # EXEC
        builder = ReportBuilder()
        builder.assign(name='f', value=f, line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_return(self):
        # SETUP
        expected_value = 'bob'
        expected_report = """\

return 'bob' 
"""
        
        # EXEC
        builder = ReportBuilder()
        value = builder.return_value(value=expected_value, line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        self.assertEqual(expected_value, value)

    def test_call(self):
        # SETUP
        expected_report = """\

a = [1, 2] 
"""
        expected_result = 23
        
        # EXEC
        builder = ReportBuilder()
        result = builder.record_call(name='a', 
                                     display_before='[2, 1]',
                                     result=expected_result, 
                                     display_after='[1, 2]', 
                                     line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        self.assertEqual(expected_result, result)

    def test_call_no_change(self):
        # SETUP
        expected_report = ""
        expected_result = 23
        
        # EXEC
        builder = ReportBuilder()
        result = builder.record_call(name='a', 
                                     display_before='[1, 2]',
                                     result=expected_result, 
                                     display_after='[1, 2]', 
                                     line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())
        self.assertEqual(expected_result, result)

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

    def test_frames(self):
        # SETUP
        expected_report = """\
i = 1 | i = 2 
n = 2 | """
        
        # EXEC
        builder = ReportBuilder()
        frame1 = builder.start_frame(1, 2)
        frame1.assign(name='i', value=1, line_number=1)
        frame2 = builder.start_frame(1, 2)
        frame2.assign(name='i', value=2, line_number=1)
        frame1.assign(name='n', value=2, line_number=2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

    def test_frames_with_extra_message(self):
        # SETUP
        expected_report = """\
i = 1 | i = 2 
      | extra message"""
        
        # EXEC
        builder = ReportBuilder()
        frame1 = builder.start_frame(1, 2)
        frame1.assign(name='i', value=1, line_number=1)
        frame2 = builder.start_frame(1, 2)
        frame2.assign(name='i', value=2, line_number=1)
        builder.add_extra_message('extra message', 2)
        report = builder.report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report.splitlines())

if __name__ == '__main__':
    unittest.main()
