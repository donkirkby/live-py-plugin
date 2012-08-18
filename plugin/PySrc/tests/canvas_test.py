import unittest
from canvas import Canvas

class CanvasTest(unittest.TestCase):

    def test_create_line(self):
        # SETUP
        expected_report = "create_line(1, 2, 100, 200)"
        
        # EXEC
        canvas = Canvas()
        canvas.create_line(1, 2, 100, 200)
        report = canvas.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_multiple_calls(self):
        # SETUP
        expected_report = """\
create_line(1, 2, 100, 200)
create_rectangle(5, 10, 500, 1000)"""
        
        # EXEC
        canvas = Canvas()
        canvas.create_line(1, 2, 100, 200)
        canvas.create_rectangle(5, 10, 500, 1000)
        report = canvas.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)
        
    def test_unknown_method(self):
        # EXEC
        canvas = Canvas()
        with self.assertRaisesRegexp(
                AttributeError, 
                "Canvas instance has no attribute 'create_wirple'"):
            canvas.create_wirple(1, 'floop')

    def test_bounds(self):
        # SETUP
        expected_width = 800
        expected_height = 600
        # EXEC
        canvas = Canvas(expected_width, expected_height)
        width = canvas.cget('width')
        height = canvas.cget('height')
        
        # VERIFY
        self.assertEqual(expected_width, width)
        self.assertEqual(expected_height, height)

if __name__ == '__main__':
    unittest.main()
