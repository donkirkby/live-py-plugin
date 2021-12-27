import unittest
from space_tracer.canvas import Canvas


class CanvasTest(unittest.TestCase):
    def setUp(self):
        if not hasattr(self, 'assertRaisesRegex'):
            self.assertRaisesRegex = self.assertRaisesRegexp

    def test_create_line(self):
        # SETUP
        expected_report = """\
create_line
    1
    2
    100
    200"""
        
        # EXEC
        canvas = Canvas()
        # noinspection PyUnresolvedReferences
        canvas.create_line(1, 2, 100, 200)
        report = canvas.build_report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_multiple_calls(self):
        # SETUP
        expected_report = """\
create_line
    1
    2
    100
    200
create_rectangle
    5
    10
    500
    1000"""
        
        # EXEC
        canvas = Canvas()
        # noinspection PyUnresolvedReferences
        canvas.create_line(1, 2, 100, 200)
        # noinspection PyUnresolvedReferences
        canvas.create_rectangle(5, 10, 500, 1000)
        report = canvas.build_report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)
        
    def test_unknown_method(self):
        # EXEC
        canvas = Canvas()
        with self.assertRaisesRegex(
                AttributeError, 
                "(Canvas instance|'Canvas' object) has no attribute 'create_wirple'"):
            # noinspection PyUnresolvedReferences
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

    def test_create_text(self):
        # SETUP
        expected_report = """\
create_text
    100
    200
    anchor='s'
    font=('Arial', 8, 'normal')
    text='foo'"""
        
        # EXEC
        canvas = Canvas()
        # noinspection PyUnresolvedReferences
        canvas.create_text(100,
                           200, 
                           text='foo', 
                           font=('Arial', 8, 'normal'),
                           anchor='s')
        report = canvas.build_report()
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)


if __name__ == '__main__':
    unittest.main()
