import unittest

from canvas import Canvas
from mock_turtle import MockTurtle

class MockTurtleTest(unittest.TestCase):

    def test_forward(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'"""
        
        # EXEC
        t = MockTurtle()
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_right(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    0
    100
    fill='black'"""
        
        # EXEC
        t = MockTurtle()
        t.right(90)
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_penup(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
create_line
    150
    0
    350
    0
    fill='black'"""
        
        # EXEC
        t = MockTurtle()
        t.fd(100)
        t.penup()
        t.fd(50)
        t.pendown()
        t.fd(200)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_bounds(self):
        # SETUP
        expected_width = 800
        expected_height = 600
        
        # EXEC
        t = MockTurtle(canvas=Canvas(expected_width, expected_height))
        width = t.window_width()
        height = t.window_height()
        
        # VERIFY
        self.assertEqual(expected_width, width)
        self.assertEqual(expected_height, height)

    def test_offset(self):
        # SETUP
        expected_report = """\
create_line
    400
    300
    500
    300
    fill='black'"""

        
        # EXEC
        t = MockTurtle(canvas=Canvas(800, 600))
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_repr(self):
        # SETUP
        expected_text = "MockTurtle(100, 0, 10)"
        
        # EXEC
        t = MockTurtle(25, 0, -7)
        t.left(7)
        t.fd(75)
        t.left(10)
        text = repr(t)
        
        # VERIFY
        self.assertEqual(expected_text, text)

    def test_write(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
create_text
    100
    0
    text='Bob'
    font=('Arial', 8, 'normal')
    anchor='sw'"""
        
        # EXEC
        t = MockTurtle()
        t.fd(100)
        t.write('Bob')
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_color(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#ff0080'"""
        
        # EXEC
        t = MockTurtle()
        t.color(1.0, 0.0, 0.5)
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)
