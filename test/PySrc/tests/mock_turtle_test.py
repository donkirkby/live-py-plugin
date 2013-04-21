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
    pensize=1
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
    pensize=1
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
    pensize=1
    fill='black'
create_line
    150
    0
    350
    0
    pensize=1
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
    pensize=1
    fill='black'"""

        
        # EXEC
        t = MockTurtle(canvas=Canvas(800, 600))
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_scale(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    pensize=1
    fill='black'
create_line
    100
    0
    100
    150
    pensize=1
    fill='black'"""

        
        # EXEC
        t = MockTurtle(canvas=Canvas())
        t.screen.xscale = 100.0
        t.screen.yscale = 50
        t.fd(1)
        t.right(90)
        t.fd(3)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_offset_with_scale(self):
        """ The offset is applied BEFORE the scale. """
        
        # SETUP
        expected_report = """\
create_line
    400
    300
    500
    300
    pensize=1
    fill='black'"""

        
        # EXEC
        t = MockTurtle(canvas=Canvas(800, 600))
        t.screen.xscale = 100 
        t.fd(1)
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
    pensize=1
    fill='black'
create_text
    100
    0
    text='Bob'
    font=('Arial', 8, 'normal')
    anchor='sw'
    fill='black'"""
        
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
    pensize=1
    fill='#ff0080'"""
        
        # EXEC
        t = MockTurtle()
        t.color(1.0, 0.0, 0.5)
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_fill(self):
        # SETUP
        expected_report = """\
create_polygon
    0
    0
    100
    0
    100
    100
    outline=''
    fill='#0000ff'
create_line
    0
    0
    100
    0
    pensize=1
    fill='#ff0000'
create_line
    100
    0
    100
    100
    pensize=1
    fill='#ff0000'"""
        
        # EXEC
        t = MockTurtle()
        t.color('red', 'blue')
        t.begin_fill()
        for _ in range(2):
            t.fd(100)
            t.right(90)
        t.end_fill()
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_forgotten_end_fill(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    pensize=1
    fill='#ff0000'
create_line
    100
    0
    100
    100
    pensize=1
    fill='#ff0000'"""
        
        # EXEC
        t = MockTurtle()
        t.color('red', 'blue')
        t.begin_fill()
        for _ in range(2):
            t.fd(100)
            t.right(90)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)
