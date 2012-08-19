import unittest

from canvas import Canvas
from mock_turtle import MockTurtle

class MockTurtleTest(unittest.TestCase):

    def test_forward(self):
        # SETUP
        expected_report = "create_line(0, 0, 100, 0)"
        
        # EXEC
        t = MockTurtle()
        t.fd(100)
        report = t.report
        
        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_right(self):
        # SETUP
        expected_report = "create_line(0, 0, 0, 100)"
        
        # EXEC
        t = MockTurtle()
        t.right(90)
        t.fd(100)
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
        expected_report = "create_line(400, 300, 500, 300)"
        
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
