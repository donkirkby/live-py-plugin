import unittest
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
