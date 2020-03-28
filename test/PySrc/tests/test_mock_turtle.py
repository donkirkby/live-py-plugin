import unittest

from space_tracer.canvas import Canvas
from space_tracer.mock_turtle import MockTurtle
import turtle


class MockTurtleTest(unittest.TestCase):
    def tearDown(self):
        MockTurtle.remove_monkey_patch()

    def test_forward(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""

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
    fill='black'
    pensize=1
"""

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
    pensize=1
create_line
    150
    0
    350
    0
    fill='#000000'
    pensize=1
"""

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

    def test_bounds_after_monkey_patch(self):
        # SETUP
        expected_width = 300
        expected_height = 200

        # EXEC
        MockTurtle.monkey_patch(canvas=Canvas(expected_width, expected_height))
        width = turtle.window_width()  # @UndefinedVariable
        height = turtle.window_height()  # @UndefinedVariable

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
    fill='black'
    pensize=1
"""

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
    fill='black'
    pensize=1
create_line
    100
    0
    100
    150
    fill='black'
    pensize=1
"""

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
    fill='black'
    pensize=1
"""

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
    fill='black'
    pensize=1
create_text
    100
    0
    anchor='sw'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

        # EXEC
        t = MockTurtle()
        t.fd(100)
        t.write('Bob')
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_write_center(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    100
    0
    anchor='s'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

        # EXEC
        t = MockTurtle()
        t.fd(100)
        t.write('Bob', align='center')
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_write_right(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    100
    0
    anchor='se'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

        # EXEC
        t = MockTurtle()
        t.fd(100)
        t.write('Bob', align='right')
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_write_move(self):
        """ Not supported yet """
        t = MockTurtle()
        with self.assertRaises(NotImplementedError):
            t.write('Bob', move=True)

    def test_color(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#ff0080'
    pensize=1"""

        # EXEC
        t = MockTurtle()
        t.color(1.0, 0.0, 0.5)
        t.fd(100)
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_color_name(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#0000ff'
    pensize=1"""

        # EXEC
        t = MockTurtle()
        t.color('blue')
        t.fd(100)
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_color_bad(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#000000'
    pensize=1"""

        # EXEC
        t = MockTurtle()
        t.color((1.0, 0.0))  # Only two numbers, fails to black.
        t.fd(100)
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_color_bad_range(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#000000'
    pensize=1"""

        # EXEC
        t = MockTurtle()
        t.color(1.0, 0.0, 1.5)  # Over 1.0 not allowed, fails to black.
        t.fd(100)
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_pen_dict(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#0000ff'
    pensize=1"""

        # EXEC
        t = MockTurtle()
        t.pen(pencolor=(0, 0, 1.0))
        t.fd(100)
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_get_color_names(self):
        t = MockTurtle()
        t.color('blue')
        color = t.color()

        self.assertIn(color, (('blue', 'blue'), ('blue1', 'blue1')))

    def test_get_color_rgb(self):
        t = MockTurtle()
        expected_color = (1.0, 0.0, 0.5)
        t.color(expected_color)
        color = t.color()

        self.assertEqual((expected_color, expected_color), color)

    def test_get_default_color(self):
        t = MockTurtle()
        color = t.color()

        self.assertEqual(('black', 'black'), color)

    def test_bgcolor(self):
        # SETUP
        expected_report = """\
bgcolor
    fill='#00ff00'
    outline=''
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1"""

        # EXEC
        t = MockTurtle()
        t.fd(100)
        color1 = t.screen.bgcolor()
        t.screen.bgcolor('green')
        color2 = t.screen.bgcolor()
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)
        self.assertEqual('white', color1)
        self.assertEqual('green', color2)

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
    fill='#0000ff'
    outline=''
create_line
    0
    0
    100
    0
    fill='#ff0000'
    pensize=1
create_line
    100
    0
    100
    100
    fill='#ff0000'
    pensize=1"""

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

    def test_is_filling(self):
        # SETUP

        # EXEC
        t = MockTurtle()
        is_filling1 = t.fill()
        t.begin_fill()
        is_filling2 = t.fill()
        t.end_fill()
        is_filling3 = t.fill()

        # VERIFY
        self.assertFalse(is_filling1)
        self.assertTrue(is_filling2)
        self.assertFalse(is_filling3)

    def test_forgotten_end_fill(self):
        # SETUP
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='#ff0000'
    pensize=1
create_line
    100
    0
    100
    100
    fill='#ff0000'
    pensize=1
"""

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

    def test_stamp(self):
        # SETUP
        expected_report = """\
create_polygon
    0
    0
    -9
    -5
    -7
    0
    -9
    5
    0
    0
    fill='#000000'
    outline=''
create_line
    0
    0
    -9
    -5
    fill='#000000'
    pensize=1
create_line
    -9
    -5
    -7
    0
    fill='#000000'
    pensize=1
create_line
    -7
    0
    -9
    5
    fill='#000000'
    pensize=1
create_line
    -9
    5
    0
    0
    fill='#000000'
    pensize=1
"""

        # EXEC
        t = MockTurtle()
        t.stamp()
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_forgotten_end_fill_with_stamp(self):
        # SETUP
        expected_report = """\
create_polygon
    0
    0
    -9
    -5
    -7
    0
    -9
    5
    0
    0
    fill='#000000'
    outline=''
create_line
    0
    0
    -9
    -5
    fill='#000000'
    pensize=1
create_line
    -9
    -5
    -7
    0
    fill='#000000'
    pensize=1
create_line
    -7
    0
    -9
    5
    fill='#000000'
    pensize=1
create_line
    -9
    5
    0
    0
    fill='#000000'
    pensize=1
create_line
    0
    0
    100
    0
    fill='#ff0000'
    pensize=1
create_line
    100
    0
    100
    100
    fill='#ff0000'
    pensize=1
"""

        # EXEC
        t = MockTurtle()
        t.stamp()
        t.color('red', 'blue')
        t.begin_fill()
        for _ in range(2):
            t.fd(100)
            t.right(90)
        report = t.report

        # VERIFY
        self.assertEqual(expected_report.splitlines(), report)

    def test_monkey_patch_anonymous_turtle(self):
        MockTurtle.monkey_patch()
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""

        turtle.fd(100)  # @UndefinedVariable
        report = MockTurtle.get_all_reports()

        self.assertEqual(expected_report.splitlines(), report)

    def test_monkey_patch_new_turtle(self):
        MockTurtle.monkey_patch()
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""

        t = turtle.Turtle()
        t.fd(100)
        report = MockTurtle.get_all_reports()

        self.assertEqual(expected_report.splitlines(), report)

    def test_monkey_patch_multiple_turtles(self):
        MockTurtle.monkey_patch()
        expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_line
    100
    0
    100
    100
    fill='black'
    pensize=1
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_line
    100
    0
    100
    -100
    fill='black'
    pensize=1
"""

        t1 = turtle.Turtle()
        t1.begin_fill()
        t1.fd(100)
        t1.right(90)
        t1.fd(100)
        t2 = turtle.Turtle()
        t2.begin_fill()
        t2.fd(100)
        t2.left(90)
        t2.fd(100)
        report = MockTurtle.get_all_reports()

        self.maxDiff = None
        self.assertEqual(expected_report.splitlines(), report)

    def test_bad_attribute(self):
        # SETUP

        # EXEC
        t = MockTurtle()

        # VERIFY
        self.assertFalse(hasattr(t, 'bogus'))

    def test_screen_methods_exist(self):
        # SETUP
        t = MockTurtle()

        # EXEC

        # VERIFY
        t.screen.tracer()
        t.screen.update()
