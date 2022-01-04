import pytest
from space_tracer.canvas import Canvas
from space_tracer.main import TraceRunner
from space_tracer.mock_turtle import MockTurtle
import turtle


@pytest.fixture
def patched_turtle():
    MockTurtle.monkey_patch()
    yield
    MockTurtle.remove_monkey_patch()


def test_is_patched():
    assert not MockTurtle.is_patched()

    MockTurtle.monkey_patch()
    try:
        assert MockTurtle.is_patched()
    finally:
        MockTurtle.remove_monkey_patch()

    assert not MockTurtle.is_patched()


def test_patch_twice():
    MockTurtle.monkey_patch()
    try:
        with pytest.raises(RuntimeError,
                           match=r'MockTurtle is already monkey patched\.'):
            MockTurtle.monkey_patch()
    finally:
        MockTurtle.remove_monkey_patch()


def test_remove_patch_too_soon():
    with pytest.raises(RuntimeError,
                       match=r'MockTurtle is not monkey patched\.'):
        MockTurtle.remove_monkey_patch()


def test_forward(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_right(patched_turtle):
    expected_report = """\
create_line
    0
    0
    0
    100
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.right(90)
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_reset(patched_turtle):
    expected_report = """\
create_line
    0
    0
    0
    100
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.color('blue')
    t.fd(100)
    t.reset()
    t.right(90)
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_penup(patched_turtle):
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
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.fd(100)
    t.penup()
    t.fd(50)
    t.pendown()
    t.fd(200)
    report = t.report

    assert report == expected_report.splitlines()


def test_mode(patched_turtle):
    expected_report = """\
create_line
    0
    0
    0
    -100
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.getscreen().mode('logo')
    t.forward(100)

    report = t.report

    assert report == expected_report.splitlines()


def test_dot(patched_turtle):
    """ Draw dot with the rounded ends of a zero-length line. """
    expected_report = """\
create_line
    0
    0
    0
    0
    fill='black'
    pensize=5"""

    t = MockTurtle()
    t.dot()
    report = t.report

    assert report == expected_report.splitlines()


@pytest.mark.parametrize('image',
                         ['NotAMultipleOf4',
                          'NowAMultipleOf4=',
                          'ABufferThatIsLongEnoughButHasNoPNGHeader'])
def test_display_image_bad_image(patched_turtle, image):
    image_start = image[:10] + '...'

    with pytest.raises(ValueError, match="Invalid image: " + image_start):
        MockTurtle.display_image(image, align='top')


def test_bounds():
    expected_width = 800
    expected_height = 600

    t = MockTurtle(canvas=Canvas(expected_width, expected_height))
    width = t.getscreen().window_width()
    height = t.getscreen().window_height()
    size = t.getscreen().screensize()

    assert width == expected_width
    assert height == expected_height
    assert size == (expected_width, expected_height)


def test_bounds_after_monkey_patch():
    expected_width = 300
    expected_height = 200

    MockTurtle.monkey_patch(canvas=Canvas(expected_width, expected_height))
    try:
        width = turtle.window_width()
        height = turtle.window_height()
        size = turtle.screensize()
    finally:
        MockTurtle.remove_monkey_patch()

    assert width == expected_width
    assert height == expected_height
    assert size == (expected_width, expected_height)


def test_offset():
    assert not MockTurtle.is_patched()
    expected_report = """\
create_line
    400
    300
    500
    300
    fill='black'
    pensize=1
"""

    t = MockTurtle(canvas=Canvas(800, 600))
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_scale(patched_turtle):
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

    t = MockTurtle(canvas=Canvas())
    t.screen.xscale = 100.0
    t.screen.yscale = 50
    t.fd(1)
    t.right(90)
    t.fd(3)
    report = t.report

    assert report == expected_report.splitlines()


def test_offset_with_scale():
    """ The offset is applied BEFORE the scale. """

    assert not MockTurtle.is_patched()
    expected_report = """\
create_line
    400
    300
    500
    300
    fill='black'
    pensize=1
"""

    t = MockTurtle(canvas=Canvas(800, 600))
    t.screen.xscale = 100
    t.fd(1)
    report = t.report

    assert report == expected_report.splitlines()


def test_repr():
    expected_text = "MockTurtle(100, 0, 10)"

    t = MockTurtle(25, 0, -7)
    t.left(7)
    t.fd(75)
    t.left(10)
    text = repr(t)

    assert text == expected_text


def test_write(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='sw'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    t.write('Bob')
    report = t.report

    assert report == expected_report.splitlines()


def test_write_center(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='s'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    t.write('Bob', align='center')
    report = t.report

    assert report == expected_report.splitlines()


def test_write_right(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='se'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    t.write('Bob', align='right')
    report = t.report

    assert report == expected_report.splitlines()


def test_write_font(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='sw'
    fill='black'
    font=('Courier', 14, 'bold')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    t.write('Bob', font=('Courier', 14, 'bold'))
    report = t.report

    assert report == expected_report.splitlines()


def test_write_partial_font(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='sw'
    fill='black'
    font=('Courier', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    # noinspection PyTypeChecker
    t.write('Bob', font=('Courier',))
    report = t.report

    assert report == expected_report.splitlines()


def test_write_font_string(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='sw'
    fill='black'
    font=('Courier', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    # noinspection PyTypeChecker
    t.write('Bob', font='Courier')
    report = t.report

    assert report == expected_report.splitlines()


def test_write_font_list(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='sw'
    fill='black'
    font=('Courier', 14, 'italic')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    # noinspection PyTypeChecker
    t.write('Bob', font=['Courier', 14, 'italic'])
    report = t.report

    assert report == expected_report.splitlines()


def test_write_font_none(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
create_text
    99
    0
    anchor='sw'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
    # noinspection PyTypeChecker
    t.write('Bob', font=None)
    report = t.report

    assert report == expected_report.splitlines()


def test_write_move():
    """ Not supported yet """
    t = MockTurtle()
    with pytest.raises(NotImplementedError):
        t.write('Bob', move=True)


def test_write_bad_size():
    t = MockTurtle()
    with pytest.raises(ValueError):
        # noinspection PyTypeChecker
        t.write('Bob', font=('Arial', 'eight', 'normal'))


def test_color(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='#ff0080'
    pensize=1"""

    t = MockTurtle()
    t.color(1.0, 0.0, 0.5)
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_color_name(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='#0000ff'
    pensize=1"""

    t = MockTurtle()
    t.color('blue')
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_color_bad(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='#000000'
    pensize=1"""

    t = MockTurtle()
    # noinspection PyTypeChecker
    t.color((1.0, 0.0))  # Only two numbers, fails to black.
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_color_bad_range(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='#000000'
    pensize=1"""

    t = MockTurtle()
    t.color(1.0, 0.0, 1.5)  # Over 1.0 not allowed, fails to black.
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_pen_dict(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='#0000ff'
    pensize=1"""

    t = MockTurtle()
    t.pen(pencolor=(0, 0, 1.0))
    t.fd(100)
    report = t.report

    assert report == expected_report.splitlines()


def test_get_color_names():
    t = MockTurtle()
    t.color('blue')
    color = t.color()

    # Before Python 3.6, dicts were not ordered, and either name was possible.
    assert color in (('blue', 'blue'), ('blue1', 'blue1'))


def test_get_color_rgb():
    t = MockTurtle()
    expected_color = (1.0, 0.0, 0.5)
    t.color(expected_color)
    color = t.color()

    assert color == (expected_color, expected_color)


def test_get_default_color():
    t = MockTurtle()
    color = t.color()

    assert color == ('black', 'black')


def test_bgcolor(patched_turtle):
    expected_report = """\
bgcolor
    fill='#008000'
    outline=''
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1"""

    t = MockTurtle()
    t.fd(100)
    color1 = t.screen.bgcolor()
    t.screen.bgcolor('green')
    color2 = t.screen.bgcolor()
    report = t.report

    assert report == expected_report.splitlines()
    assert color1 == 'white'
    assert color2 == 'green'


def test_clearscreen(patched_turtle):
    expected_report = """\
create_line
    0
    0
    20
    0
    fill='black'
    pensize=1"""

    t = MockTurtle()
    for i in range(4):
        t.pensize(i+1)  # force new line
        t.fd(100)
        t.left(90)
    t.getscreen().clear()

    t2 = MockTurtle()
    t2.fd(20)
    report = t2.report

    assert report == expected_report.splitlines()
    assert not any(item['deleted']
                   for item in t.getscreen().cv.items)


def test_anonymous_turtle_after_clearscreen():
    source = """\
import turtle as t

for i in range(5):
    t.pensize(i+1)
    t.forward(20)

t.clearscreen()
t.forward(30)
"""
    expected_report = """\
create_line
    0
    0
    30
    0
    fill='black'
    pensize=1"""

    runner = TraceRunner()
    report = runner.trace_turtle(source)

    assert report == expected_report


def test_fill(patched_turtle):
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

    t = MockTurtle()
    t.color('red', 'blue')
    t.begin_fill()
    for _ in range(2):
        t.fd(100)
        t.right(90)
    t.end_fill()
    report = t.report

    assert report == expected_report.splitlines()


def test_is_filling():
    t = MockTurtle()
    is_filling1 = t.filling()
    t.begin_fill()
    is_filling2 = t.filling()
    t.end_fill()
    is_filling3 = t.filling()

    assert not is_filling1
    assert is_filling2
    assert not is_filling3


def test_forgotten_end_fill(patched_turtle):
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

    t = MockTurtle()
    t.color('red', 'blue')
    t.begin_fill()
    for _ in range(2):
        t.fd(100)
        t.right(90)
    report = t.report

    assert report == expected_report.splitlines()


def test_stamp(patched_turtle):
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
    fill='black'
    outline=''
create_line
    0
    0
    -9
    -5
    fill='black'
    pensize=1
create_line
    -9
    -5
    -7
    0
    fill='black'
    pensize=1
create_line
    -7
    0
    -9
    5
    fill='black'
    pensize=1
create_line
    -9
    5
    0
    0
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.stamp()
    report = t.report

    assert report == expected_report.splitlines()


def test_clearstamp(patched_turtle):
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
    fill='black'
    outline=''
create_line
    0
    0
    -9
    -5
    fill='black'
    pensize=1
create_line
    -9
    -5
    -7
    0
    fill='black'
    pensize=1
create_line
    -7
    0
    -9
    5
    fill='black'
    pensize=1
create_line
    -9
    5
    0
    0
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    stamp1 = t.stamp()
    t.up()
    t.forward(100)
    stamp2 = t.stamp()
    t.clearstamp(stamp2)
    report = t.report

    assert report == expected_report.splitlines()
    assert stamp1 != stamp2


def test_forgotten_end_fill_with_stamp(patched_turtle):
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
    fill='black'
    outline=''
create_line
    0
    0
    -9
    -5
    fill='black'
    pensize=1
create_line
    -9
    -5
    -7
    0
    fill='black'
    pensize=1
create_line
    -7
    0
    -9
    5
    fill='black'
    pensize=1
create_line
    -9
    5
    0
    0
    fill='black'
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

    t = MockTurtle()
    t.stamp()
    t.color('red', 'blue')
    t.begin_fill()
    for _ in range(2):
        t.fd(100)
        t.right(90)
    report = t.report

    assert report == expected_report.splitlines()


def test_stamp_while_filling(patched_turtle):
    expected_report = """\
create_polygon
    0
    0
    40
    0
    40
    40
    0
    40
    fill='#008000'
    outline=''
create_line
    0
    0
    40
    0
    fill='black'
    pensize=4
create_line
    40
    0
    40
    40
    fill='black'
    pensize=4
create_line
    40
    40
    0
    40
    fill='black'
    pensize=4
create_polygon
    40
    0
    45
    -9
    40
    -7
    35
    -9
    fill='#008000'
    outline=''
create_line
    40
    0
    45
    -9
    fill='black'
    pensize=1
create_line
    45
    -9
    40
    -7
    fill='black'
    pensize=1
create_line
    40
    -7
    35
    -9
    fill='black'
    pensize=1
create_line
    35
    -9
    40
    0
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.pensize(4)
    t.fillcolor('green')
    t.begin_fill()
    t.forward(40)
    t.right(90)
    t.stamp()
    t.forward(40)
    t.right(90)
    t.forward(40)
    t.end_fill()
    report = t.report

    assert report == expected_report.splitlines()


def test_undo(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""

    t = MockTurtle()
    t.forward(100)
    t.right(90)
    t.forward(100)
    t.undo()
    report = t.report

    assert report == expected_report.splitlines()


def test_monkey_patch_anonymous_turtle(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""

    turtle.fd(100)
    report = MockTurtle.get_all_reports()

    assert report == expected_report.splitlines()


def test_monkey_patch_new_turtle(patched_turtle):
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

    assert report == expected_report.splitlines()


def test_monkey_patch_multiple_turtles(patched_turtle):
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

    assert report == expected_report.splitlines()


def test_bad_attribute():
    t = MockTurtle()

    assert not hasattr(t, 'bogus')


def test_tracer(patched_turtle):
    expected_report = """\
create_line
    0
    0
    100
    0
    fill='black'
    pensize=1
"""
    t = turtle

    default_tracer = t.tracer()
    t.forward(100)
    t.tracer(10)
    t.right(90)
    t.forward(200)

    assert default_tracer == 1
    assert MockTurtle.get_all_reports() == expected_report.splitlines()


def test_update(patched_turtle):
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
    200
    fill='black'
    pensize=1
"""
    t = turtle

    t.forward(100)
    t.tracer(10)
    t.right(90)
    t.forward(200)
    t.update()

    assert MockTurtle.get_all_reports() == expected_report.splitlines()


def test_speed():
    all_speeds = set()
    t = MockTurtle()

    all_speeds.add(t.speed())
    t.speed(4)
    all_speeds.add(t.speed())
    t.speed('normal')
    all_speeds.add(t.speed())
    t.speed(2000)
    all_speeds.add(t.speed())
    t.pen(speed=5)
    all_speeds.add(t.speed())
    t.pen(dict(speed=7))
    all_speeds.add(t.speed())

    assert all_speeds == {0}
