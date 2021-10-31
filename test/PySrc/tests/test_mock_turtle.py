import re
import typing

import pytest
from PIL import Image

from space_tracer.canvas import Canvas
from space_tracer.mock_turtle import MockTurtle
from space_tracer import display_image
import turtle

from test_report_builder import trim_report


@pytest.fixture
def patched_turtle():
    MockTurtle.monkey_patch()
    yield
    MockTurtle.remove_monkey_patch()


def replace_image(report):
    report = trim_report(report)
    report = re.sub(r"image='[a-zA-Z0-9+/=]*'", "image='...'", report)
    return report


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
    fill='#000000'
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


def test_dot(patched_turtle):
    t2 = MockTurtle()
    t2.up()
    t2.goto(0, -2.5)
    t2.begin_fill()
    t2.circle(2.5)
    t2.end_fill()
    expected_report = t2.report
    MockTurtle._screen = None

    t = MockTurtle()
    t.dot()
    report = t.report

    assert report == expected_report


def test_display_image(patched_turtle):
    expected_report = """\
create_image
    0
    0
    image='UE5HX0lNQUdFX0RBVEE='
"""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    display_image(image_data)

    report = t.report

    assert report == expected_report.splitlines()


def test_display_image_position(patched_turtle):
    expected_report = """\
create_image
    100
    200
    image='UE5HX0lNQUdFX0RBVEE='
"""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    display_image(image_data, (100, -200))

    report = t.report

    assert report == expected_report.splitlines()


def test_display_image_with_size():
    expected_report = """\
create_image
    0
    0
    image='UE5HX0lNQUdFX0RBVEE='
"""

    MockTurtle.monkey_patch(Canvas(width=200, height=400))
    try:
        t = MockTurtle()
        image_data = b'PNG_IMAGE_DATA'

        display_image(image_data)

        report = t.report
    finally:
        MockTurtle.remove_monkey_patch()

    assert report == expected_report.splitlines()


def test_display_image_position_with_size():
    expected_report = """\
create_image
    110
    180
    image='UE5HX0lNQUdFX0RBVEE='
"""

    MockTurtle.monkey_patch(Canvas(width=200, height=400))
    try:
        t = MockTurtle()
        image_data = b'PNG_IMAGE_DATA'

        display_image(image_data, (10, 20))

        report = t.report
    finally:
        MockTurtle.remove_monkey_patch()

    assert report == expected_report.splitlines()


def test_display_image_not_patched():
    expected_report = ""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    display_image(image_data)

    report = t.report

    assert report == expected_report.splitlines()


def test_display_image_pillow(patched_turtle):
    image = Image.new('RGB', (2, 2))
    expected_report = """\
create_image
    0
    0
    image='...'
"""

    t = MockTurtle()

    display_image(image)

    report = t.report

    assert replace_image(report) == expected_report


@pytest.mark.parametrize('align,position',
                         [('topleft', (100, -200)),
                          ('bottomleft', (100, -220)),
                          ('topright', (110, -200)),
                          ('centerleft', (100, -210)),
                          ('topcenter', (105, -200)),
                          ('top', (105, -200)),
                          ('left', (100, -210))])
def test_display_image_bottom_left(patched_turtle,
                                   align: str,
                                   position: typing.Tuple[int, int]):
    image = Image.new('RGB', (10, 20))
    expected_report = """\
create_image
    100
    200
    image='...'
"""

    t = MockTurtle()

    display_image(image, position, align)

    report = t.report

    assert replace_image(report) == expected_report


def test_display_image_bad_align(patched_turtle):
    image = Image.new('RGB', (10, 20))

    with pytest.raises(ValueError, match="Invalid align: 'topfloop'."):
        display_image(image, align='topfloop')


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
    MockTurtle.remove_monkey_patch()
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

    MockTurtle.remove_monkey_patch()
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
    100
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
    100
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
    100
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
    100
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
    100
    0
    anchor='sw'
    fill='black'
    font=('Courier', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
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
    100
    0
    anchor='sw'
    fill='black'
    font=('Courier', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
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
    100
    0
    anchor='sw'
    fill='black'
    font=('Courier', 14, 'italic')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
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
    100
    0
    anchor='sw'
    fill='black'
    font=('Arial', 8, 'normal')
    text='Bob'
"""

    t = MockTurtle()
    t.fd(100)
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
    is_filling1 = t.fill()
    t.begin_fill()
    is_filling2 = t.fill()
    t.end_fill()
    is_filling3 = t.fill()

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

    t = MockTurtle()
    t.stamp()
    report = t.report

    assert report == expected_report.splitlines()


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
    fill='#000000'
    pensize=4
create_line
    40
    0
    40
    40
    fill='#000000'
    pensize=4
create_line
    40
    40
    0
    40
    fill='#000000'
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
    40
    0
    fill='#008000'
    outline=''
create_line
    40
    0
    45
    -9
    fill='#000000'
    pensize=1
create_line
    45
    -9
    40
    -7
    fill='#000000'
    pensize=1
create_line
    40
    -7
    35
    -9
    fill='#000000'
    pensize=1
create_line
    35
    -9
    40
    0
    fill='#000000'
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


def test_screen_methods_exist():
    """ Test that a couple of methods exist, but don't do anything. """
    t = MockTurtle()

    t.screen.tracer()
    t.screen.update()
