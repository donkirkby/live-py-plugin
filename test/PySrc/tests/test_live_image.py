import re
import typing

import pytest

from space_tracer import LiveImage, LivePillowImage
from space_tracer.canvas import Canvas
from space_tracer.mock_turtle import MockTurtle

# noinspection PyUnresolvedReferences
from test_mock_turtle import patched_turtle
from test_report_builder import trim_report

try:
    from PIL import Image
except ImportError:
    Image = None


def replace_image(report):
    report = trim_report(report)
    report = re.sub(r"image='[a-zA-Z0-9+/=]*'", "image='...'", report)
    return report


def test_display(patched_turtle):
    expected_report = """\
create_image
    0
    0
    image='UE5HX0lNQUdFX0RBVEE='
"""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    LiveImage(image_data).display()

    report = t.report

    assert report == expected_report.splitlines()


def test_display_none():
    with pytest.raises(ValueError, match=r'png_bytes is None\.'):
        LiveImage().display()


def test_display_position(patched_turtle):
    expected_report = """\
create_image
    100
    200
    image='UE5HX0lNQUdFX0RBVEE='
"""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    LiveImage(image_data).display((100, -200))

    report = t.report

    assert report == expected_report.splitlines()


def test_display_with_size():
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

        LiveImage(image_data).display()

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

        LiveImage(image_data).display((10, 20))

        report = t.report
    finally:
        MockTurtle.remove_monkey_patch()

    assert report == expected_report.splitlines()


def test_display_not_patched():
    expected_report = ""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    LiveImage(image_data).display()

    report = t.report

    assert report == expected_report.splitlines()


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_display_pillow_image(patched_turtle):
    # noinspection PyUnresolvedReferences
    image = Image.new('RGB', (2, 2))
    expected_report = """\
create_image
    0
    0
    image='...'
"""

    t = MockTurtle()

    LivePillowImage(image).display()

    report = t.report

    assert replace_image(report) == expected_report


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
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

    LivePillowImage(image).display(position, align)

    report = t.report

    assert replace_image(report) == expected_report


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_display_image_bad_align(patched_turtle):
    image = Image.new('RGB', (10, 20))

    with pytest.raises(ValueError, match="Invalid align: 'topfloop'."):
        LivePillowImage(image).display(align='topfloop')
