import re
import typing

import pytest

from space_tracer import LivePng, LivePillowImage
from space_tracer.canvas import Canvas
from space_tracer.live_image import LiveImageDiffer
from space_tracer.mock_turtle import MockTurtle

# noinspection PyUnresolvedReferences
from test_mock_turtle import patched_turtle
from test_report_builder import trim_report

try:
    from PIL import Image
    from PIL import ImageDraw
except ImportError:
    Image = ImageDraw = None


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

    LivePng(image_data).display()

    report = t.report

    assert report == expected_report.splitlines()


def test_display_position(patched_turtle):
    expected_report = """\
create_image
    100
    200
    image='UE5HX0lNQUdFX0RBVEE='
"""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    LivePng(image_data).display((100, -200))

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

        LivePng(image_data).display()

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

        LivePng(image_data).display((10, 20))

        report = t.report
    finally:
        MockTurtle.remove_monkey_patch()

    assert report == expected_report.splitlines()


def test_display_not_patched():
    expected_report = ""

    t = MockTurtle()
    image_data = b'PNG_IMAGE_DATA'

    LivePng(image_data).display()

    report = t.report

    assert report == expected_report.splitlines()


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_display_pillow_image(patched_turtle):
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


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_live_pillow_pixels():
    default = (0, 0, 0, 0)
    blue = (0, 0, 255, 255)

    image = LivePillowImage(Image.new('RGBA', (10, 20)))
    image.set_pixel((5, 10), blue)
    p1 = image.get_pixel((5, 10))
    p2 = image.get_pixel((6, 10))

    assert p1 == blue
    assert p2 == default


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_differ_compare():
    blue = (0, 0, 255, 255)
    white = (255, 255, 255, 255)
    expected_match = (0, 0, 255, 255//3)
    expected_diff = (255, 255//5, 255*2//5, 255)

    image1 = LivePillowImage(Image.new('RGBA', (10, 20)))
    image1.set_pixel((5, 10), blue)
    image1.set_pixel((6, 10), blue)
    image2 = LivePillowImage(Image.new('RGBA', (10, 20)))
    image2.set_pixel((5, 10), blue)
    image2.set_pixel((6, 10), white)

    differ = LiveImageDiffer()

    diff = differ.compare(image1, image2).convert_to_painter()

    diff_pixel1 = diff.get_pixel((5, 10))
    diff_pixel2 = diff.get_pixel((6, 10))

    assert diff_pixel1 == expected_match
    assert diff_pixel2 == expected_diff
    assert differ.diff_count == 1


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_differ_compare_display(patched_turtle):
    expected_report = """\
create_text
    0
    20
    anchor='sw'
    fill='#000000'
    font=('Arial', 10, 'normal')
    text='Actual'
create_image
    0
    20
    image='...'
create_text
    0
    60
    anchor='sw'
    fill='#000000'
    font=('Arial', 10, 'normal')
    text='Diff (0 pixels)'
create_image
    0
    60
    image='...'
create_text
    0
    100
    anchor='sw'
    fill='#000000'
    font=('Arial', 10, 'normal')
    text='Expected'
create_image
    0
    100
    image='...'
"""

    t = MockTurtle()

    image1 = LivePillowImage(Image.new('RGBA', (10, 20)))
    image2 = LivePillowImage(Image.new('RGBA', (10, 20)))

    differ = LiveImageDiffer()

    differ.compare(image1, image2)

    report = t.report

    assert replace_image(report) == expected_report


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_differ_assert_passes():
    image1 = LivePillowImage(Image.new('RGBA', (10, 20)))
    image2 = LivePillowImage(Image.new('RGBA', (10, 20)))

    differ = LiveImageDiffer()

    differ.assert_equal(image1, image2)


@pytest.mark.skipif(Image is None, reason='Pillow not installed.')
def test_differ_assert_fails():
    blue = (0, 0, 255, 255)

    image1 = LivePillowImage(Image.new('RGBA', (10, 20)))
    image1.set_pixel((5, 10), blue)
    image2 = LivePillowImage(Image.new('RGBA', (10, 20)))

    differ = LiveImageDiffer()

    with pytest.raises(AssertionError):
        differ.assert_equal(image1, image2)
