import pytest

from space_tracer.main import TraceRunner
from test_live_image import replace_image

try:
    import PIL
except ImportError:
    PIL = None

REPORT_IMAGE_CREATED = """\
create_image
    0
    0
    image='...'
"""


@pytest.fixture(name='is_pillow_cleared')
def clear_pillow():
    """
    We used to clear the pillow state between tests.
    Now, we just check that pillow is installed.
    """
    if PIL is None:
        pytest.skip('Pillow is not installed.')
    yield True


def test_image_show_show(is_pillow_cleared):
    code = """\
from PIL import Image, ImageShow
im = Image.new( mode = "RGB", size = (256, 256), color = (209, 123, 193) )
ImageShow.show(im)
"""
    tracer = TraceRunner()
    report = tracer.trace_turtle(code)
    assert REPORT_IMAGE_CREATED == replace_image(report)


def test_image_show(is_pillow_cleared):
    code = """\
from PIL import Image
im = Image.new( mode = "RGB", size = (256, 256), color = (209, 123, 193) )
im.show(im)
"""
    tracer = TraceRunner()
    report = tracer.trace_turtle(code)
    assert REPORT_IMAGE_CREATED == replace_image(report)
