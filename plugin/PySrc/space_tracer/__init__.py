import sys

try:
    from .main import main, web_main, traced
    from .about import __title__, __version__, __url__
except (ImportError, SyntaxError):
    if sys.version_info < (3,):
        print('Python 2 is no longer supported.')
        exit(1)


def display_image(image, position=None, align='topleft'):
    """ Display an image on the mock turtle's canvas.

    :param image: either a b64 encoded string of PNG image bytes or a PIL Image
    :param position: (x, y) coordinates on the canvas, or None for the top-left
        corner
    :param align: which point in the image to line up with (x, y) - a
        combination of 'top', 'center', or 'bottom' plus 'left', 'center', or
        'right'. If one of the words is missing, it defaults to 'center'.
    """

    from space_tracer.mock_turtle import MockTurtle

    MockTurtle.display_image(image, position, align)
