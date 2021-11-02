import io
import sys
import typing
from base64 import standard_b64encode

from space_tracer.mock_turtle import MockTurtle

try:
    from PIL.Image import Image
except ImportError:
    Image = None

try:
    from matplotlib.pyplot import Figure
except ImportError:
    Figure = None


class LiveImage:
    def __init__(self, png_bytes: bytes = None):
        """ Initialize from bytes in PNG format.

        To display any other image format, create a subclass that converts
        your format to PNG bytes. See the PIL Image version just below as an
        example.
        """
        self.png_bytes = png_bytes

    def convert_to_png(self) -> bytes:
        """ Convert this image to bytes in PNG format.

        Override this method for any subclass to handle a different format.
        """
        return self.png_bytes

    def display(self,
                position: typing.Tuple[int, int] = None,
                align: str = 'topleft'):
        """ Display this image on the mock turtle's canvas.


        :param image: PNG image bytes
        :param position: (x, y) coordinates on the canvas, or None for the top-left
            corner
        :param align: which point in the image to line up with (x, y) - a
            combination of 'top', 'center', or 'bottom' plus 'left', 'center', or
            'right'. If one of the words is missing, it defaults to 'center'.
        """

        png_bytes = self.convert_to_png()
        if png_bytes is None:
            raise ValueError('png_bytes is None.')
        b64_bytes = standard_b64encode(png_bytes)
        b64_string = b64_bytes.decode('UTF-8')
        MockTurtle.display_image(b64_string, position, align)


class LivePillowImage(LiveImage):
    def __init__(self, image: Image):
        """ Initialize from a Pillow Image.

        Use this as an example to display a live version of any other image or
        visualization class.
        """
        super().__init__()
        self.image = image

    def convert_to_png(self) -> bytes:
        data = io.BytesIO()
        self.image.save(data, 'PNG')

        return data.getvalue()


class LiveFigure(LiveImage):
    def __init__(self, figure: Figure):
        super().__init__()
        self.figure = figure

    def convert_to_png(self) -> bytes:
        data = io.BytesIO()
        self.figure.savefig(data, format='PNG')

        return data.getvalue()


def monkey_patch_pyglet(canvas):

    pyglet = sys.modules['pyglet']

    # noinspection PyUnresolvedReferences
    class MockPygletWindow(pyglet.window.Window):

        # noinspection PyUnusedLocal
        def __init__(self, **kwargs):
            conf = pyglet.gl.Config(double_buffer=True)
            super(MockPygletWindow, self).__init__(
                config=conf,
                resizable=True,
                visible=False,

                # Let the canvas size dictate the program's window size.
                width=canvas.cget('width'),
                height=canvas.cget('height')
            )
            self.on_resize(self.width, self.height)

        @staticmethod
        def on_draw():
            # Get the colour buffer, write it to a bytearray in png format.
            buf = pyglet.image.get_buffer_manager().get_color_buffer()
            b = io.BytesIO()
            buf.save('buffer.png', b)
            LiveImage(b.getvalue()).display()

    # noinspection PyUnresolvedReferences
    def run():
        for window in list(pyglet.app.windows):
            for i in range(2):
                pyglet.clock.tick()
                window.switch_to()
                window.dispatch_events()
                window.dispatch_event('on_draw')
                window.flip()
            window.close()

    pyglet.app.run = run
    pyglet.window.Window = MockPygletWindow
