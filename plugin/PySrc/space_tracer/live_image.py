import io
import os
import re
import sys
import typing
from abc import ABC, abstractmethod
from base64 import standard_b64encode
from contextlib import contextmanager
from pathlib import Path

from space_tracer.mock_turtle import MockTurtle

try:
    from PIL import Image
    from PIL.ImageFilter import GaussianBlur
    new_image = Image.new
    open_image = Image.open
except ImportError:
    class Image:
        Image = None

    new_image = open_image = GaussianBlur = None


class LiveImage(ABC):
    """ Display an image with live updates on the turtle canvas.

    To display any image format, create a subclass that converts your format to
    PNG bytes. See the Pillow Image version below as an example. If you want to
    be able to edit the image and display diffs, derive from LivePainter.
    """
    Position = typing.Tuple[float, float]  # (x, y)
    Size = typing.Tuple[int, int]  # (width, height)
    Fill = typing.Tuple[int, int, int, int]  # (r, g, b, alpha)

    @abstractmethod
    def convert_to_png(self) -> bytes:
        """ Convert this image to bytes in PNG format.

        Override this method for any subclass to handle an image format.
        """

    def convert_to_painter(self) -> 'LivePainter':
        """ Convert this image to one that can be edited.

        Override this method if you don't want to depend on Pillow.
        """
        if open_image is None:
            raise RuntimeError('Pillow is not installed. Install it, or '
                               'override LivePng.convert_to_painter().')
        png_file = io.BytesIO(self.convert_to_png())
        image = open_image(png_file)
        image.load()
        return LivePillowImage(image)

    def save(self, file_path: Path) -> Path:
        """ Save the image to a file.

        :param file_path: The path to save the file to, without an extension.
        :return: The path of the saved file, with an extension.
        """
        extended_path = file_path.with_suffix('.png')
        extended_path.write_bytes(self.convert_to_png())
        return extended_path

    def display(self, position: Position = None, align: str = 'topleft'):
        """ Display this image on the mock turtle's canvas.

        :param position: (x, y) coordinates on the canvas, or None for the
            top-left corner
        :param align: which point in the image to line up with (x, y) - a
            combination of 'top', 'center', or 'bottom' plus 'left', 'center', or
            'right'. If one of the words is missing, it defaults to 'center'.
        """

        png_bytes = self.convert_to_png()
        b64_bytes = standard_b64encode(png_bytes)
        b64_string = b64_bytes.decode('UTF-8')
        MockTurtle.display_image(b64_string, position, align)


class LivePainter(LiveImage):
    @abstractmethod
    def set_pixel(self, position: LiveImage.Position, fill: LiveImage.Fill):
        """ Set the colour of a pixel.

        :param position: the x and y coordinates of the pixel
        :param fill: the colour as a tuple of (red, green, blue, alpha) where
            all are 0 to 255
        """

    @abstractmethod
    def get_pixel(self, position: LiveImage.Position) -> LiveImage.Fill:
        """ Get the colour of a pixel.

        :param position: the x and y coordinates of the pixel
        :return: the colour as a tuple of (red, green, blue, alpha) where all
            are 0 to 255
        """

    @abstractmethod
    def get_size(self) -> LiveImage.Size:
        """ Get the size of the image.

        :return: (width, height) as a tuple
        """

    def convert_to_painter(self) -> 'LivePainter':
        return self


class LivePng(LiveImage):
    def __init__(self, png_bytes: bytes):
        """ Initialize from bytes in PNG format. """
        self.png_bytes = png_bytes

    def convert_to_png(self) -> bytes:
        """ Convert this image to bytes in PNG format.

        This version is trivial.
        """
        return self.png_bytes


class LivePillowImage(LivePainter):
    def __init__(self, image: Image.Image):
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

    def get_pixel(self, position: LiveImage.Position) -> LiveImage.Fill:
        return self.image.getpixel(position)

    def set_pixel(self, position: LiveImage.Position, fill: LiveImage.Fill):
        self.image.putpixel(position, fill)

    def get_size(self) -> LiveImage.Size:
        return self.image.size


class LiveFigure(LiveImage):
    def __init__(self, figure):
        super().__init__()
        self.figure = figure

    def convert_to_png(self) -> bytes:
        data = io.BytesIO()
        self.figure.savefig(data, format='PNG', dpi=self.figure.dpi)

        return data.getvalue()


class LiveImageDiffer:
    def __init__(self, diffs_path: Path = None, request=None, is_displayed=True):
        # noinspection PySingleQuotedDocstring
        ''' Initialize the object and clean out the diffs path.

        This class requires Pillow to be installed, but you can remove that
        dependency with a subclass that overrides the create_painter() method.

        A good way to use this class is to create a session fixture and regular
        fixture like this:

        @pytest.fixture(scope='session')
        def session_image_differ():
            """ Track all images compared in a session. """
            diffs_path = Path(__file__).parent / 'image_diffs'
            differ = LiveImageDiffer(diffs_path)
            yield differ
            differ.remove_common_prefix()


        @pytest.fixture
        def image_differ(request, session_image_differ):
            """ Pass the current request to the session image differ. """
            session_image_differ.request = request
            yield session_image_differ

        Then each test that needs to compare images can require the image_differ
        fixture.

        :param diffs_path: The folder to write comparison images in, or None
            if you don't want to write any. Will be created if it doesn't exist.
        :param request: The Pytest request fixture, if you want to generate
            default file names based on the current test name.
        :param is_displayed: True if the comparison should be displayed on the
            live canvas.
        '''
        self.diffs_path = diffs_path
        self.request = request
        self.is_displayed = is_displayed
        self.diff = None  # type: typing.Optional[LivePainter]
        self.diff_files = set()  # type: typing.Set[Path]
        self.tolerance = 3
        self.blur_radius = 1
        self.diff_count = 0  # number of mismatched pixels
        self.max_diff = 0  # maximum difference seen between pixel values

        # for all calls to compare
        self.file_prefixes = set()  # type: typing.Set[str]

        # only for files that were written
        self.file_names = []  # type: typing.List[str]

        self.clean_diffs()

    @staticmethod
    def start_painter(size: LiveImage.Size, fill: str = None) -> LivePainter:
        """ Create a painter to use for comparison.

        If Pillow is not installed, subclass LiveImageDiffer, and override this
        method to create a subclass of LivePainter. If you need to clean up
        painters, override end_painters().

        :param size: the size of painter to create.
        :param fill: the background colour, defaults to transparent black.
        """
        if new_image is None:
            raise RuntimeError('Pillow is not installed. Install it, or '
                               'override LiveImageDiffer.start_painter().')
        return LivePillowImage(new_image('RGBA', size, fill))

    def end_painters(self, *painters: LivePainter):
        """ Clean up painters created by start_painter.

        :param painters: painters to clean up.
        """

    @contextmanager
    def create_painters(self,
                        size: LiveImage.Size,
                        fill: str = None) -> typing.ContextManager[
            typing.Tuple[LivePainter, LivePainter]]:
        """ Create two painters, then compare them when the context closes.

        Also display the first painter as the Actual image, the second painter
        as the Expected image, and a difference between them. If Pillow isn't
        installed, you must override the helper method, create_painter().
        :param size: the size of the painters to create
        :param fill: the background colour, defaults to transparent black.
        """
        painters = [self.start_painter(size, fill)]
        try:
            painters.append(self.start_painter(size, fill))
            actual, expected = painters
            try:
                yield actual, expected
            except Exception:
                t = MockTurtle()
                t.display_error()
                raise
            self.assert_equal(actual, expected)
        finally:
            self.end_painters(*painters)

    def start_diff(self, size: LiveImage.Size):
        """ Start the comparison by creating a diff painter.

        Overrides must set self.diff to a LivePainter object.
        :param size: the size of painter to put in self.diff.
        """
        self.diff = self.start_painter(size)

    def end_diff(self) -> LiveImage:
        """ End the comparison by cleaning up.

        :return: the final version of the diff image
        """
        diff = self.diff
        self.diff = None
        self.end_painters(diff)
        return diff

    def compare(self,
                actual: LiveImage,
                expected: LiveImage,
                file_prefix: str = None) -> LiveImage:
        """ Build an image to highlight the differences between two images.

        Also display this image as the Actual image, the other image as the
        Expected image, and a difference between them.
        :param actual: the test image
        :param expected: the image to compare to
        :param file_prefix: base name for debug files to write when images
            aren't equal. Debug files are written when diffs_path is passed to
            __init__() and either request fixture is passed to init or
            file_prefix is passed to this method.
        """
        if self.diffs_path is None:
            if file_prefix is not None:
                raise ValueError('Used file_prefix without diffs_path.')
        else:
            if file_prefix is None and self.request is not None:
                file_prefix = re.sub(r'\W', '-', self.request.node.nodeid)
            if file_prefix in self.file_prefixes:
                raise ValueError('Duplicate file_prefix: {!r}.'.format(
                    file_prefix))
            if file_prefix is not None:
                self.file_prefixes.add(file_prefix)
        painter1 = actual.convert_to_painter()
        painter2 = expected.convert_to_painter()
        blurred1 = self.blur(painter1)
        blurred2 = self.blur(painter2)
        width1, height1 = blurred1.get_size()
        width2, height2 = blurred2.get_size()
        width = max(width1, width2)
        height = max(height1, height2)
        self.diff_count = 0
        self.start_diff((width, height))
        try:
            default_colour = (0, 0, 0, 0)
            for x in range(width):
                for y in range(height):
                    position = (x, y)
                    is_missing = False
                    if x < width1 and y < height1:
                        fill1 = blurred1.get_pixel(position)
                    else:
                        is_missing = True
                        fill1 = default_colour
                    if x < width2 and y < height2:
                        fill2 = blurred2.get_pixel(position)
                    else:
                        is_missing = True
                        fill2 = default_colour
                    diff_fill = self.compare_pixel(fill1, fill2, is_missing)
                    self.diff.set_pixel(position, diff_fill)
            self.display_diff(painter1, painter2)
            if self.diff_count:
                self.write_image(actual, file_prefix, 'actual')
                self.write_image(self.diff, file_prefix, 'diff')
                self.write_image(expected, file_prefix, 'expected')
        finally:
            final_diff = self.end_diff()
        return final_diff

    def assert_equal(self,
                     actual: LiveImage,
                     expected: LiveImage,
                     file_prefix: str = None):
        """ Raise an AssertionError if this image doesn't match the expected.

        Also display this image as the Actual image, the other image as the
        Expected image, and a difference between them.
        :param actual: the test image
        :param expected: the image to compare to
        :param file_prefix: base name for debug files to write when images
            aren't equal. Debug files are written when diffs_path is passed to
            __init__() and either request fixture is passed to init or
            file_prefix is passed to this method.
        """
        __tracebackhide__ = True
        self.compare(actual, expected, file_prefix)
        if self.diff_count == 0:
            return
        if self.diff_count == 1:
            suffix = ''
        else:
            suffix = 's'
        raise AssertionError(f'Images differ by {self.diff_count} '
                             f'pixel{suffix} with a maximum difference of '
                             f'{self.max_diff}.')

    def remove_common_prefix(self):
        common_prefix = os.path.commonprefix(self.file_names)
        if not common_prefix:
            return
        prefix_length = len(common_prefix)
        new_file_names = []
        for old_file_name in self.file_names:
            new_file_name = old_file_name[prefix_length:]
            new_file_names.append(new_file_name)
            old_path = self.diffs_path / old_file_name
            new_path = self.diffs_path / new_file_name
            old_path.rename(new_path)
        self.file_names = new_file_names

    def write_image(self, image: LiveImage, file_prefix: str, suffix: str):
        if file_prefix is None:
            return
        name = file_prefix + '-' + suffix
        path = self.diffs_path / name
        file_path = image.save(path)
        self.file_names.append(str(file_path.relative_to(self.diffs_path)))

    def blur(self, painter: LivePainter) -> LivePainter:
        if self.blur_radius <= 1:
            return painter
        if not isinstance(painter, LivePillowImage):
            raise RuntimeError(f'{painter.__class__.__name__} cannot be '
                               f'blurred. Override LiveImageDiffer.blur().')
        if GaussianBlur is None:
            raise RuntimeError('Pillow is not installed. Install or override '
                               'LiveImageDiffer.blur().')

        # noinspection PyCallingNonCallable
        blurred = painter.image.filter(GaussianBlur(self.blur_radius))
        return LivePillowImage(blurred)

    def clean_diffs(self):
        if self.diffs_path is None:
            return
        self.diffs_path.mkdir(exist_ok=True, parents=True)
        for file in self.diffs_path.iterdir():
            for suffix in ('actual', 'diff', 'expected'):
                if file.stem.endswith(suffix):
                    file.unlink()
                    break

    def compare_pixel(self,
                      actual_pixel: LiveImage.Fill,
                      expected_pixel: LiveImage.Fill,
                      is_missing: bool = False) -> LiveImage.Fill:
        ar, ag, ab, aa = actual_pixel
        er, eg, eb, ea = expected_pixel

        # maximum difference for this pixel's four channels
        max_diff = max(abs(a - b) for a, b in zip(actual_pixel, expected_pixel))

        # maximum difference across the whole image
        self.max_diff = max(self.max_diff, max_diff)

        if max_diff > self.tolerance or is_missing:
            self.diff_count += 1
            # Colour
            dr = 0xff
            dg = (ag + eg) // 5
            db = (ab + eb) // 5

            # Opacity
            da = 0xff
        else:
            # Colour
            dr, dg, db = ar, ag, ab

            # Opacity
            da = aa // 3
        return dr, dg, db, da

    def display_diff(self, actual: LivePainter, expected: LivePainter):
        if not self.is_displayed:
            return
        if not MockTurtle.is_patched():
            return
        t = MockTurtle()
        w = t.getscreen().window_width()
        h = t.getscreen().window_height()
        ox, oy = w / 2, h / 2
        actual_width, actual_height = actual.get_size()
        expected_width, expected_height = expected.get_size()
        diff_width, diff_height = self.diff.get_size()
        text_space = (h - actual_height - diff_height - expected_height)
        text_height = max(20, text_space // 3)
        font = ('Arial', text_height // 2, 'normal')
        t.penup()
        t.goto(-ox, oy)
        t.right(90)
        t.forward(text_height)
        t.write('Actual', font=font)
        actual.display(t.pos())
        t.forward(actual_height)
        t.forward(text_height)
        t.write('Diff ({} pixels)'.format(self.diff_count), font=font)
        self.diff.display(t.pos())
        t.forward(diff_height)
        t.forward(text_height)
        t.write('Expected', font=font)
        expected.display(t.pos())


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
            LivePng(b.getvalue()).display()

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

    # noinspection PyUnresolvedReferences
    pyglet.app.run = run
    # noinspection PyUnresolvedReferences
    pyglet.window.Window = MockPygletWindow
