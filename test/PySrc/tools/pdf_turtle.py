from io import BytesIO

from reportlab.graphics.shapes import Drawing
from reportlab.pdfgen.canvas import Canvas
from svglib.svglib import svg2rlg

from svg_turtle import SvgTurtle

original_clip_path = Canvas.clipPath


# noinspection PyUnusedLocal,PyPep8Naming
def patched_clip_path(canvas,
                      aPath,
                      stroke=1,
                      fill=0,
                      fillMode=None):
    stroke = 0
    # noinspection PyArgumentList
    return original_clip_path(canvas, aPath, stroke, fill, fillMode)


class PdfTurtle(SvgTurtle):
    """ Helper class to include turtle graphics within a PDF document. """

    @classmethod
    def create(cls, width="400px", height="250px", patch=True) -> "PdfTurtle":
        """ Create a PdfTurtle object to include in a PDF story.

        :param width: a string with units, or an int number of pixels
        :param height: a string with units, or an int number of pixels
        :param patch: True if you want to monkeypatch the clip path to avoid
        drawing a rectangle around the graphics. (Works around bug report:
        https://github.com/deeplook/svglib/issues/238 )
        """
        if patch:
            Canvas.clipPath = patched_clip_path
        svg_drawing = cls.create_svg(width, height)
        turtle = cls(svg_drawing)
        return turtle

    def to_reportlab(self) -> Drawing:
        svg_text = self.to_svg()
        svg_bytes = svg_text.encode()
        drawing = svg2rlg(BytesIO(svg_bytes))
        return drawing
