from reportlab.platypus import SimpleDocTemplate, Paragraph
from reportlab.platypus.flowables import Flowable, Spacer
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch

from turtle_example import draw_spiral
from pdf_turtle import PdfTurtle

class TurtleArt(Flowable):
    def wrap(self, availWidth, availHeight):
        self.width = availWidth
        self.height = availHeight
        return (availWidth, availHeight)
        
    def draw(self):
        t = PdfTurtle(self.canv, self._frame)
        draw_spiral(t)

def go():
    doc = SimpleDocTemplate("example.pdf")
    styles = getSampleStyleSheet()
    story = [Paragraph('PDF Example', styles['Title'])]
    text = """\
This is an example of how to use turtle graphics in a PDF document."""
    story.append(Paragraph(text, styles['Normal']))
    story.append(Spacer(1,0.055*inch))
    story.append(TurtleArt())
    doc.build(story)

go()

## Uncomment this to display the PDF after you generate it.
#from subprocess import call
#call(["evince", "example.pdf"])
