from turtle import TNavigator, TPen
from argparse import ArgumentError

class PdfTurtle(TNavigator, TPen):
    """ Helper class to include turtle graphics within a PDF document.
    
    *NOTE* You must call turtle.penup() after you finish all the turtle 
    graphics. This writes out the last section to the PDF."""
    
    class _Screen(object):
        def __init__(self, canvas):
            self.cv = canvas
            self._path = None
            
    def __init__(self, canvas, frame):
        self._path = None
        self.screen = None
        TNavigator.__init__(self)
        TPen.__init__(self)
        self.screen = PdfTurtle._Screen(canvas)
        self.frame = frame
        self.__xoff = self.window_width()/2
        self.__yoff = self.window_height()/2
        
    def _convert_position(self, position):
        return (position[0] + self.__xoff, position[1] - self.__yoff)
    
    def _goto(self, end):
        if self._drawing and self.screen:
            if self._path is None:
                self._path = self.screen.cv.beginPath()
                self._path.moveTo(*self._convert_position(self._position))
            self._path.lineTo(*self._convert_position(end))
        self._position = end
    
    def _newLine(self, usePos=True):
        """Closes current line item and starts a new one.
        """
        if self._path is not None:
            self.screen.cv.drawPath(self._path)
            self._path = None
        
    def window_width(self):
        return self.frame._width

    def window_height(self):
        return self.frame._height

    def write(self, arg, move=False, align="left", font=("Helvetica", 8, "normal")):
        if move:
            raise ArgumentError('move', 'Parameter is not supported.')
        fontName = font[0]
        is_style_added = False
        for style in font[2].split():
            if style != 'normal':
                if not is_style_added:
                    fontName += '-'
                    is_style_added = True
                fontName += style.capitalize()
        
        x = self.xcor() + self.__xoff
        y = self.ycor() - self.__yoff
        y += font[1] * 0.45
        self.screen.cv.setFont(fontName, font[1])
        if align == 'left':
            self.screen.cv.drawString(x, 
                                      y,
                                      str(arg))
        elif align == 'center':
            self.screen.cv.drawCentredString(x, 
                                             y,
                                             str(arg))
        elif align == 'right':
            self.screen.cv.drawRightString(x, 
                                           y,
                                           str(arg))
