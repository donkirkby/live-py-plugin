from turtle import TNavigator, TPen

from canvas import Canvas
from argparse import ArgumentError

class MockTurtle(TNavigator, TPen):
    def __init__(self, x=0, y=0, heading=0, canvas=None):
        TNavigator.__init__(self)
        TPen.__init__(self)
        self.canvas = canvas if canvas else Canvas()
        self.__xoff = self.canvas.cget('width')/2
        self.__yoff = self.canvas.cget('height')/2
        if x or y:
            self.setx(x)
            self.sety(y)
        self.setheading(heading)
    
    def __repr__(self):
        x = round(self.xcor())
        y = round(self.ycor())
        h = round(self.heading())
        return 'MockTurtle(%d, %d, %d)' % (x, y, h)
        
    def _goto(self, end):
        xstart = self.xcor()
        ystart = self.ycor()
        xend, yend = end
        if self._drawing:
            self.canvas.create_line(xstart + self.__xoff, 
                                    -ystart + self.__yoff, 
                                    xend + self.__xoff, 
                                    -yend + self.__yoff)
        self._position = end
    
    def __getattr__(self, name):
        if name == 'report':
            return self.canvas.report
        raise AttributeError(name)

    def window_width(self):
        return self.canvas.cget('width')

    def window_height(self):
        return self.canvas.cget('height')

    def write(self, arg, move=False, align="left", font=("Arial", 8, "normal")):
        if move:
            raise ArgumentError('move', 'Parameter is not supported.')
        if align == 'left':
            anchor = 'SW'
        elif align == 'center':
            anchor = 'S'
        elif align == 'right':
            anchor = 'SE'
        self.canvas.create_text(self.xcor() + self.__xoff, 
                                -self.ycor() + self.__yoff,
                                text=str(arg),
                                anchor=anchor,
                                font=font)
        