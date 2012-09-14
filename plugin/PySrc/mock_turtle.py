from turtle import TNavigator, TPen

from canvas import Canvas
from argparse import ArgumentError

class MockTurtle(TNavigator, TPen):
    class _Screen(object):
        def __init__(self, canvas):
            self.cv = canvas
            
    def __init__(self, x=0, y=0, heading=0, canvas=None):
        TNavigator.__init__(self)
        TPen.__init__(self)
        if canvas == None:
            canvas = Canvas()
        self.screen = MockTurtle._Screen(canvas)
        self.__xoff = self.screen.cv.cget('width')/2
        self.__yoff = self.screen.cv.cget('height')/2
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
        kwargs = {}
        if self._pencolor:
            kwargs['fill'] = self._pencolor
        if self._drawing:
            self.screen.cv.create_line(xstart + self.__xoff, 
                                       -ystart + self.__yoff, 
                                       xend + self.__xoff, 
                                       -yend + self.__yoff,
                                       **kwargs)
        self._position = end
    
    def __getattr__(self, name):
        if name == 'report':
            return self.screen.cv.report
        raise AttributeError(name)

    def window_width(self):
        return self.screen.cv.cget('width')

    def window_height(self):
        return self.screen.cv.cget('height')

    def write(self, arg, move=False, align="left", font=("Arial", 8, "normal")):
        if move:
            raise ArgumentError('move', 'Parameter is not supported.')
        if align == 'left':
            anchor = 'sw'
        elif align == 'center':
            anchor = 's'
        elif align == 'right':
            anchor = 'se'
        self.screen.cv.create_text(self.xcor() + self.__xoff, 
                                   -self.ycor() + self.__yoff,
                                   text=str(arg),
                                   anchor=anchor,
                                   font=font)
        
    def _colorstr(self, color):
        """Return color string corresponding to args.

        Argument may be a string or a tuple of three
        numbers corresponding to actual colormode,
        i.e. in the range 0<=n<=colormode.

        If the argument doesn't represent a color,
        just uses black.
        """
        if len(color) == 1:
            color = color[0]
        if isinstance(color, str):
            return color
        try:
            r, g, b = color
        except:
            return 'black'
        r, g, b = [round(255.0*x) for x in (r, g, b)]
        if not ((0 <= r <= 255) and (0 <= g <= 255) and (0 <= b <= 255)):
            return 'black'
        return "#%02x%02x%02x" % (r, g, b)
