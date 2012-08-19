from turtle import TNavigator, TPen

from canvas import Canvas

class MockTurtle(TNavigator, TPen):
    def __init__(self, canvas=None):
        TNavigator.__init__(self)
        TPen.__init__(self)
        self.canvas = canvas if canvas else Canvas()
        self.__xoff = canvas.cget('width')/2
        self.__yoff = canvas.cget('height')/2
        
    def _goto(self, end):
        start = self._position
        self.canvas.create_line(int(start[0]) + self.__xoff, 
                                -int(start[1]) + self.__yoff, 
                                int(end[0]) + self.__xoff, 
                                -int(end[1]) + self.__yoff)
        self._position = end
    
    def __getattr__(self, name):
        if name == 'report':
            return self.canvas.report
        raise AttributeError(name)

    def window_width(self):
        return self.canvas.cget('width')

    def window_height(self):
        return self.canvas.cget('height')
    