from turtle import TNavigator, TPen

from canvas import Canvas

class MockTurtle(TNavigator, TPen):
    def __init__(self):
        TNavigator.__init__(self)
        TPen.__init__(self)
        TNavigator.reset(self)
        TPen._reset(self)
#        self._orient = (1, 0)
#        self._position = (0, 0)
        self.canvas = Canvas()
        
    def _goto(self, end):
        start = self._position
        self.canvas.create_line(int(start[0]), 
                                int(start[1]), 
                                int(end[0]), 
                                int(end[1]))
        self._position = end
    
    def __getattr__(self, name):
        if name == 'report':
            return self.canvas.report
        raise AttributeError(name)
