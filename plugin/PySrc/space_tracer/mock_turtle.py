import types
from collections import namedtuple
import io
from base64 import standard_b64encode
import sys

from .canvas import Canvas

try:
    import tkinter as tk
except ImportError:
    tkinter_name = 'tkinter'
    tk = sys.modules[tkinter_name] = types.ModuleType(tkinter_name)

    tk.Frame = tk.Canvas = tk.Tk = object
    tk.mainloop = lambda *args, **kwargs: None

    dialog_name = tkinter_name + '.simpledialog'
    tk.simpledialog = sys.modules[dialog_name] = types.ModuleType(dialog_name)

from turtle import TNavigator, TPen

DEFAULT_FONT = ("Arial", 8, "normal")


class MockTurtle(TNavigator, TPen):
    class _Screen(object):
        def __init__(self, canvas):
            if canvas is None:
                canvas = Canvas()
            self.cv = canvas
            self.xscale = self.yscale = 1
            self._config = {'bgcolor': None}

        def window_width(self):
            return self.cv.cget('width')

        def window_height(self):
            return self.cv.cget('height')

        def screensize(self, canvwidth=None, canvheight=None, bg=None):
            if canvwidth is canvheight is bg is None:
                return self.window_width(), self.window_height()
        
        def bgcolor(self, color=None):
            if color is None:
                bgcolor = self._config['bgcolor']
                if bgcolor is None:
                    return 'white'
                return bgcolor
            self._config['bgcolor'] = color

        def tracer(self, a=None, b=None):
            pass

        def update(self):
            pass

    _Stamp = namedtuple('Stamp', 'pos heading color')
    _screen = _pen = OriginalTurtle = original_mainloop = None
    instances = []

    @classmethod
    def monkey_patch(cls, canvas=None):
        turtle_module = sys.modules['turtle']
        cls.OriginalTurtle = turtle_module.Turtle
        turtle_module.Turtle = MockTurtle
        cls.original_mainloop = turtle_module.mainloop
        turtle_module.mainloop = turtle_module.done = lambda: None
        # noinspection PyProtectedMember
        MockTurtle._screen = MockTurtle._Screen(canvas)
        MockTurtle._pen = MockTurtle()

    @classmethod
    def remove_monkey_patch(cls):
        MockTurtle.instances = []
        if cls.OriginalTurtle is not None:
            turtle_module = sys.modules['turtle']
            turtle_module.Turtle = cls.OriginalTurtle
            turtle_module.mainloop = turtle_module.done = cls.original_mainloop
            MockTurtle._pen = cls.OriginalTurtle = cls.original_mainloop = None
            MockTurtle._screen = None

    @classmethod
    def get_all_reports(cls):
        return MockTurtle._pen.report

    @classmethod
    def display_image(cls, x, y, image):
        MockTurtle._screen.cv.create_image(x, y, image=image)

    def __init__(self, x=0, y=0, heading=0, canvas=None):
        self._path = None
        self._lines_to_draw = []
        TNavigator.__init__(self)
        TPen.__init__(self)
        if MockTurtle._screen is not None:
            self.screen = MockTurtle._screen
        else:
            # noinspection PyProtectedMember
            self.screen = MockTurtle._Screen(canvas)
        self.stamps = []
        self.__xoff = self.screen.cv.cget('width')/2
        self.__yoff = self.screen.cv.cget('height')/2
        if x or y:
            self.up()
            self.setx(x)
            self.sety(y)
            self.down()
        self.setheading(heading)
        MockTurtle.instances.append(self)

    def __repr__(self):
        x = round(self.xcor())
        y = round(self.ycor())
        h = round(self.heading())
        return 'MockTurtle(%d, %d, %d)' % (x, y, h)

    def _convert_position(self, position):
        x, y = position
        return (x*self.screen.xscale + self.__xoff,
                -y*self.screen.yscale + self.__yoff)

    def _goto(self, end):
        xstart, ystart = self._convert_position(self._position)
        xend, yend = self._convert_position(end)
        kwargs = {}
        if self._pencolor:
            kwargs['fill'] = self._pencolor
        if self._pensize:
            kwargs['pensize'] = self._pensize
        if self._drawing:
            args = [xstart, ystart, xend, yend]
            if self._path:
                self._lines_to_draw.append((args, kwargs))
            else:
                self.screen.cv.create_line(*args, **kwargs)
        if self._path:
            self._path.append(xend)
            self._path.append(yend)
        self._position = end

    def reset(self):
        TNavigator.reset(self)
        # noinspection PyUnresolvedReferences,PyProtectedMember
        TPen._reset(self)
        self.screen.cv.report.clear()

    def dot(self, size=None, *color):
        x, y = self._position
        if size is not None:
            diameter = size
        else:
            pensize = self._pensize or 0
            diameter = max(pensize+4, 2*pensize)
        if len(color):
            pencolor = self._colorstr(color)
        else:
            pencolor = self._pencolor or 0
        r = diameter / 2
        t2 = self.__class__(canvas=self.screen.cv)
        t2.up()
        t2.goto(x, y - r)
        t2.fillcolor(pencolor)
        t2.begin_fill()
        t2.circle(r)
        t2.end_fill()

    # noinspection PyProtectedMember
    def __getattr__(self, name):
        if name == 'report':
            for instance in MockTurtle.instances:
                instance._path = None  # Cancel incomplete fill.
                instance._newLine()
                instance._flush_lines()
                instance._draw_stamps()
            report = self.screen.cv.report[:]
            bgcolor = self.screen._config['bgcolor']
            if bgcolor is not None:
                # noinspection PyTypeChecker
                bgcolorstr = self._colorstr(bgcolor)
                report[:0] = ["bgcolor",
                              "    fill={!r}".format(bgcolorstr),
                              "    outline=''"]
            return report
        raise AttributeError(
            "'MockTurtle' object has no attribute {!r}".format(name))

    def _draw_stamps(self):
        if not self.stamps:
            return

        start_pos = self.pos()
        start_heading = self.heading()
        start_pensize = self.pensize()
        start_isdown = self.isdown()
        self.pensize(1)
        stamps = self.stamps[:]
        self.stamps.clear()
        for stamp in stamps:
            self.up()
            self.goto(stamp.pos)
            self.setheading(stamp.heading)
            self.color(*stamp.color)
            self.down()
            self.begin_fill()
            self.left(151)
            self.fd(10.296)
            self.left(140.8)
            self.fd(5.385)
            self.right(43.6)
            self.fd(5.385)
            self.setpos(stamp.pos)
            self.end_fill()
        self.up()
        self.goto(start_pos)
        self.setheading(start_heading)
        self.pensize(start_pensize)
        if start_isdown:
            self.down()

    def getscreen(self):
        return self.screen

    def begin_fill(self):
        self.fill(True)

    def end_fill(self):
        self.fill(False)

    def _flush_lines(self):
        for args, kwargs in self._lines_to_draw:
            self.screen.cv.create_line(*args, **kwargs)

        self._lines_to_draw = []
        self._draw_stamps()

    def fill(self, flag=None):
        if flag is None:
            return self._path is not None
        if self._path and len(self._path) > 2:
            self.screen.cv.create_polygon(*self._path,
                                          fill=self._fillcolor,
                                          outline='')
        self._flush_lines()
        if not flag:
            self._path = None
        else:
            x, y = self._position
            self._path = [x + self.__xoff, -y + self.__yoff]

    def stamp(self):
        self.stamps.append(
            MockTurtle._Stamp(self.pos(), self.heading(), self.color()))
        if not self.fill():
            self._draw_stamps()

    def write(self,
              arg,
              move=False,
              align="left",
              font=DEFAULT_FONT):
        if move:
            raise NotImplementedError('move parameter is not supported.')
        if align == 'left':
            anchor = 'sw'
        elif align == 'center':
            anchor = 's'
        else:
            assert align == 'right'
            anchor = 'se'

        # noinspection PyBroadException
        try:
            if isinstance(font, str):
                font = [font]
            else:
                font = list(font)
            font += DEFAULT_FONT[len(font):]
        except Exception:
            font = list(DEFAULT_FONT)
        font[1] = int(font[1])
        font = tuple(font)

        kwargs = dict(text=str(arg),
                      anchor=anchor,
                      font=font)
        if self._pencolor:
            kwargs['fill'] = self._pencolor
        x, y = self._convert_position(self._position)
        self.screen.cv.create_text(x, y, **kwargs)

    def _update(self, *args, **kwargs):
        if not self._pencolor.startswith('#'):
            self._pencolor = self._colorstr(self._pencolor)
        if not self._fillcolor.startswith('#'):
            self._fillcolor = self._colorstr(self._fillcolor)
        return super(MockTurtle, self)._update(*args, **kwargs)

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
            return color_map.get(color.lower(), color)
        try:
            r, g, b = color
        except ValueError:
            return '#000000'
        r, g, b = [round(255.0*x) for x in (r, g, b)]
        if not ((0 <= r <= 255) and (0 <= g <= 255) and (0 <= b <= 255)):
            return '#000000'
        return "#%02x%02x%02x" % (r, g, b)

    @staticmethod
    def _rgb_value(rgbstr):
        return round(int(rgbstr, 16)/2.55)/100.0

    def _color(self, colorstr):
        """ Reverse lookup of _colorstr. """
        if not colorstr.startswith('#'):
            return colorstr
        itercolors = getattr(color_map, 'iteritems', color_map.items)
        for name, code in itercolors():
            if code == colorstr:
                return name
        return tuple(self._rgb_value(colorstr[2*i+1:2*i+3]) for i in range(3))


# Normally, Tkinter will look up these colour names for you, but we don't
# actually launch Tkinter when we're analysing code.
color_map = {
    'alice blue': '#f0f8ff',
    'aliceblue': '#f0f8ff',
    'antique white': '#faebd7',
    'antiquewhite': '#faebd7',
    'antiquewhite1': '#ffefdb',
    'antiquewhite2': '#eedfcc',
    'antiquewhite3': '#cdc0b0',
    'antiquewhite4': '#8b8378',
    'aquamarine': '#7fffd4',
    'aquamarine1': '#7fffd4',
    'aquamarine2': '#76eec6',
    'aquamarine3': '#66cdaa',
    'aquamarine4': '#458b74',
    'azure': '#f0ffff',
    'azure1': '#f0ffff',
    'azure2': '#e0eeee',
    'azure3': '#c1cdcd',
    'azure4': '#838b8b',
    'beige': '#f5f5dc',
    'bisque': '#ffe4c4',
    'bisque1': '#ffe4c4',
    'bisque2': '#eed5b7',
    'bisque3': '#cdb79e',
    'bisque4': '#8b7d6b',
    'black': '#000000',
    'blanched almond': '#ffebcd',
    'blanchedalmond': '#ffebcd',
    'blue': '#0000ff',
    'blue violet': '#8a2be2',
    'blue1': '#0000ff',
    'blue2': '#0000ee',
    'blue3': '#0000cd',
    'blue4': '#00008b',
    'blueviolet': '#8a2be2',
    'brown': '#a52a2a',
    'brown1': '#ff4040',
    'brown2': '#ee3b3b',
    'brown3': '#cd3333',
    'brown4': '#8b2323',
    'burlywood': '#deb887',
    'burlywood1': '#ffd39b',
    'burlywood2': '#eec591',
    'burlywood3': '#cdaa7d',
    'burlywood4': '#8b7355',
    'cadet blue': '#5f9ea0',
    'cadetblue': '#5f9ea0',
    'cadetblue1': '#98f5ff',
    'cadetblue2': '#8ee5ee',
    'cadetblue3': '#7ac5cd',
    'cadetblue4': '#53868b',
    'chartreuse': '#7fff00',
    'chartreuse1': '#7fff00',
    'chartreuse2': '#76ee00',
    'chartreuse3': '#66cd00',
    'chartreuse4': '#458b00',
    'chocolate': '#d2691e',
    'chocolate1': '#ff7f24',
    'chocolate2': '#ee7621',
    'chocolate3': '#cd661d',
    'chocolate4': '#8b4513',
    'coral': '#ff7f50',
    'coral1': '#ff7256',
    'coral2': '#ee6a50',
    'coral3': '#cd5b45',
    'coral4': '#8b3e2f',
    'cornflower blue': '#6495ed',
    'cornflowerblue': '#6495ed',
    'cornsilk': '#fff8dc',
    'cornsilk1': '#fff8dc',
    'cornsilk2': '#eee8cd',
    'cornsilk3': '#cdc8b1',
    'cornsilk4': '#8b8878',
    'cyan': '#00ffff',
    'cyan1': '#00ffff',
    'cyan2': '#00eeee',
    'cyan3': '#00cdcd',
    'cyan4': '#008b8b',
    'dark blue': '#00008b',
    'dark cyan': '#008b8b',
    'dark goldenrod': '#b8860b',
    'dark gray': '#a9a9a9',
    'dark green': '#006400',
    'dark grey': '#a9a9a9',
    'dark khaki': '#bdb76b',
    'dark magenta': '#8b008b',
    'dark olive green': '#556b2f',
    'dark orange': '#ff8c00',
    'dark orchid': '#9932cc',
    'dark red': '#8b0000',
    'dark salmon': '#e9967a',
    'dark sea green': '#8fbc8f',
    'dark slate blue': '#483d8b',
    'dark slate gray': '#2f4f4f',
    'dark slate grey': '#2f4f4f',
    'dark turquoise': '#00ced1',
    'dark violet': '#9400d3',
    'darkblue': '#00008b',
    'darkcyan': '#008b8b',
    'darkgoldenrod': '#b8860b',
    'darkgoldenrod1': '#ffb90f',
    'darkgoldenrod2': '#eead0e',
    'darkgoldenrod3': '#cd950c',
    'darkgoldenrod4': '#8b6508',
    'darkgray': '#a9a9a9',
    'darkgreen': '#006400',
    'darkgrey': '#a9a9a9',
    'darkkhaki': '#bdb76b',
    'darkmagenta': '#8b008b',
    'darkolivegreen': '#556b2f',
    'darkolivegreen1': '#caff70',
    'darkolivegreen2': '#bcee68',
    'darkolivegreen3': '#a2cd5a',
    'darkolivegreen4': '#6e8b3d',
    'darkorange': '#ff8c00',
    'darkorange1': '#ff7f00',
    'darkorange2': '#ee7600',
    'darkorange3': '#cd6600',
    'darkorange4': '#8b4500',
    'darkorchid': '#9932cc',
    'darkorchid1': '#bf3eff',
    'darkorchid2': '#b23aee',
    'darkorchid3': '#9a32cd',
    'darkorchid4': '#68228b',
    'darkred': '#8b0000',
    'darksalmon': '#e9967a',
    'darkseagreen': '#8fbc8f',
    'darkseagreen1': '#c1ffc1',
    'darkseagreen2': '#b4eeb4',
    'darkseagreen3': '#9bcd9b',
    'darkseagreen4': '#698b69',
    'darkslateblue': '#483d8b',
    'darkslategray': '#2f4f4f',
    'darkslategray1': '#97ffff',
    'darkslategray2': '#8deeee',
    'darkslategray3': '#79cdcd',
    'darkslategray4': '#528b8b',
    'darkslategrey': '#2f4f4f',
    'darkturquoise': '#00ced1',
    'darkviolet': '#9400d3',
    'deep pink': '#ff1493',
    'deep sky blue': '#00bfff',
    'deeppink': '#ff1493',
    'deeppink1': '#ff1493',
    'deeppink2': '#ee1289',
    'deeppink3': '#cd1076',
    'deeppink4': '#8b0a50',
    'deepskyblue': '#00bfff',
    'deepskyblue1': '#00bfff',
    'deepskyblue2': '#00b2ee',
    'deepskyblue3': '#009acd',
    'deepskyblue4': '#00688b',
    'dim gray': '#696969',
    'dim grey': '#696969',
    'dimgray': '#696969',
    'dimgrey': '#696969',
    'dodger blue': '#1e90ff',
    'dodgerblue': '#1e90ff',
    'dodgerblue1': '#1e90ff',
    'dodgerblue2': '#1c86ee',
    'dodgerblue3': '#1874cd',
    'dodgerblue4': '#104e8b',
    'firebrick': '#b22222',
    'firebrick1': '#ff3030',
    'firebrick2': '#ee2c2c',
    'firebrick3': '#cd2626',
    'firebrick4': '#8b1a1a',
    'floral white': '#fffaf0',
    'floralwhite': '#fffaf0',
    'forest green': '#228b22',
    'forestgreen': '#228b22',
    'gainsboro': '#dcdcdc',
    'ghost white': '#f8f8ff',
    'ghostwhite': '#f8f8ff',
    'gold': '#ffd700',
    'gold1': '#ffd700',
    'gold2': '#eec900',
    'gold3': '#cdad00',
    'gold4': '#8b7500',
    'goldenrod': '#daa520',
    'goldenrod1': '#ffc125',
    'goldenrod2': '#eeb422',
    'goldenrod3': '#cd9b1d',
    'goldenrod4': '#8b6914',
    'gray': '#bebebe',
    'gray0': '#000000',
    'gray1': '#030303',
    'gray2': '#050505',
    'gray3': '#080808',
    'gray4': '#0a0a0a',
    'gray5': '#0d0d0d',
    'gray6': '#0f0f0f',
    'gray7': '#121212',
    'gray8': '#141414',
    'gray9': '#171717',
    'gray10': '#1a1a1a',
    'gray11': '#1c1c1c',
    'gray12': '#1f1f1f',
    'gray13': '#212121',
    'gray14': '#242424',
    'gray15': '#262626',
    'gray16': '#292929',
    'gray17': '#2b2b2b',
    'gray18': '#2e2e2e',
    'gray19': '#303030',
    'gray20': '#333333',
    'gray21': '#363636',
    'gray22': '#383838',
    'gray23': '#3b3b3b',
    'gray24': '#3d3d3d',
    'gray25': '#404040',
    'gray26': '#424242',
    'gray27': '#454545',
    'gray28': '#474747',
    'gray29': '#4a4a4a',
    'gray30': '#4d4d4d',
    'gray31': '#4f4f4f',
    'gray32': '#525252',
    'gray33': '#545454',
    'gray34': '#575757',
    'gray35': '#595959',
    'gray36': '#5c5c5c',
    'gray37': '#5e5e5e',
    'gray38': '#616161',
    'gray39': '#636363',
    'gray40': '#666666',
    'gray41': '#696969',
    'gray42': '#6b6b6b',
    'gray43': '#6e6e6e',
    'gray44': '#707070',
    'gray45': '#737373',
    'gray46': '#757575',
    'gray47': '#787878',
    'gray48': '#7a7a7a',
    'gray49': '#7d7d7d',
    'gray50': '#7f7f7f',
    'gray51': '#828282',
    'gray52': '#858585',
    'gray53': '#878787',
    'gray54': '#8a8a8a',
    'gray55': '#8c8c8c',
    'gray56': '#8f8f8f',
    'gray57': '#919191',
    'gray58': '#949494',
    'gray59': '#969696',
    'gray60': '#999999',
    'gray61': '#9c9c9c',
    'gray62': '#9e9e9e',
    'gray63': '#a1a1a1',
    'gray64': '#a3a3a3',
    'gray65': '#a6a6a6',
    'gray66': '#a8a8a8',
    'gray67': '#ababab',
    'gray68': '#adadad',
    'gray69': '#b0b0b0',
    'gray70': '#b3b3b3',
    'gray71': '#b5b5b5',
    'gray72': '#b8b8b8',
    'gray73': '#bababa',
    'gray74': '#bdbdbd',
    'gray75': '#bfbfbf',
    'gray76': '#c2c2c2',
    'gray77': '#c4c4c4',
    'gray78': '#c7c7c7',
    'gray79': '#c9c9c9',
    'gray80': '#cccccc',
    'gray81': '#cfcfcf',
    'gray82': '#d1d1d1',
    'gray83': '#d4d4d4',
    'gray84': '#d6d6d6',
    'gray85': '#d9d9d9',
    'gray86': '#dbdbdb',
    'gray87': '#dedede',
    'gray88': '#e0e0e0',
    'gray89': '#e3e3e3',
    'gray90': '#e5e5e5',
    'gray91': '#e8e8e8',
    'gray92': '#ebebeb',
    'gray93': '#ededed',
    'gray94': '#f0f0f0',
    'gray95': '#f2f2f2',
    'gray96': '#f5f5f5',
    'gray97': '#f7f7f7',
    'gray98': '#fafafa',
    'gray99': '#fcfcfc',
    'gray100': '#ffffff',
    'green': '#00ff00',
    'green yellow': '#adff2f',
    'green1': '#00ff00',
    'green2': '#00ee00',
    'green3': '#00cd00',
    'green4': '#008b00',
    'greenyellow': '#adff2f',
    'grey': '#bebebe',
    'grey0': '#000000',
    'grey1': '#030303',
    'grey2': '#050505',
    'grey3': '#080808',
    'grey4': '#0a0a0a',
    'grey5': '#0d0d0d',
    'grey6': '#0f0f0f',
    'grey7': '#121212',
    'grey8': '#141414',
    'grey9': '#171717',
    'grey10': '#1a1a1a',
    'grey11': '#1c1c1c',
    'grey12': '#1f1f1f',
    'grey13': '#212121',
    'grey14': '#242424',
    'grey15': '#262626',
    'grey16': '#292929',
    'grey17': '#2b2b2b',
    'grey18': '#2e2e2e',
    'grey19': '#303030',
    'grey20': '#333333',
    'grey21': '#363636',
    'grey22': '#383838',
    'grey23': '#3b3b3b',
    'grey24': '#3d3d3d',
    'grey25': '#404040',
    'grey26': '#424242',
    'grey27': '#454545',
    'grey28': '#474747',
    'grey29': '#4a4a4a',
    'grey30': '#4d4d4d',
    'grey31': '#4f4f4f',
    'grey32': '#525252',
    'grey33': '#545454',
    'grey34': '#575757',
    'grey35': '#595959',
    'grey36': '#5c5c5c',
    'grey37': '#5e5e5e',
    'grey38': '#616161',
    'grey39': '#636363',
    'grey40': '#666666',
    'grey41': '#696969',
    'grey42': '#6b6b6b',
    'grey43': '#6e6e6e',
    'grey44': '#707070',
    'grey45': '#737373',
    'grey46': '#757575',
    'grey47': '#787878',
    'grey48': '#7a7a7a',
    'grey49': '#7d7d7d',
    'grey50': '#7f7f7f',
    'grey51': '#828282',
    'grey52': '#858585',
    'grey53': '#878787',
    'grey54': '#8a8a8a',
    'grey55': '#8c8c8c',
    'grey56': '#8f8f8f',
    'grey57': '#919191',
    'grey58': '#949494',
    'grey59': '#969696',
    'grey60': '#999999',
    'grey61': '#9c9c9c',
    'grey62': '#9e9e9e',
    'grey63': '#a1a1a1',
    'grey64': '#a3a3a3',
    'grey65': '#a6a6a6',
    'grey66': '#a8a8a8',
    'grey67': '#ababab',
    'grey68': '#adadad',
    'grey69': '#b0b0b0',
    'grey70': '#b3b3b3',
    'grey71': '#b5b5b5',
    'grey72': '#b8b8b8',
    'grey73': '#bababa',
    'grey74': '#bdbdbd',
    'grey75': '#bfbfbf',
    'grey76': '#c2c2c2',
    'grey77': '#c4c4c4',
    'grey78': '#c7c7c7',
    'grey79': '#c9c9c9',
    'grey80': '#cccccc',
    'grey81': '#cfcfcf',
    'grey82': '#d1d1d1',
    'grey83': '#d4d4d4',
    'grey84': '#d6d6d6',
    'grey85': '#d9d9d9',
    'grey86': '#dbdbdb',
    'grey87': '#dedede',
    'grey88': '#e0e0e0',
    'grey89': '#e3e3e3',
    'grey90': '#e5e5e5',
    'grey91': '#e8e8e8',
    'grey92': '#ebebeb',
    'grey93': '#ededed',
    'grey94': '#f0f0f0',
    'grey95': '#f2f2f2',
    'grey96': '#f5f5f5',
    'grey97': '#f7f7f7',
    'grey98': '#fafafa',
    'grey99': '#fcfcfc',
    'grey100': '#ffffff',
    'honeydew': '#f0fff0',
    'honeydew1': '#f0fff0',
    'honeydew2': '#e0eee0',
    'honeydew3': '#c1cdc1',
    'honeydew4': '#838b83',
    'hot pink': '#ff69b4',
    'hotpink': '#ff69b4',
    'hotpink1': '#ff6eb4',
    'hotpink2': '#ee6aa7',
    'hotpink3': '#cd6090',
    'hotpink4': '#8b3a62',
    'indian red': '#cd5c5c',
    'indianred': '#cd5c5c',
    'indianred1': '#ff6a6a',
    'indianred2': '#ee6363',
    'indianred3': '#cd5555',
    'indianred4': '#8b3a3a',
    'ivory': '#fffff0',
    'ivory1': '#fffff0',
    'ivory2': '#eeeee0',
    'ivory3': '#cdcdc1',
    'ivory4': '#8b8b83',
    'khaki': '#f0e68c',
    'khaki1': '#fff68f',
    'khaki2': '#eee685',
    'khaki3': '#cdc673',
    'khaki4': '#8b864e',
    'lavender': '#e6e6fa',
    'lavender blush': '#fff0f5',
    'lavenderblush': '#fff0f5',
    'lavenderblush1': '#fff0f5',
    'lavenderblush2': '#eee0e5',
    'lavenderblush3': '#cdc1c5',
    'lavenderblush4': '#8b8386',
    'lawn green': '#7cfc00',
    'lawngreen': '#7cfc00',
    'lemon chiffon': '#fffacd',
    'lemonchiffon': '#fffacd',
    'lemonchiffon1': '#fffacd',
    'lemonchiffon2': '#eee9bf',
    'lemonchiffon3': '#cdc9a5',
    'lemonchiffon4': '#8b8970',
    'light blue': '#add8e6',
    'light coral': '#f08080',
    'light cyan': '#e0ffff',
    'light goldenrod': '#eedd82',
    'light goldenrod yellow': '#fafad2',
    'light gray': '#d3d3d3',
    'light green': '#90ee90',
    'light grey': '#d3d3d3',
    'light pink': '#ffb6c1',
    'light salmon': '#ffa07a',
    'light sea green': '#20b2aa',
    'light sky blue': '#87cefa',
    'light slate blue': '#8470ff',
    'light slate gray': '#778899',
    'light slate grey': '#778899',
    'light steel blue': '#b0c4de',
    'light yellow': '#ffffe0',
    'lightblue': '#add8e6',
    'lightblue1': '#bfefff',
    'lightblue2': '#b2dfee',
    'lightblue3': '#9ac0cd',
    'lightblue4': '#68838b',
    'lightcoral': '#f08080',
    'lightcyan': '#e0ffff',
    'lightcyan1': '#e0ffff',
    'lightcyan2': '#d1eeee',
    'lightcyan3': '#b4cdcd',
    'lightcyan4': '#7a8b8b',
    'lightgoldenrod': '#eedd82',
    'lightgoldenrod1': '#ffec8b',
    'lightgoldenrod2': '#eedc82',
    'lightgoldenrod3': '#cdbe70',
    'lightgoldenrod4': '#8b814c',
    'lightgoldenrodyellow': '#fafad2',
    'lightgray': '#d3d3d3',
    'lightgreen': '#90ee90',
    'lightgrey': '#d3d3d3',
    'lightpink': '#ffb6c1',
    'lightpink1': '#ffaeb9',
    'lightpink2': '#eea2ad',
    'lightpink3': '#cd8c95',
    'lightpink4': '#8b5f65',
    'lightsalmon': '#ffa07a',
    'lightsalmon1': '#ffa07a',
    'lightsalmon2': '#ee9572',
    'lightsalmon3': '#cd8162',
    'lightsalmon4': '#8b5742',
    'lightseagreen': '#20b2aa',
    'lightskyblue': '#87cefa',
    'lightskyblue1': '#b0e2ff',
    'lightskyblue2': '#a4d3ee',
    'lightskyblue3': '#8db6cd',
    'lightskyblue4': '#607b8b',
    'lightslateblue': '#8470ff',
    'lightslategray': '#778899',
    'lightslategrey': '#778899',
    'lightsteelblue': '#b0c4de',
    'lightsteelblue1': '#cae1ff',
    'lightsteelblue2': '#bcd2ee',
    'lightsteelblue3': '#a2b5cd',
    'lightsteelblue4': '#6e7b8b',
    'lightyellow': '#ffffe0',
    'lightyellow1': '#ffffe0',
    'lightyellow2': '#eeeed1',
    'lightyellow3': '#cdcdb4',
    'lightyellow4': '#8b8b7a',
    'lime green': '#32cd32',
    'limegreen': '#32cd32',
    'linen': '#faf0e6',
    'magenta': '#ff00ff',
    'magenta1': '#ff00ff',
    'magenta2': '#ee00ee',
    'magenta3': '#cd00cd',
    'magenta4': '#8b008b',
    'maroon': '#b03060',
    'maroon1': '#ff34b3',
    'maroon2': '#ee30a7',
    'maroon3': '#cd2990',
    'maroon4': '#8b1c62',
    'medium aquamarine': '#66cdaa',
    'medium blue': '#0000cd',
    'medium orchid': '#ba55d3',
    'medium purple': '#9370db',
    'medium sea green': '#3cb371',
    'medium slate blue': '#7b68ee',
    'medium spring green': '#00fa9a',
    'medium turquoise': '#48d1cc',
    'medium violet red': '#c71585',
    'mediumaquamarine': '#66cdaa',
    'mediumblue': '#0000cd',
    'mediumorchid': '#ba55d3',
    'mediumorchid1': '#e066ff',
    'mediumorchid2': '#d15fee',
    'mediumorchid3': '#b452cd',
    'mediumorchid4': '#7a378b',
    'mediumpurple': '#9370db',
    'mediumpurple1': '#ab82ff',
    'mediumpurple2': '#9f79ee',
    'mediumpurple3': '#8968cd',
    'mediumpurple4': '#5d478b',
    'mediumseagreen': '#3cb371',
    'mediumslateblue': '#7b68ee',
    'mediumspringgreen': '#00fa9a',
    'mediumturquoise': '#48d1cc',
    'mediumvioletred': '#c71585',
    'midnight blue': '#191970',
    'midnightblue': '#191970',
    'mint cream': '#f5fffa',
    'mintcream': '#f5fffa',
    'misty rose': '#ffe4e1',
    'mistyrose': '#ffe4e1',
    'mistyrose1': '#ffe4e1',
    'mistyrose2': '#eed5d2',
    'mistyrose3': '#cdb7b5',
    'mistyrose4': '#8b7d7b',
    'moccasin': '#ffe4b5',
    'navajo white': '#ffdead',
    'navajowhite': '#ffdead',
    'navajowhite1': '#ffdead',
    'navajowhite2': '#eecfa1',
    'navajowhite3': '#cdb38b',
    'navajowhite4': '#8b795e',
    'navy': '#000080',
    'navy blue': '#000080',
    'navyblue': '#000080',
    'old lace': '#fdf5e6',
    'oldlace': '#fdf5e6',
    'olive drab': '#6b8e23',
    'olivedrab': '#6b8e23',
    'olivedrab1': '#c0ff3e',
    'olivedrab2': '#b3ee3a',
    'olivedrab3': '#9acd32',
    'olivedrab4': '#698b22',
    'orange': '#ffa500',
    'orange red': '#ff4500',
    'orange1': '#ffa500',
    'orange2': '#ee9a00',
    'orange3': '#cd8500',
    'orange4': '#8b5a00',
    'orangered': '#ff4500',
    'orangered1': '#ff4500',
    'orangered2': '#ee4000',
    'orangered3': '#cd3700',
    'orangered4': '#8b2500',
    'orchid': '#da70d6',
    'orchid1': '#ff83fa',
    'orchid2': '#ee7ae9',
    'orchid3': '#cd69c9',
    'orchid4': '#8b4789',
    'pale goldenrod': '#eee8aa',
    'pale green': '#98fb98',
    'pale turquoise': '#afeeee',
    'pale violet red': '#db7093',
    'palegoldenrod': '#eee8aa',
    'palegreen': '#98fb98',
    'palegreen1': '#9aff9a',
    'palegreen2': '#90ee90',
    'palegreen3': '#7ccd7c',
    'palegreen4': '#548b54',
    'paleturquoise': '#afeeee',
    'paleturquoise1': '#bbffff',
    'paleturquoise2': '#aeeeee',
    'paleturquoise3': '#96cdcd',
    'paleturquoise4': '#668b8b',
    'palevioletred': '#db7093',
    'palevioletred1': '#ff82ab',
    'palevioletred2': '#ee799f',
    'palevioletred3': '#cd687f',
    'palevioletred4': '#8b475d',
    'papaya whip': '#ffefd5',
    'papayawhip': '#ffefd5',
    'peach puff': '#ffdab9',
    'peachpuff': '#ffdab9',
    'peachpuff1': '#ffdab9',
    'peachpuff2': '#eecbad',
    'peachpuff3': '#cdaf95',
    'peachpuff4': '#8b7765',
    'peru': '#cd853f',
    'pink': '#ffc0cb',
    'pink1': '#ffb5c5',
    'pink2': '#eea9b8',
    'pink3': '#cd919e',
    'pink4': '#8b636c',
    'plum': '#dda0dd',
    'plum1': '#ffbbff',
    'plum2': '#eeaeee',
    'plum3': '#cd96cd',
    'plum4': '#8b668b',
    'powder blue': '#b0e0e6',
    'powderblue': '#b0e0e6',
    'purple': '#a020f0',
    'purple1': '#9b30ff',
    'purple2': '#912cee',
    'purple3': '#7d26cd',
    'purple4': '#551a8b',
    'red': '#ff0000',
    'red1': '#ff0000',
    'red2': '#ee0000',
    'red3': '#cd0000',
    'red4': '#8b0000',
    'rosy brown': '#bc8f8f',
    'rosybrown': '#bc8f8f',
    'rosybrown1': '#ffc1c1',
    'rosybrown2': '#eeb4b4',
    'rosybrown3': '#cd9b9b',
    'rosybrown4': '#8b6969',
    'royal blue': '#4169e1',
    'royalblue': '#4169e1',
    'royalblue1': '#4876ff',
    'royalblue2': '#436eee',
    'royalblue3': '#3a5fcd',
    'royalblue4': '#27408b',
    'saddle brown': '#8b4513',
    'saddlebrown': '#8b4513',
    'salmon': '#fa8072',
    'salmon1': '#ff8c69',
    'salmon2': '#ee8262',
    'salmon3': '#cd7054',
    'salmon4': '#8b4c39',
    'sandy brown': '#f4a460',
    'sandybrown': '#f4a460',
    'sea green': '#2e8b57',
    'seagreen': '#2e8b57',
    'seagreen1': '#54ff9f',
    'seagreen2': '#4eee94',
    'seagreen3': '#43cd80',
    'seagreen4': '#2e8b57',
    'seashell': '#fff5ee',
    'seashell1': '#fff5ee',
    'seashell2': '#eee5de',
    'seashell3': '#cdc5bf',
    'seashell4': '#8b8682',
    'sienna': '#a0522d',
    'sienna1': '#ff8247',
    'sienna2': '#ee7942',
    'sienna3': '#cd6839',
    'sienna4': '#8b4726',
    'sky blue': '#87ceeb',
    'skyblue': '#87ceeb',
    'skyblue1': '#87ceff',
    'skyblue2': '#7ec0ee',
    'skyblue3': '#6ca6cd',
    'skyblue4': '#4a708b',
    'slate blue': '#6a5acd',
    'slate gray': '#708090',
    'slate grey': '#708090',
    'slateblue': '#6a5acd',
    'slateblue1': '#836fff',
    'slateblue2': '#7a67ee',
    'slateblue3': '#6959cd',
    'slateblue4': '#473c8b',
    'slategray': '#708090',
    'slategray1': '#c6e2ff',
    'slategray2': '#b9d3ee',
    'slategray3': '#9fb6cd',
    'slategray4': '#6c7b8b',
    'slategrey': '#708090',
    'snow': '#fffafa',
    'snow1': '#fffafa',
    'snow2': '#eee9e9',
    'snow3': '#cdc9c9',
    'snow4': '#8b8989',
    'spring green': '#00ff7f',
    'springgreen': '#00ff7f',
    'springgreen1': '#00ff7f',
    'springgreen2': '#00ee76',
    'springgreen3': '#00cd66',
    'springgreen4': '#008b45',
    'steel blue': '#4682b4',
    'steelblue': '#4682b4',
    'steelblue1': '#63b8ff',
    'steelblue2': '#5cacee',
    'steelblue3': '#4f94cd',
    'steelblue4': '#36648b',
    'tan': '#d2b48c',
    'tan1': '#ffa54f',
    'tan2': '#ee9a49',
    'tan3': '#cd853f',
    'tan4': '#8b5a2b',
    'thistle': '#d8bfd8',
    'thistle1': '#ffe1ff',
    'thistle2': '#eed2ee',
    'thistle3': '#cdb5cd',
    'thistle4': '#8b7b8b',
    'tomato': '#ff6347',
    'tomato1': '#ff6347',
    'tomato2': '#ee5c42',
    'tomato3': '#cd4f39',
    'tomato4': '#8b3626',
    'turquoise': '#40e0d0',
    'turquoise1': '#00f5ff',
    'turquoise2': '#00e5ee',
    'turquoise3': '#00c5cd',
    'turquoise4': '#00868b',
    'violet': '#ee82ee',
    'violet red': '#d02090',
    'violetred': '#d02090',
    'violetred1': '#ff3e96',
    'violetred2': '#ee3a8c',
    'violetred3': '#cd3278',
    'violetred4': '#8b2252',
    'wheat': '#f5deb3',
    'wheat1': '#ffe7ba',
    'wheat2': '#eed8ae',
    'wheat3': '#cdba96',
    'wheat4': '#8b7e66',
    'white': '#ffffff',
    'white smoke': '#f5f5f5',
    'whitesmoke': '#f5f5f5',
    'yellow': '#ffff00',
    'yellow green': '#9acd32',
    'yellow1': '#ffff00',
    'yellow2': '#eeee00',
    'yellow3': '#cdcd00',
    'yellow4': '#8b8b00',
    'yellowgreen': '#9acd32',
}


def monkey_patch_pyglet(canvas):

    pyglet = sys.modules['pyglet']

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
            image = b.getvalue()
            encoded = standard_b64encode(image)
            img_str = str(encoded.decode('UTF-8'))
            MockTurtle.display_image(0, 0, image=img_str)

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
