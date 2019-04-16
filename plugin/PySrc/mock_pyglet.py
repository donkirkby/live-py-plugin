import sys
from functools import partial

import pyglet
import pyglet.gl as gl


class MockPyglet(object):

    class Window(pyglet.window.Window):

        def __init__(self, width, height):
            conf = gl.Config(double_buffer=True)
            super(MockPyglet.Window, self).__init__(
                config=conf,
                resizable=True,
                visible=False,
                width=width,
                height=height
            )
            self.on_resize( width, height )

        def on_draw(self):
            colorbuffer = pyglet.image.get_buffer_manager().get_color_buffer()
            colorbuffer.save('screenshot.png')


    @classmethod
    def monkey_patch(cls, canvas):
        pyglet_module = sys.modules['pyglet']
        w = canvas.cget('width')
        h = canvas.cget('height')

        def run():
            for window in pyglet_module.app.windows:
                for i in range(2):
                    pyglet.clock.tick()
                    window.switch_to()
                    window.dispatch_events()
                    window.dispatch_event('on_draw')
                    window.flip()
                window.close()

        pyglet_module.app.run = run
        pyglet_module.window.Window = partial(MockPyglet.Window, width=w, height=h)