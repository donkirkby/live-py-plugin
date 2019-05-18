import io
import sys
import base64

import pyglet

from mock_turtle import MockTurtle


class MockPyglet(object):

    class Window(pyglet.window.Window):

        def __init__(self, **kwargs):
            conf = pyglet.gl.Config(double_buffer=True)
            super(MockPyglet.Window, self).__init__(
                config=conf,
                resizable=True,
                visible=False,
                width=self.width,
                height=self.height
            )
            self.on_resize(self.width, self.height)

        def on_draw(self):

            # Get the colour buffer, write it to a bytearray in png format.
            buf = pyglet.image.get_buffer_manager().get_color_buffer()
            b = io.BytesIO()
            buf.save('buffer.png', b)
            img_str = base64.b64encode(b.getvalue()) 
            MockTurtle.display_image(0, 0, image=img_str)


    @classmethod
    def monkey_patch(cls, canvas):
        pyglet_module = sys.modules['pyglet']

        # Let the canvas size dictate the program's window size.
        cls.width = canvas.cget('width')
        cls.height = canvas.cget('height')

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
        pyglet_module.window.Window = MockPyglet.Window