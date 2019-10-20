import io
import logging
import os
import subprocess
import sys

import sublime, sublime_plugin


XMIN, YMIN, XMAX, YMAX = list(range(4))
OUTPUT_VIEW_ID = 'output_view_id'
INPUT_VIEW_TRACER_ARGS = 'input_view_tracer_args'
CANVAS_START = 'start_canvas'
CANVAS_END = 'end_canvas'
HAS_IDLE_TIMER = 'has_idle_timer'
SCROLL_TIMER = 50


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def find_view(view_id):
    for window in sublime.windows():
        for view in window.views():
            if view.id() == view_id:
                return view
    return None


def trace_code(input_view):
    view_settings = input_view.settings()
    tracer_args = view_settings.get(INPUT_VIEW_TRACER_ARGS)
    contents = input_view.substr(sublime.Region(0, input_view.size()))

    # Pull location of python exe and code_tracer.py script from user settings.
    settings = sublime.load_settings('python_live_coding.sublime-settings')
    executable = settings.get('python_executable', sys.executable)
    default_space_tracer = os.path.join(__file__, '../space_tracer')
    tracer_path = settings.get('space_tracer', default_space_tracer)
    tracer_path = os.path.abspath(os.path.dirname(tracer_path))
    python_path = os.pathsep.join([tracer_path] + sys.path)
    new_env = dict(os.environ, PYTHONPATH=python_path)
    args = [executable,
            '-m', 'space_tracer',
            '--live',
            '--source_width', '0',
            '--traced_file', input_view.file_name()]
    if tracer_args:
        args.extend(tracer_args)
    args.append(input_view.file_name())

    # Startup info so we don't open a new command prompt in windows.
    startupinfo = None
    if os.name == 'nt':
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW

    # logger.info('space_tracer args: ' + ' '.join(args))
    # Launch, pipe in code and return.
    proc = subprocess.Popen(
        args, 
        stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE, 
        stderr=subprocess.PIPE, 
        startupinfo=startupinfo,
        universal_newlines=True,
        env=new_env)
    return proc.communicate(input=contents)


def update_output_for_view(input_view):

    # Trace the code in input view's buffer.
    view_settings = input_view.settings()
    out, err = trace_code(input_view)
    if err:
        logger.error(err)

    img = None
    reader = io.StringIO(out)
    stdout = ''
    done = False
    while not done:
        line = reader.readline()
        if line.strip() == CANVAS_START:
            cmds = CanvasReader(reader).read_commands()
            for cmd in cmds:
                if cmd.name == 'create_image':
                    encoded = cmd.options['image']
                    img_src = 'data:image/png;base64,{}'.format(encoded)
                    img = '<img src="{}" width="800" height="600">'.format(img_src)
            reader.readline()  # Skip a line.
        elif line:
            stdout += line
        else:
            done = True

    # Remove all phantoms from the output view then update with stdout / goal
    # image.
    output_view = find_view(view_settings.get(OUTPUT_VIEW_ID))
    output_view.erase_phantoms('goal_image')
    if img is not None:
        output_view.run_command('output_view_replace', {'text': ''})
        output_view.add_phantom('goal_image', output_view.sel()[0], img, sublime.LAYOUT_BLOCK)
    else:
        output_view.run_command('output_view_replace', {'text': stdout})


class CanvasCommand(object):
    
    def __init__(self, *args, **kwargs):
        self.name = None
        self.coordinates = []
        self.options = {}
    

class CanvasReader(object):

    def __init__(self, input_reader):
        self.input_reader = input_reader
        self.next_line = None

    def read(self):
        line = self.next_line if self.next_line is not None else self.input_reader.readline().rstrip()
        if line is None:
            return None
        self.next_line = None
        command = CanvasCommand()
        command.name = line
        while True:
            self.next_line = self.input_reader.readline().rstrip()
            if self.next_line is None or not self.next_line.startswith('    '):
                return command
            self.next_line = self.next_line[4:] # trim spaces
            if '=' not in self.next_line:
                command.coordinates.append(int(self.next_line))
            else:
                position = self.next_line.index('=')
                name = self.next_line[:position]
                value = self.next_line[position + 1:]
                value = self.unescape_string(value)
                command.options[name] = value

    def unescape_string(self, value):
        if value.startswith('\'') and value.endswith('\''):
            value = value[1:len(value) - 1]
            writer = ''
            for i, c in enumerate(value):
                if c != '\\' or i == len(value) - 1:
                    writer += c
                else:
                    i += 1
                    c2 = value[i]
                    if c2 == '\\':
                        writer += '\\'
                        break
                    elif c2 == 'n':
                        writer += '\n'
                        break
                    elif c2 == 'r':
                        writer += '\r'
                        break
                    elif c2 == 't':
                        writer += '\t'
                        break
                    elif c2 == 'x':
                        if i + 2 < len(value):
                            x = int(value[i + 1:i + 3])
                            i += 2
                            break
                    else:
                        writer += c
                        writer += c2
                        break
        return value

    def read_commands(self):
        new_commands = []
        done = False
        while not done:
            command = self.read()
            done = command is None or command.name == CANVAS_END
            if not done:
                new_commands.append(command)
        return new_commands


class BaseWindowCommand(sublime_plugin.WindowCommand):

    def fixed_set_layout(self, window, layout):

        # A bug was introduced in Sublime Text 3, sometime before 3053, in that 
        # it changes the active group to 0 when the layout is changed. Annoying.
        active_group = window.active_group()
        window.run_command('set_layout', layout)
        num_groups = len(layout['cells'])
        window.focus_group(min(active_group, num_groups - 1))

    def get_layout(self):
        layout = self.window.get_layout()
        cells = layout['cells']
        rows = layout['rows']
        cols = layout['cols']
        return rows, cols, cells

    def create_pane(self):
        
        rows, cols, cells = self.get_layout()
        active_group = self.window.active_group()

        old_cell = cells[active_group]

        cols.insert(old_cell[XMAX], (cols[old_cell[XMIN]] + cols[old_cell[XMAX]]) / 2)
        new_cell = [old_cell[XMAX], old_cell[YMIN], old_cell[XMAX] + 1, old_cell[YMAX]]
        cells.append(new_cell)

        self.fixed_set_layout(self.window, {
            'cols': cols, 
            'rows': rows, 
            'cells': cells
        })

        # Create a new file in the new group, then switch active group back.
        self.window.focus_group(len(cells) - 1)
        output_view = self.window.new_file()
        self.window.focus_group(active_group)
        logger.info('Created output view id: {}'.format(output_view.id()))

        return output_view


class ResetCommand(BaseWindowCommand):

    def run(self):
        
        # Close any output views and remove their tag.
        for view in self.window.views():
            if not view.settings().has(OUTPUT_VIEW_ID):
                continue
            output_view_id = view.settings().get(OUTPUT_VIEW_ID)
            output_view = find_view(output_view_id)
            if output_view is not None:
                logger.info('Closing view: "{}"'.format(output_view.name()))
                output_view.close()
            view.settings().erase(OUTPUT_VIEW_ID)
            view.settings().erase(HAS_IDLE_TIMER)

        # Set the layout back to a single group.
        rows, cols, cells = self.get_layout()
        self.fixed_set_layout(self.window, {
            'cols': [0, 1], 
            'rows': [0, 1], 
            'cells': [[0, 0, 1, 1]]
        })


class StartCommand(BaseWindowCommand):

    TRACER_ARGS = []

    def run(self):
        active_group = self.window.active_group()
        input_view = self.window.active_view_in_group(active_group)
        if input_view.settings().has(OUTPUT_VIEW_ID):
            msg = 'Already a live coding session for the current view.'
            sublime.message_dialog(msg)
            return

        output_view = self.create_pane()
        output_view.set_scratch(True)
        output_view.settings().set('word_wrap', False)
        output_view.set_name('live coding output')

        input_view.settings().set('word_wrap', False)
        input_view.settings().set(OUTPUT_VIEW_ID, output_view.id())
        input_view.settings().set(INPUT_VIEW_TRACER_ARGS, self.TRACER_ARGS)

        update_output_for_view(input_view)


class StartCanvasCommand(StartCommand):

    TRACER_ARGS = ['--canvas']


class OutputViewReplaceCommand(sublime_plugin.TextCommand):

    def run(self, edit, text=''):
        self.view.replace(edit, sublime.Region(0, self.view.size()), text)


class InputViewEventListener(sublime_plugin.ViewEventListener):

    """
    Will execute live coding update once the timeout expires. Based on the 
    IdleWatcher example here: http://www.sublimetext.com/docs/plugin-examples
    """

    pending = 0

    @classmethod
    def is_applicable(cls, settings):
        return settings.has(OUTPUT_VIEW_ID)

    def on_modified_timeout_async(self):
        self.pending = self.pending - 1
        if self.pending == 0:

            # There are no more queued up calls to on_timeout, so it must have
            # been 1000ms since the last modification.
            update_output_for_view(self.view)

    def on_modified_async(self):
        self.pending = self.pending + 1

        # Ask for on_timeout to be called when timeout has expired.
        settings = sublime.load_settings('python_live_coding.sublime-settings')
        timeout = settings.get('timout_duration', 300)
        sublime.set_timeout_async(self.on_modified_timeout_async, timeout)

    def on_activated_timeout_async(self): 

        # Get the output view. Note that it may be None in the event that this
        # callback fires after the user closes the view.
        output_view = find_view(self.view.settings().get(OUTPUT_VIEW_ID))
        if output_view is None:
            logger.warning('Attempted to scroll non-existent view')
            return

        output_view.set_viewport_position(self.view.viewport_position())

        # Queue the callback to fire again in order to create an on_idle event.
        if self.is_applicable(self.view.settings()):
            sublime.set_timeout_async(self.on_activated_timeout_async, SCROLL_TIMER)

    def on_activated_async(self):
        settings = self.view.settings()
        if settings.has(HAS_IDLE_TIMER):
            logger.info('Already a scroll timer on the current view')
            return
        settings.set(HAS_IDLE_TIMER, True)
        sublime.set_timeout_async(self.on_activated_timeout_async, SCROLL_TIMER)
