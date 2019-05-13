import os
import io
import sys
import logging
import tempfile
import functools
import subprocess

import sublime, sublime_plugin


XMIN, YMIN, XMAX, YMAX = list(range(4))
OUTPUT_TEXT = 'output_text'
OUTPUT_IMAGE = 'output_image'
OUTPUT_TYPE = 'output_type'
OUTPUT_VIEW_ID = 'output_view_id'

CANVAS_START = 'start_canvas'
CANVAS_END = 'end_canvas'


logging.basicConfig(level=logging.INFO)


def find_view(view_id):
    for window in sublime.windows():
        for view in window.views():
            if view.id() == view_id:
                return view
    return None


class CanvasCommand(object):
    pass
    

class CanvasReader(object):

    def __init__(self, inputReader):
        self.inputReader = inputReader

    def read(self):
        return self.inputReader.readline()

    def readCommands(self):
        newCommands = []
        done = False
        while not done:
            command = self.read()
            print('command:', command)
            done = command == None or command.strip() == CANVAS_END
            if not done:
               newCommands.append(command)
        return newCommands


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
        logging.getLogger().info('Created output view id: {}'.format(output_view.id()))

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
               logging.getLogger().info('Closing view: {}'.format(output_view.name()))
               output_view.close()
            view.settings().erase(OUTPUT_VIEW_ID)

        # Set the layout back to a single group.
        rows, cols, cells = self.get_layout()
        self.fixed_set_layout(self.window, {
            'cols': [0, 1], 
            'rows': [0, 1], 
            'cells': [[0, 0, 1, 1]]
        })


class BaseStartCommand(BaseWindowCommand):

    OUTPUT_TYPE = None

    def run(self):
        active_group = self.window.active_group()
        input_view = self.window.active_view_in_group(active_group)
        if input_view.settings().has(OUTPUT_VIEW_ID):
            msg = 'Already a live coding session for the current view.'
            sublime.message_dialog(msg)
            return

        output_view = self.create_pane()

        input_view.settings().set('word_wrap', False)
        input_view.settings().set(OUTPUT_VIEW_ID, output_view.id())
        
        output_view.set_scratch(True)
        output_view.settings().set('word_wrap', False)
        output_view.settings().set(OUTPUT_TYPE, self.OUTPUT_TYPE)
        output_view.set_name('live coding output')
        
        output_view.run_command('output_view_update', {'input_view_id': input_view.id()})


class StartCommand(BaseStartCommand):

    OUTPUT_TYPE = OUTPUT_TEXT


class PygletCommand( BaseStartCommand ):

    OUTPUT_TYPE = OUTPUT_IMAGE


class OutputViewUpdateCommand(sublime_plugin.TextCommand):

    def trace_code(self, contents):

        # Pull location of python exe and code_tracer.py script from user
        # settings.
        settings = sublime.load_settings('PythonLiveCoding.sublime-settings')
        py_path = settings.get('python_executable')
        tracer_path = settings.get('code_tracer')
        args = [
            py_path, 
            tracer_path,
            '--canvas',
            '-'
        ]

        # Startup info so we don't open a new command prompt.
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW

        # Launch, pipe in code and return.
        proc = subprocess.Popen(
            args, 
            stdin=subprocess.PIPE, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE, 
            startupinfo=startupinfo,
            universal_newlines=True
        )
        out, err = proc.communicate(input=contents)

        reader = io.StringIO(out)
        stdout = ''
        done = False
        while not done:
            line = reader.readline()
            if line.strip() == CANVAS_START:
                cmds = CanvasReader(reader).readCommands()
                line = reader.readline() # Skip a line.
            elif line:
                stdout += line
            else:
                done = True
        return stdout

    def run(self, edit, input_view_id=None):
        input_view = find_view(input_view_id)
        logging.getLogger().info('Update running for input view id: {}'.format(self.view.id()))
        contents = input_view.substr(sublime.Region(0, input_view.size()))
        code_report = self.trace_code(contents)
        output_type = self.view.settings().get(OUTPUT_TYPE, OUTPUT_TEXT)
        if output_type == OUTPUT_TEXT:
            logging.getLogger().info('Updating text in view id: {}'.format(self.view.id()))
            self.view.replace(edit, sublime.Region(0, self.view.size()), code_report)
        else:
            logging.getLogger().info('Updating image in view id: {}'.format(self.view.id()))

            self.view.erase(edit, sublime.Region(0, self.view.size()))
            img_path = 'file://{}'.format(os.path.join(tempfile.gettempdir(), 'screenshot.png'))
            html = '<img src="' + img_path + '" width="800" height="600">'
            self.view.erase_phantoms('test')
            self.view.add_phantom('test', self.view.sel()[0], html, sublime.LAYOUT_BLOCK)


class InputViewEventListener(sublime_plugin.ViewEventListener):

    """
    Will execute live coding update once the timeout expires. Based on the 
    IdleWatcher example here: http://www.sublimetext.com/docs/plugin-examples
    """

    pending = 0

    @classmethod
    def is_applicable(cls, settings):
        return settings.has(OUTPUT_VIEW_ID)

    def on_timeout(self):
        self.pending = self.pending - 1
        if self.pending == 0:

            # There are no more queued up calls to on_timeout, so it must 
            # have been 1000ms since the last modification.
            settings = sublime.load_settings('PythonLiveCoding.sublime-settings')
            timeout = settings.get('timout_duration', 300)
            logging.getLogger().info('Window idle for: {}ms - updating live coding output'.format(timeout))
            self.on_idle()

    def on_idle(self):
        """Update the output view."""
        settings = self.view.settings()
        output_view = find_view(settings.get(OUTPUT_VIEW_ID))
        output_view.run_command('output_view_update', {'input_view_id': self.view.id()})

    def on_modified_async(self):
        self.pending = self.pending + 1

        # Ask for on_timeout to be called when timeout has expired.
        settings = sublime.load_settings('PythonLiveCoding.sublime-settings')
        timeout = settings.get('timout_duration', 300)
        sublime.set_timeout(functools.partial(self.on_timeout), timeout)