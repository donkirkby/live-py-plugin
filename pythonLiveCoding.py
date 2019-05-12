import sys
import logging
import functools
import subprocess

import sublime, sublime_plugin


XMIN, YMIN, XMAX, YMAX = list(range(4))
LC_TARGET_VIEW_ID = 'lc_tgt_view_id'


logging.basicConfig(level=logging.INFO)


def find_view(view_id):
    for window in sublime.windows():
        for view in window.views():
            if view.id() == view_id:
                return view
    return None


class BaseWindowCommand(sublime_plugin.WindowCommand):

    def fixedSetLayout(self, window, layout):
        #A bug was introduced in Sublime Text 3, sometime before 3053, in that it
        #changes the active group to 0 when the layout is changed. Annoying.
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


class ResetCommand(BaseWindowCommand):

    def run(self):
        
        # Close any target views and remove their tag.
        for view in self.window.views():
            if not view.settings().has(LC_TARGET_VIEW_ID):
                continue
            tgt_view_id = view.settings().get(LC_TARGET_VIEW_ID)
            tgt_view = find_view(tgt_view_id)
            if tgt_view is not None:
               print('closing:', tgt_view.name())
               tgt_view.close()
            view.settings().erase(LC_TARGET_VIEW_ID)

        # Set the layout back to a single group.
        rows, cols, cells = self.get_layout()
        self.fixedSetLayout(self.window, {
            'cols': [0, 1], 
            'rows': [0, 1], 
            'cells': [[0, 0, 1, 1]]
        })


class StartCommand(BaseWindowCommand):

    def create_pane(self):
        
        rows, cols, cells = self.get_layout()
        active_group = self.window.active_group()

        old_cell = cells[active_group]

        cols.insert(old_cell[XMAX], (cols[old_cell[XMIN]] + cols[old_cell[XMAX]]) / 2)
        new_cell = [old_cell[XMAX], old_cell[YMIN], old_cell[XMAX] + 1, old_cell[YMAX]]
        cells.append(new_cell)

        self.fixedSetLayout(self.window, {
            'cols': cols, 
            'rows': rows, 
            'cells': cells
        })

        # Create a new file in the new group, then switch active group back.
        self.window.focus_group(len(cells) - 1)
        tgt_view = self.window.new_file()
        self.window.focus_group(active_group)
        
        return tgt_view

    def run(self):

        active_group = self.window.active_group()
        src_view = self.window.active_view_in_group(active_group)
        if src_view.settings().has(LC_TARGET_VIEW_ID):
            msg = 'Already a live coding session for the current view.'
            sublime.message_dialog(msg)
            return

        tgt_view = self.create_pane()

        src_view.settings().set('word_wrap', False)
        src_view.settings().set(LC_TARGET_VIEW_ID, tgt_view.id())
        
        tgt_view.set_scratch(True)
        tgt_view.settings().set('word_wrap', False)
        tgt_view.set_name('live coding output')
        tgt_view.run_command('target_view_replace', {'src_view_id': src_view.id()})
 

class TargetViewReplaceCommand(sublime_plugin.TextCommand):

    def trace_code(self, contents):

        # Pull location of python exe and code_tracer.py script from user
        # settings.
        settings = sublime.load_settings('PythonLiveCoding.sublime-settings')
        py_path = settings.get('python_executable')
        tracer_path = settings.get('code_tracer')
        args = [
            py_path, 
            tracer_path,
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
        return out

    def run(self, edit, src_view_id=None):
        src_view = find_view(src_view_id)
        contents = src_view.substr(sublime.Region(0, src_view.size()))
        code_report = self.trace_code(contents)
        self.view.replace(edit, sublime.Region(0, self.view.size()), code_report)

 
class SourceViewEventListener(sublime_plugin.ViewEventListener):

    """
    Will execute live coding update once the timeout expires. Based on the 
    IdleWatcher example here: http://www.sublimetext.com/docs/plugin-examples
    """

    pending = 0

    @classmethod
    def is_applicable(cls, settings):
        return settings.has(LC_TARGET_VIEW_ID)

    def handleTimeout(self):
        self.pending = self.pending - 1
        if self.pending == 0:

            # There are no more queued up calls to handleTimeout, so it must 
            # have been 1000ms since the last modification.
            self.onIdle()

    def onIdle(self):
        settings = sublime.load_settings('PythonLiveCoding.sublime-settings')
        timeout = settings.get('timout_duration', 300)
        logging.getLogger().info('Window idle for: {}ms - updating live coding output'.format(timeout))
        tgt_view = find_view(self.view.settings().get(LC_TARGET_VIEW_ID))
        tgt_view.run_command('target_view_replace', {'src_view_id': self.view.id()})

    def on_modified_async(self):
        self.pending = self.pending + 1

        # Ask for handleTimeout to be called when timeout has expired.
        settings = sublime.load_settings('PythonLiveCoding.sublime-settings')
        timeout = settings.get('timout_duration', 300)
        sublime.set_timeout(functools.partial(self.handleTimeout), timeout)