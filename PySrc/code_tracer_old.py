import copy
import sys
import traceback
import types

class CodeTracer(object):
    def __init__(self, parent=None):
        self.previous_line = 0
        self.report = parent.report if parent else []
        self.log = parent.log if parent else []
        self.locals = dict()
        self.parent = parent
        self.exception = None

    def _add_line_message(self, line_index, message):
        while line_index >= len(self.report):
            self.report.append("")
        
        self.report[line_index] += message

    def _update_locals(self, frame, line_index):
        """ Check for changes in local variables, and record those changes as
            occurring on the report entry for line_index.
        """
        
        for k, v in frame.f_locals.iteritems():
            is_ignored = (isinstance(v, types.FunctionType) or
                          isinstance(v, types.TypeType) or
                          k == '__builtins__')
            if self.locals.get(k) != v and not is_ignored:
                message = "%s = %r " % (k, v)
                self._add_line_message(line_index, message)
                self._log_message("%d: %s = %r" % (self.previous_line, k, v))
                self.locals[k] = copy.deepcopy(v)


    def _mark_loop(self, frame):
        loop_start = frame.f_lineno - 1
        loop_bound = self.previous_line
        iteration = self.report[loop_start].count('|')
        blank = ''.join([(c if c == '|' else ' ') for c in self.report[loop_start]])
        blank = blank.rstrip() + ' '
        self._add_line_message(loop_bound - 1, "")
        width = 0
        for j in range(loop_start, loop_bound):
            if self.report[j].count('|') < iteration:
                self.report[j] = blank + self.report[j]
            width = max(width, len(self.report[j]))
        
        for j in range(loop_start, loop_bound):
            self.report[j] = self.report[j].ljust(width) + '| '


    def _log_event(self, frame, event, arg):
        if event == 'exception':
            message = self._format_exception(arg)
        else:
            message = repr(arg)
        self._log_message("%d: %s %s" % (frame.f_lineno, event, message))

    def _log_message(self, message):
        self.log.append(message)


    def _format_exception(self, arg):
        exception_type, value = arg[:2]
        message = exception_type.__name__
        if value:
            message += ' ' + str(value)
        return message

    def dump_frame(self, frame, event, arg):
        if frame.f_globals.get('__name__') is not None:
            return self.dump_frame
#        self._log_message('not skipping on filename ' + repr(frame.f_code.co_filename))
        
        if event == 'call':
            self._log_event(frame, event, arg)
#            line_index = frame.f_lineno - 1
#            self._update_locals(frame, line_index)
            child = CodeTracer(self)
            child.previous_line = frame.f_lineno
            return child.dump_frame
        
        line_index = self.previous_line - 1
        if line_index >= 0:
            self._update_locals(frame, line_index)
            
        self._log_event(frame, event, arg)
        
        if event == 'exception':
            self.exception = arg
            if self.parent:
                self.parent.exception = arg
            message = self._format_exception(arg) + ' '
            self._add_line_message(frame.f_lineno - 1, message)
        
        if event == 'return':
            if arg is not None:
                line_index = frame.f_lineno - 1
                self._add_line_message(line_index, 'return %r ' % arg)
            return self.parent.dump_frame

        if frame.f_lineno < self.previous_line:
            self._mark_loop(frame)
        self.previous_line = frame.f_lineno
        return self.dump_frame
    
    def trace_code(self, code):
        original_trace = sys.gettrace()
        try:
            try:
                sys.settrace(self.dump_frame)
                exec code in dict()
            
            finally:
                sys.settrace(original_trace)
        except:
            if self.exception is not None:
                pass # it should have been recorded in log and report.
            else:
                exc_info = sys.exc_info()
                try:
                    etype, value = exc_info[:2]
                    messages = traceback.format_exception_only(etype, value)
                    self._add_line_message(
                        value.lineno - 1, 
                        messages[-1].strip() + ' ')
                finally:
                    del exc_info # prevents circular reference
        return '\n'.join(self.report)
    
if __name__ == '__main__':
    code = sys.stdin.read()
    
    print CodeTracer().trace_code(code)