import re
import sys
import traceback

class ReportBuilder(object):
    def __init__(self, message_limit=None):
        self.messages = []
        self.message_count = 0
        self.message_limit = message_limit
        self.stack_block = None #(first_line, last_line) numbers, not indexes
        self.stack = [] # current call stack
        self.history = [] # all stack frames that need to be combined
        self.line_widths = {}
        self.max_width = None
        self.frame_width = 0
        
    def start_block(self, first_line, last_line):
        """ Cap all the lines from first_line to last_line inclusive with
        pipes if they need it. They don't need it if they are all empty, or
        if they all end in pipes. first_line and last_line are line numbers,
        not indexes.
        Return the maximum width of all the lines.
        """
        
        self._check_line_count(last_line)
        line_indexes = range(first_line-1, last_line)
        max_width = 0
        all_end_in_pipes = True
        for line_index in line_indexes:
            message = self.messages[line_index]
            max_width = max(len(message), max_width)
            all_end_in_pipes = all_end_in_pipes and message.endswith('| ')
        if max_width and not all_end_in_pipes:
            for line_index in line_indexes:
                message = self.messages[line_index]
                self.messages[line_index] = message.ljust(max_width) + '| '
                self._update_frame_width(max_width + 2, line_index+1)
        else:
            self._increment_message_count()
        return max_width
    
    def _update_frame_width(self, new_width, line_number):
        if not self.max_width:
            # no limit.
            return
        if new_width > self.frame_width or not self.stack_block:
            should_throw = False
            if not self.stack_block:
                self.line_widths[line_number-1] = new_width
                should_throw = new_width > self.max_width
            else:
                first_line, last_line = self.stack_block
                for line_index in range(first_line - 1, last_line):
                    line_width = (
                        self.line_widths.get(line_index, 0) + 
                        new_width - 
                        self.frame_width)
                    if line_width > self.max_width:
                        should_throw = True
                    self.line_widths[line_index] = line_width
                self.frame_width = new_width
            if should_throw:
                raise RuntimeError('live coding message limit exceeded')
            
    def start_frame(self, first_line, last_line):
        new_frame = ReportBuilder()
        new_frame.stack_block = (first_line, last_line)
        new_frame.line_widths = self.line_widths
        new_frame.max_width = self.max_width
        self.history.append(new_frame)
        return new_frame
    
    def _increment_message_count(self):
        if (self.message_limit is not None and 
            self.message_count >= self.message_limit):
            
            raise RuntimeError('live coding message limit exceeded')
        self.message_count += 1

    def add_message(self, message, line_number):
        """ Add a message to the report on line line_number (1-based). """
        self._increment_message_count()
        self._check_line_count(line_number)
        new_width = len(self.messages[line_number - 1]) + len(message)
        self._update_frame_width(new_width, line_number)
        self.messages[line_number - 1] += message
            
    def add_extra_message(self, message, line_number):
        """ Add an extra message to the last frame after the code has finished
        running. """
        
        target = self.history[-1] if self.history else self
        target.max_width = self.max_width
        target.add_message(message, line_number)

    def assign(self, name, value, line_number):
        display = repr(value)
        if not display.startswith('<'):
            if '\n' in display:
                display = re.sub(r'\s+', ' ', display)
            self.add_message('%s = %s ' % (name, display), line_number)
        return value
    
    def exception(self):
        etype, value, tb = sys.exc_info()
        messages = traceback.format_exception_only(etype, value)
        message = messages[-1].strip() + ' '
        entries = traceback.extract_tb(tb)
        if entries:
            _, line_number, _, _ = entries[0]
            self.add_message(message, line_number)
    
    def return_value(self, value, line_number):
        self.add_message('return %s ' % repr(value), line_number)
        return value
        
    def yield_value(self, value, line_number):
        if isinstance(value, tuple):
            display = ', '.join([repr(item) for item in value])
        else:
            display = repr(value)
        self.add_message('yield %s ' % display, line_number)
        return value
        
    def record_call(self, 
                    name, 
                    display_before, 
                    result, 
                    display_after, 
                    line_number):
        if display_before != display_after:
            self.add_message('%s = %s ' % (name, display_after), line_number)
        return result
    
    def report(self):
        for frame in self.history:
            first_line, last_line = frame.stack_block
            self.start_block(first_line, last_line)
            for i in range(len(frame.messages)):
                message = frame.messages[i]
                if message:
                    line_number = i+1
                    self.add_message(message, line_number)
        self.history = []
        return '\n'.join(self.messages)

    def _check_line_count(self, line_count):
        while len(self.messages) < line_count:
            self.messages.append('')

