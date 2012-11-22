class ReportBuilder(object):
    def __init__(self, message_limit=None):
        self.messages = []
        self.message_count = 0
        self.message_limit = message_limit
        self.stack_block = None
        self.stack = [] # current call stack
        self.history = [] # all stack frames that need to be combined
        
    def start_block(self, first_line, last_line):
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
        else:
            self._increment_message_count()
            
    def start_frame(self, first_line, last_line):
        new_frame = ReportBuilder()
        new_frame.stack_block = (first_line, last_line)
        self.history.append(new_frame)
        return new_frame
    
    def _increment_message_count(self):
        if (self.message_limit is not None and self.message_count >= self.message_limit):
            raise RuntimeError('live coding message limit exceeded')
        self.message_count += 1

    def add_message(self, message, line_number):
        """ Add a message to the report on line line_number (1-based). """
        self._increment_message_count()
        self._check_line_count(line_number)
        self.messages[line_number - 1] += message
            
    def add_extra_message(self, message, line_number):
        """ Add an extra message to the last frame after the code has finished
        running. """
        
        target = self.history[-1] if self.history else self
        target.add_message(message, line_number)

    def assign(self, name, value, line_number):
        display = repr(value)
        if not display.startswith('<'):
            self.add_message('%s = %r ' % (name, value), line_number)
        return value
    
    def return_value(self, value, line_number):
        self.add_message('return %r ' % value, line_number)
        return value
        
    def yield_value(self, value, line_number):
        self.add_message('yield %r ' % value, line_number)
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

