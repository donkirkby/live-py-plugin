class ReportBuilder(object):
    def __init__(self, message_limit=None):
        self.messages = []
        self.message_count = 0
        self.message_limit = message_limit
        self.limited_line_number = None
        
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
    
    def add_message(self, message, line_number):
        if      (self.message_limit is not None and 
                 self.message_count >= self.message_limit):
            self.limited_line_number = line_number
            raise RuntimeError('live coding message limit exceeded')
        self._check_line_count(line_number)
        self.message_count += 1
        self.messages[line_number - 1] += message

    def assign(self, name, value, line_number):
        self.add_message('%s = %r ' % (name, value), line_number)
    
    def return_value(self, value, line_number):
        self.add_message('return %r ' % value, line_number)
    
    def report(self):
        return '\n'.join(self.messages)

    def _check_line_count(self, line_count):
        while len(self.messages) < line_count:
            self.messages.append('')

