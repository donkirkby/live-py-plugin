class ReportBuilder(object):
    def __init__(self):
        self.messages = []
        self.previous_line = 0
        
    def start_block(self, first_line, last_line):
        self._check_line_count(last_line)
        line_indexes = range(first_line-1, last_line)
        width = 0
        for line_index in line_indexes:
            message = self.messages[line_index]
            width = max(len(message), width)
        if width:
            for line_index in line_indexes:
                message = self.messages[line_index]
                self.messages[line_index] = message.ljust(width) + '| '
    
    def add_message(self, message, line_number):
        self._check_line_count(line_number)
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

