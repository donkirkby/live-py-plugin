class ReportBuilder(object):
    def __init__(self):
        self.messages = []
        self.previous_line = 0
        
    def start_block(self, first_line, last_line):
        while len(self.messages) < last_line:
            self.messages.append('')
        line_indexes = range(first_line-1, last_line)
        width = 0
        for line_index in line_indexes:
            message = self.messages[line_index]
            width = max(len(message), width)
        if width:
            for line_index in line_indexes:
                message = self.messages[line_index]
                self.messages[line_index] = message.ljust(width) + '| '
    
    def end_block(self):
        pass
    
    def assign(self, name, value, line_number):
        line_index = line_number - 1
        self.messages[line_index] += '%s = %r ' % (name, value)
    
    def add_message(self, message):
        line_index = self.previous_line - 1
        self.messages[line_index] += message + ' '
    
    def report(self):
        return '\n'.join(self.messages)