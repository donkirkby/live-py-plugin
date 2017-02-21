import re
import sys
import traceback


class ReportBuilder(object):
    def __init__(self, message_limit=None):
        self.messages = []
        self.assignments = []
        self.message_count = 0
        self.message_limit = message_limit
        self.stack_block = None  # (first_line, last_line) numbers, not indexes
        self.stack = []  # current call stack
        self.history = []  # all stack frames that need to be combined
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
        """ Start a new stack frame to support recursive calls.

        This allows extra messages to be added to a stack frame after a
        recursive call finishes.
        :param int first_line: the first line of the function that the frame is
        running.
        :param int last_line: the last line of the function that the frame is
        running.
        """
        new_frame = ReportBuilder(self.message_limit)
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
        if '\n' in message:
            message = re.sub(r'\s+', ' ', message)
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
        """ Convenience method for simple assignments.

        Just wraps all the separate steps for a flexible assignment. """

        self.start_assignment()
        try:
            self.set_assignment_value(value)
            self.report_assignment('{} = {{!r}}'.format(name),
                                   line_number=line_number)
        finally:
            self.end_assignment()
        return value

    def start_assignment(self):
        self.assignments.append(AssignmentReport())

    def end_assignment(self):
        self.assignments.pop()

    def set_assignment_value(self, value):
        self.assignments[-1].value = value
        return value

    def add_assignment_index(self, value):
        self.assignments[-1].indexes.append(value)
        return value

    def get_assignment_index(self, index_index):
        return self.assignments[-1].indexes[index_index]

    def report_assignment(self, format_string, line_number):
        assignment = self.assignments[-1]
        try:
            display = format_string.format(*(assignment.indexes +
                                             [assignment.value]))
        except Exception:
            display = None
        if display is not None and not display.endswith('>'):
            self.add_message(display + ' ', line_number)

    def exception(self):
        etype, value, tb = sys.exc_info()
        messages = traceback.format_exception_only(etype, value)
        message = messages[-1].strip() + ' '
        entries = traceback.extract_tb(tb)
        if entries:
            _, line_number, _, _ = entries[0]
            try:
                old_limit, self.message_limit = self.message_limit, None
                old_width, self.max_width = self.max_width, None
                self.add_message(message, line_number)
            finally:
                self.message_limit = old_limit
                self.max_width = old_width

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
                    names,
                    displays_before,
                    result,
                    displays_after,
                    line_number):
        zipped = zip(names, displays_before, displays_after)
        for name, display_before, display_after in zipped:
            if display_before != display_after:
                self.add_message('%s = %s ' % (name, display_after),
                                 line_number)
        return result

    def record_delete(self, name, target, line_number):
        return DeletionTarget(name, target, line_number, self)

    def report(self):
        self.max_width = None
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


class DeletionTarget(object):
    def __init__(self, name, target, line_number, report_builder):
        self.name = name
        self.target = target
        self.line_number = line_number
        self.report_builder = report_builder

    def __delitem__(self, key):
        before = repr(self.target)
        del self.target[key]
        after = repr(self.target)
        if before != after:
            self.report_builder.assign(self.name,
                                       self.target,
                                       self.line_number)

    def __delattr__(self, key):
        before = repr(self.target)
        self.target.__delattr__(key)
        after = repr(self.target)
        if before != after:
            self.report_builder.assign(self.name,
                                       self.target,
                                       self.line_number)


class AssignmentReport(object):
    def __init__(self):
        self.value = None
        self.indexes = []
