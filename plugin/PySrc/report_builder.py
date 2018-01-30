import re

import os
import sys
import traceback


class ReportBuilder(object):
    def __init__(self, message_limit=None):
        self.messages = []
        self.assignments = []
        self.message_count = 0
        self.message_limit = message_limit
        self.stack_block = None  # (first_line, last_line) numbers, not indexes
        self.history = []  # all stack frames that need to be combined
        self.line_widths = {}
        self.max_width = None
        self.frame_width = 0
        self.is_muted = False
        self.current_output = ''
        self.current_output_line = None
        self.current_output_is_stderr = False
        self.has_print_function = True

    def start_block(self, first_line, last_line):
        """ Cap all the lines from first_line to last_line inclusive with
        pipes if they need it. They don't need it if they are all empty, or
        if they all end in pipes. first_line and last_line are line numbers,
        not indexes.
        Return the maximum width of all the lines.
        """

        self._check_line_count(last_line)
        self.check_output()
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
        if self.is_muted:
            return self
        for frame in self.history:
            if frame.is_muted:
                return frame
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
        self._increment_message_count()
        if self.is_muted:
            return
        if '\n' in message:
            message = re.sub(r'\s+', ' ', message)

        self.check_output()
        self._check_line_count(line_number)
        new_width = len(self.messages[line_number - 1]) + len(message)
        self._update_frame_width(new_width, line_number)
        self.messages[line_number - 1] += message

    def check_output(self):
        if self.current_output:
            if self.current_output_is_stderr:
                template = 'sys.stderr.write({}) '
            elif self.current_output.endswith('\n'):
                self.current_output = self.current_output[:-1]
                template = 'print({}) ' if self.has_print_function else 'print {} '
            else:
                template = 'sys.stdout.write({}) '
            print_message = template.format(self.get_repr(self.current_output))
            self.current_output = ''
            print_line = self.current_output_line
            self.current_output_line = None
            self.add_message(print_message, print_line)

    def add_output(self, text, line_number, has_print_function=True, is_stderr=False):
        if (line_number != self.current_output_line or
                is_stderr != self.current_output_is_stderr):
            self.check_output()
        self.current_output += text
        self.current_output_line = line_number
        self.current_output_is_stderr = is_stderr
        self.has_print_function = has_print_function

    def add_extra_message(self, message, line_number):
        """ Add an extra message to the last frame after the code has finished
        running. """

        target = self.history[-1] if self.history else self
        target.max_width = self.max_width
        target.add_message(message, line_number)

    def get_repr(self, value):
        """ Get the representation of an object without reporting the call. """
        if self.is_muted:
            return ''
        start_count = self.message_count
        self.is_muted = True
        try:
            repr_text = repr(value)
        finally:
            self.is_muted = False
            self.message_count = start_count
        max_width = 80
        if len(repr_text) > max_width:
            half_width = max_width//2 - 5
            full_width = half_width * 2
            repr_text = "{}[{} chars]{}".format(repr_text[:half_width],
                                                len(repr_text) - full_width,
                                                repr_text[-half_width:])
        return repr_text

    def assign(self, name, value, line_number):
        """ Convenience method for simple assignments.

        Just wraps all the separate steps for a flexible assignment. """

        self.start_assignment()
        try:
            self.set_assignment_value(value)
            self.report_assignment('{} = {{}}'.format(name),
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
        # noinspection PyBroadException
        try:
            display = format_string.format(*(assignment.indexes +
                                             [self.get_repr(assignment.value)]))
        except Exception:
            display = None
        self.start_block(line_number, line_number)
        if display is not None and not display.endswith('>'):
            self.add_message(display + ' ', line_number)

    def report_lambda(self, first_line, last_line, *args):
        """ Report lambda parameters and result, and return result.

        :param first_line: where to display the message
        :param last_line: range for the block markers
        :param args: parameter values, followed by the result.
        """
        params = ', '.join(self.get_repr(arg) for arg in args[:-1])
        result = args[-1]
        self.start_block(first_line, last_line)
        self.add_message('({} => {}) '.format(params, self.get_repr(result)), first_line)
        return result

    def exception(self):
        etype, value, tb = sys.exc_info()
        messages = traceback.format_exception_only(etype, value)
        message = messages[-1].strip() + ' '
        entries = traceback.extract_tb(tb)
        if entries:
            _, line_number, _, _ = entries[0]
            old_limit, self.message_limit = self.message_limit, None
            old_width, self.max_width = self.max_width, None
            try:
                self.add_message(message, line_number)
            finally:
                self.message_limit = old_limit
                self.max_width = old_width

    def return_value(self, value, line_number):
        self.add_message('return %s ' % self.get_repr(value), line_number)
        return value

    def yield_value(self, value, line_number):
        if isinstance(value, tuple):
            display = ', '.join([self.get_repr(item) for item in value])
        else:
            display = self.get_repr(value)
        self.add_message('yield %s ' % display, line_number)
        return value

    def yield_from(self, values, line_number):
        for value in values:
            self.start_block(line_number, line_number)
            yield self.yield_value(value, line_number)

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

    def report(self, total_lines=0):
        self.check_output()
        self.max_width = None
        self.message_limit = None
        for frame in self.history:
            frame.check_output()
            first_line, last_line = frame.stack_block
            self.start_block(first_line, last_line)
            for i in range(len(frame.messages)):
                message = frame.messages[i]
                if message:
                    line_number = i+1
                    self.add_message(message, line_number)
        self.history = []
        self._check_line_count(total_lines)
        return '\n'.join(self.messages)

    def _check_line_count(self, line_count):
        while len(self.messages) < line_count:
            self.messages.append('')

    def count_all_messages(self):
        history_count = sum(frame.message_count for frame in self.history)
        return self.message_count + history_count


class DeletionTarget(object):
    def __init__(self, name, target, line_number, report_builder):
        self.name = name
        self.target = target
        self.line_number = line_number
        self.report_builder = report_builder

    def __delitem__(self, key):
        before = self.report_builder.get_repr(self.target)
        del self.target[key]
        after = self.report_builder.get_repr(self.target)
        if before != after:
            self.report_builder.assign(self.name,
                                       self.target,
                                       self.line_number)

    def __delattr__(self, key):
        before = self.report_builder.get_repr(self.target)
        self.target.__delattr__(key)
        after = self.report_builder.get_repr(self.target)
        if before != after:
            self.report_builder.assign(self.name,
                                       self.target,
                                       self.line_number)


class AssignmentReport(object):
    def __init__(self):
        self.value = None
        self.indexes = []
