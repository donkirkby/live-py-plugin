import re

import sys
import traceback
from unittest.mock import Mock
from enum import Enum
from itertools import groupby


class ReportBuilder(object):
    is_tracing_next_block = False
    is_using_traced_blocks = False
    hide = None

    def __init__(self, message_limit=None):
        self.messages = []
        self.assignments = []
        self.message_count = self.event_count = 0
        self.message_limit = message_limit
        self.stack_block = None  # (first_line, last_line) numbers, not indexes
        self.history = []  # all stack frames that need to be combined
        self.line_widths = {}
        self.max_width = None
        self.frame_width = 0
        self.is_muted = False  # Used to mute messages during some repr() calls.
        self.current_output = ''
        self.current_output_line = None
        self.current_output_target_name = None
        self.has_print_function = True
        self.current_exception = None
        self.is_decorated = False
        self.root_frame = None
        self.minimum_blocks = set()  # {(first_line, last_line)}
        self.extra_blocks = set()  # {(first_line, last_line)}

        # Updated after calling report().
        self.reported_blocks = set()  # {(first_line, last_line)}

    def trace_block(self, first_line, last_line, is_minimum=False):
        """ Mark a block for tracing, and mute everything else.

        :param int first_line: start the block on this line (1-based)
        :param int last_line: end the block on this line (1-based, included)
        :param bool is_minimum: True if this is just a minimum set of lines,
            and other lines should not be muted.
        """
        if is_minimum:
            self.minimum_blocks.add((first_line, last_line))
        else:
            self.reported_blocks.add((first_line, last_line))

    def trace_extra_block(self, first_line, last_line):
        """ Mark a block for tracing, in addition to main reported blocks.

        :param int first_line: start the block on this line (1-based)
        :param int last_line: end the block on this line (1-based, included)
        """
        self.extra_blocks.add((first_line, last_line))

    def start_block(self, first_line, last_line):
        """ Cap all the lines from first_line to last_line inclusive with
        pipes if they need it. They don't need it if they are all empty, or
        if they all end in pipes. first_line and last_line are line numbers,
        not indexes.
        Return the maximum width of all the lines.
        """
        self.check_tracing(first_line, last_line)

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
            self._increment_message_count(first_line)
        return max_width

    def check_tracing(self, first_line, last_line):
        if ReportBuilder.is_tracing_next_block:
            ReportBuilder.is_tracing_next_block = False
            root_frame = self.root_frame or self
            root_frame.trace_block(first_line, last_line)

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
        self.check_tracing(first_line, last_line)
        if self.is_muted:
            return self
        for frame in self.history:
            if frame.is_muted:
                return frame
        new_frame = ReportBuilder(self.message_limit)
        new_frame.root_frame = self
        new_frame.stack_block = (first_line, last_line)
        new_frame.line_widths = self.line_widths
        new_frame.max_width = self.max_width
        self.history.append(new_frame)
        return new_frame

    def _increment_message_count(self, line_number):
        """ Keep track of how many traced messages and other events occur.

        :param line_number: the line number where a message could be added.
        :return: True if the message should be added because everything is
            being traced, or that line number is one of the lines being traced.
        :raise: RuntimeError if the message limit or event limit has been
            exceeded.
        """
        if (self.message_limit is not None and
                self.event_count >= self.message_limit * 1000):
            raise RuntimeError('live coding event limit exceeded')
        self.event_count += 1
        if not self.is_line_traced(line_number):
            return False
        if (self.message_limit is not None and
                self.message_count >= self.message_limit):

            raise RuntimeError('live coding message limit exceeded')
        self.message_count += 1
        return True

    def add_message(self, message, line_number):
        """ Add a message to the report on line line_number (1-based). """
        is_traced = self._increment_message_count(line_number)
        if not is_traced:
            return
        if self.is_muted:
            return
        if '\n' in message:
            message = re.sub(r'\s+', ' ', message)

        self.check_output()
        self._check_line_count(line_number)
        new_width = len(self.messages[line_number - 1]) + len(message)
        self._update_frame_width(new_width, line_number)
        self.messages[line_number - 1] += message

    def is_line_traced(self, line_number):
        if not ReportBuilder.is_using_traced_blocks:
            return True
        if self.root_frame is not None:
            return self.root_frame.is_line_traced(line_number)
        for first_line, last_line in self.reported_blocks:
            if first_line <= line_number <= last_line:
                return True
        return False

    def check_output(self):
        if self.current_output:
            if (self.current_output_target_name is None and
                    self.current_output.endswith('\n')):
                self.current_output = self.current_output[:-1]
                template = 'print({}) ' if self.has_print_function else 'print {} '
            else:
                target_name = self.current_output_target_name or 'sys.stdout'
                template = target_name + '.write({}) '
            print_message = template.format(self.get_repr(self.current_output))
            self.current_output = ''
            print_line = self.current_output_line
            self.current_output_line = None
            self.add_message(print_message, print_line)

    def add_output(self, text, line_number, has_print_function=True, target_name=None):
        if (line_number != self.current_output_line or
                target_name != self.current_output_target_name):
            self.check_output()
        self.current_output += text
        self.current_output_line = line_number
        self.current_output_target_name = target_name
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
        # noinspection PyBroadException
        try:
            repr_text = repr(value)
        except Exception:
            repr_text = '<{} object>'.format(value.__class__.__name__)
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
        if isinstance(self.hide, str):
            if format_string.startswith(self.hide + ' ='):
                return
        elif self.hide:
            try:
                for hide in self.hide:
                    if format_string.startswith(hide + ' ='):
                        return
            except TypeError:
                # User passed in something weird. Ignore it.
                pass
        assignment = self.assignments[-1]
        # noinspection PyBroadException
        try:
            if isinstance(assignment.value, Mock):
                # noinspection PyProtectedMember
                mock_name = assignment.value._mock_name
                args = '' if mock_name is None else 'name={!r}'.format(mock_name)
                value_repr = '{}({})'.format(
                    assignment.value.__class__.__name__,
                    args)
            else:
                value_repr = self.get_repr(assignment.value)

            display = format_string.format(*(assignment.indexes +
                                             [value_repr]))
            if isinstance(assignment.value, Enum):
                # Leave the representation of enums alone.
                pass
            elif display.endswith('>'):
                display = None
        except Exception:
            display = None
        if display is not None:
            self.start_block(line_number, line_number)
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

    def exception(self, line_number=None):
        etype, value, tb = sys.exc_info()
        messages = traceback.format_exception_only(etype, value)
        message = messages[-1].strip() + ' '
        if line_number is None:
            if value is self.current_exception:
                return
            self.current_exception = value
            entries = traceback.extract_tb(tb)
            if entries:
                _, line_number, _, _ = entries[0]
        if line_number is not None:
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

    def report(self, total_lines=None):
        self.check_output()
        self.max_width = None
        self.message_limit = None
        if self.is_using_traced_blocks and not self.reported_blocks:
            self.trace_block(1, 3)
            old_limit = self.message_limit
            self.message_limit = None
            message = 'Traced blocks were never called.'
            border = '-' * len(message) + ' |'
            self.add_message(border, 1)
            self.add_message(message + ' |', 2)
            self.add_message(border, 3)
            self.message_limit = old_limit
        traced_blocks = set(self.reported_blocks)
        for frame in self.history:
            frame.check_output()
            first_line, last_line = frame.stack_block
            traced_blocks.update(frame.reported_blocks)
            self.start_block(first_line, last_line)
            for i in range(len(frame.messages)):
                message = frame.messages[i]
                if message:
                    line_number = i+1
                    self.add_message(message, line_number)
        self.history = []
        if traced_blocks:
            traced_blocks |= self.extra_blocks
        else:
            end = len(self.messages)
            if self.minimum_blocks:
                end = max(end,
                          *(block_end
                            for block_start, block_end in self.minimum_blocks))
            traced_blocks = [(1, end)]

        # Remove overlaps from blocks
        traced_lines = set()
        for first_line, last_line in traced_blocks:
            if first_line is None:
                first_line = 1
            if last_line is None:
                last_line = len(self.messages)
            traced_lines.update(range(first_line, last_line+1))
        sorted_blocks = []
        # noinspection PyTypeChecker
        for diff, grouped_lines in groupby(enumerate(sorted(traced_lines)),
                                           lambda item: item[1]-item[0]):
            index, first_line = next(grouped_lines)
            last_line = first_line
            for index, last_line in grouped_lines:
                pass
            sorted_blocks.append((first_line, last_line))

        reported_messages = []
        self.reported_blocks = sorted_blocks
        for first_line, last_line in self.reported_blocks:
            if total_lines:
                reported_messages.extend([''] * (first_line -
                                                 len(reported_messages) - 1))
            for line_number in range(first_line, last_line+1):
                try:
                    message = self.messages[line_number - 1]
                except IndexError:
                    message = ''
                reported_messages.append(message)
        if total_lines:
            reported_messages.extend([''] * (total_lines -
                                             len(reported_messages)))
        return '\n'.join(line.rstrip() for line in reported_messages)

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
