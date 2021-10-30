import unittest

from space_tracer.report_builder import ReportBuilder


def trim_report(report):
    if isinstance(report, list):
        if report and report[0].endswith('\n'):
            report = ''.join(report)
        else:
            report = '\n'.join(report)
    lines = report.splitlines()
    trimmed_lines = [line.rstrip() for line in lines]
    return '\n'.join(trimmed_lines) + '\n'


class ReportTestCase(unittest.TestCase):
    def setUp(self):
        super(ReportTestCase, self).setUp()
        self.addTypeEqualityFunc(str, self.assertMultiLineEqual)

    @staticmethod
    def assertReportEqual(report1, report2):
        __tracebackhide__ = True
        assert trim_report(report1) == trim_report(report2)


class ReportBuilderTest(ReportTestCase):
    def setUp(self):
        if not hasattr(self, 'assertRaisesRegex'):
            self.assertRaisesRegex = self.assertRaisesRegexp

    def test_assign(self):
        # SETUP
        expected_value = 5
        expected_report = """\

x = 5
"""

        # EXEC
        builder = ReportBuilder()
        value = builder.assign(name='x', value=expected_value, line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_value, value)

    def test_assign_object_without_repr(self):
        # SETUP
        expected_report = ''

        class ClassWithoutRepr(object):
            pass

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='x', value=ClassWithoutRepr(), line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_object_with_multiline_repr(self):
        # SETUP
        expected_report = 'm = MultilineClass(1, 2) '

        class MultilineClass(object):
            def __init__(self, x, y):
                self.x = x
                self.y = y

            def __repr__(self):
                return "MultilineClass(%r,\n%r)" % (self.x, self.y)

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='m', value=MultilineClass(1, 2), line_number=1)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_object_with_repr_exception(self):
        # SETUP
        expected_report = ''

        class BadReprClass(object):
            def __repr__(self):
                raise NotImplementedError()

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='m', value=BadReprClass(), line_number=1)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_with_indexes(self):
        # SETUP
        expected_report = "a[0][23] = b['x'] = 99 "

        # EXEC
        builder = ReportBuilder()
        builder.start_assignment()
        builder.set_assignment_value(99)
        builder.add_assignment_index(0)
        builder.add_assignment_index(23)
        builder.add_assignment_index('x')
        builder.report_assignment('a[{!r}][{!r}] = b[{!r}] = {}',
                                  line_number=1)
        builder.end_assignment()
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_value_after_indexes(self):
        # SETUP
        expected_report = "a[0][23] = 99 "

        # EXEC
        builder = ReportBuilder()
        builder.start_assignment()
        builder.add_assignment_index(0)
        builder.add_assignment_index(23)
        builder.set_assignment_value(99)
        builder.report_assignment('a[{!r}][{!r}] = {}',
                                  line_number=1)
        builder.end_assignment()
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_assign_function(self):
        # SETUP
        expected_report = ''

        def f(n):
            return n+1

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='f', value=f, line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_lambda(self):
        # SETUP
        expected_report = '(23 => 24)'
        min_line = max_line = 1

        # EXEC
        builder = ReportBuilder()
        y = list(map(lambda x: builder.report_lambda(min_line, max_line, x, x+1),
                     [23]))
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual([24], y)

    def test_lambda_multiple_params(self):
        # SETUP
        expected_report = "('a', 'b' => 'ab')"
        min_line = max_line = 1

        # EXEC
        builder = ReportBuilder()
        z = list(map(lambda x, y: builder.report_lambda(min_line, max_line, x, y, x+y),
                     ['a'],
                     ['b']))
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(['ab'], z)

    def test_lambda_multiple_calls(self):
        # SETUP
        expected_report = "('a', 'b' => 'ab') | (1, 2 => 3)"
        min_line = max_line = 1

        # EXEC
        builder = ReportBuilder()
        list(map(lambda x, y: builder.report_lambda(min_line, max_line, x, y, x+y),
                 ['a', 1],
                 ['b', 2]))
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_message_limit(self):
        # SETUP
        expected_report = """\
a = 10
b = 20
"""

        # EXEC
        builder = ReportBuilder(message_limit=4)
        builder.assign('a', 10, 1)
        builder.assign('b', 20, 2)
        with self.assertRaisesRegex(RuntimeError, r'message limit exceeded'):
            builder.assign('c', 30, 3)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_message_limit_with_empty_blocks(self):
        # SETUP
        expected_report = """\




"""

        # EXEC
        builder = ReportBuilder(message_limit=2)
        builder.start_frame(1, 5)
        builder.start_block(1, 3)
        builder.start_block(1, 3)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_return(self):
        # SETUP
        expected_value = 'bob'
        expected_report = """\

return 'bob'
"""

        # EXEC
        builder = ReportBuilder()
        value = builder.return_value(value=expected_value, line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_value, value)

    def test_return_multiline(self):
        # SETUP
        class Multiline(object):
            def __repr__(self):
                return 'Multiline(\n)'
        expected_value = Multiline()
        expected_report = """\

return Multiline( )
"""

        # EXEC
        builder = ReportBuilder()
        value = builder.return_value(value=expected_value, line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_value, value)

    def test_call(self):
        # SETUP
        expected_report = """\

a = [1, 2]
"""
        expected_result = 23

        # EXEC
        builder = ReportBuilder()
        result = builder.record_call(names=['a'],
                                     displays_before=['[2, 1]'],
                                     result=expected_result,
                                     displays_after=['[1, 2]'],
                                     line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_result, result)

    def test_call_multiple_changes(self):
        # SETUP
        expected_report = """\

a = [1, 2] b = 6
"""
        expected_result = 23

        # EXEC
        builder = ReportBuilder()
        result = builder.record_call(names=['a', 'b'],
                                     displays_before=['[2, 1]', '5'],
                                     result=expected_result,
                                     displays_after=['[1, 2]', '6'],
                                     line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_result, result)

    def test_call_no_change(self):
        # SETUP
        expected_report = ""
        expected_result = 23

        # EXEC
        builder = ReportBuilder()
        result = builder.record_call(names=['a'],
                                     displays_before=['[1, 2]'],
                                     result=expected_result,
                                     displays_after=['[1, 2]'],
                                     line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_result, result)

    def test_delete_item(self):
        # SETUP
        expected_report = "d = {'a': 1}"
        d = {'a': 1, 'b': 2}
        expected_d = {'a': 1}

        # EXEC
        builder = ReportBuilder()
        del builder.record_delete(name='d', target=d, line_number=1)['b']
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)
        self.assertEqual(expected_d, d)

    def test_multiple_lines(self):
        # SETUP
        expected_report = """\
a = 3

b = 'xyz' """

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='a', value=3, line_number=1)
        builder.assign(name='b', value="xyz", line_number=3)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_multiple_messages(self):
        # SETUP
        expected_report = """\
x = 1 | y = 2 """

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='x', value=1, line_number=1)
        builder.assign(name='y', value=2, line_number=1)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_multiple_visits(self):
        # SETUP
        expected_report = """\
x = 'first'    | x = 'second'
y = 'continue' | """

        # EXEC
        builder = ReportBuilder()
        builder.start_block(1, 2)
        builder.assign(name='x', value='first', line_number=1)
        builder.assign(name='y', value='continue', line_number=2)
        builder.start_block(1, 2)
        builder.assign(name='x', value='second', line_number=1)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_nested_blocks(self):
        # SETUP
        expected_report = """\
x = 'first' | x = 'second'
            | y = 1 | y = 2 """

        # EXEC
        builder = ReportBuilder()
        builder.start_block(1, 2)
        builder.assign(name='x', value='first', line_number=1)
        builder.start_block(1, 2)
        builder.assign(name='x', value='second', line_number=1)
        builder.start_block(2, 2)
        builder.assign(name='y', value=1, line_number=2)
        builder.start_block(2, 2)
        builder.assign(name='y', value=2, line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_separate_scopes(self):
        # SETUP
        expected_report = """\
x = 'child' | x = 'child again'

y = 'main' """

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='y', value='main', line_number=3)
        builder.start_block(1, 1)
        builder.assign(name='x', value='child', line_number=1)
        builder.start_block(1, 1)
        builder.assign(name='x', value='child again', line_number=1)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_frames(self):
        # SETUP
        expected_report = """\
i = 1 | i = 2
n = 2 | """

        # EXEC
        builder = ReportBuilder()
        frame1 = builder.start_frame(1, 2)
        frame1.assign(name='i', value=1, line_number=1)
        frame2 = builder.start_frame(1, 2)
        frame2.assign(name='i', value=2, line_number=1)
        frame1.assign(name='n', value=2, line_number=2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_frames_with_extra_message(self):
        # SETUP
        expected_report = """\
i = 1 | i = 2
      | extra message"""

        # EXEC
        builder = ReportBuilder()
        frame1 = builder.start_frame(1, 2)
        frame1.assign(name='i', value=1, line_number=1)
        frame2 = builder.start_frame(1, 2)
        frame2.assign(name='i', value=2, line_number=1)
        builder.add_extra_message('extra message', 2)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_decorated_frame(self):
        # SETUP
        expected_report = """\
i = 1 | i = 2
n = 2 | """

        # EXEC
        builder = ReportBuilder()
        builder.assign(name='i', value=0, line_number=2)
        ReportBuilder.is_tracing_next_block = True
        frame1 = builder.start_frame(3, 4)
        frame1.assign(name='i', value=1, line_number=3)
        ReportBuilder.is_tracing_next_block = True
        frame2 = builder.start_frame(3, 4)
        frame2.assign(name='i', value=2, line_number=3)
        frame1.assign(name='n', value=2, line_number=4)
        builder.assign(name='i', value=20, line_number=5)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_trace_block(self):
        # SETUP
        expected_report = """\
i = 1 | i = 2
n = 2 | """

        # EXEC
        builder = ReportBuilder()
        builder.trace_block(3, 4)
        builder.assign(name='i', value=0, line_number=2)
        frame1 = builder.start_frame(3, 4)
        frame1.assign(name='i', value=1, line_number=3)
        frame2 = builder.start_frame(3, 4)
        frame2.assign(name='i', value=2, line_number=3)
        frame1.assign(name='n', value=2, line_number=4)
        builder.assign(name='i', value=20, line_number=5)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_exception(self):
        # SETUP
        source = """\
try:
    raise RuntimeError('foo')
except:
    builder.exception()"""
        expected_report = """\

RuntimeError: foo
"""

        # EXEC
        builder = ReportBuilder()
        environment = dict(builder=builder)
        exec(source, environment, environment)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_exception_multiline_message(self):
        # SETUP
        source = """\
try:
    raise RuntimeError('a\\nb')
except:
    builder.exception()"""
        expected_report = """\

RuntimeError: a b
"""

        # EXEC
        builder = ReportBuilder()
        environment = dict(builder=builder)
        exec(source, environment, environment)
        report = builder.report()

        # VERIFY
        self.assertReportEqual(expected_report, report)

    def test_message_count(self):
        # SETUP

        # EXEC
        builder = ReportBuilder()
        builder.add_message('a', 1)
        builder.add_message('b', 2)

        # VERIFY
        self.assertEqual(2, builder.message_count)

    def test_count_all_messages(self):
        # SETUP

        # EXEC
        builder = ReportBuilder()
        frame = builder.start_frame(1, 2)
        frame.add_message('a', 1)
        frame.add_message('b', 2)

        # VERIFY
        self.assertEqual(0, builder.message_count)
        self.assertEqual(2, builder.count_all_messages())

    def test_output(self):
        # SETUP
        expected_report = """\
print('a')
"""

        # EXEC
        builder = ReportBuilder()
        builder.add_output('a\n', 1)

        # VERIFY
        self.assertReportEqual(expected_report, builder.report())

    def test_multiple_outputs(self):
        # SETUP
        expected_report = """\
print('a\\nb')
"""

        # EXEC
        builder = ReportBuilder()
        builder.add_output('a\n', 1)
        builder.add_output('b\n', 1)

        # VERIFY
        self.assertReportEqual(expected_report, builder.report())

    def test_multiple_outputs_different_lines(self):
        # SETUP
        expected_report = """\
print('a')

print('b')
"""

        # EXEC
        builder = ReportBuilder()
        builder.add_output('a\n', 1)
        builder.add_output('b\n', 3)

        # VERIFY
        self.assertReportEqual(expected_report, builder.report())

    def test_output_without_newline(self):
        # SETUP
        expected_report = """\
sys.stdout.write('a')
"""

        # EXEC
        builder = ReportBuilder()
        builder.add_output('a', 1)

        # VERIFY
        self.assertReportEqual(expected_report, builder.report())

    def test_output_stderr(self):
        # SETUP
        expected_report = """\
sys.stderr.write('a\\n')
"""

        # EXEC
        builder = ReportBuilder()
        builder.add_output('a\n', 1, target_name='sys.stderr')

        # VERIFY
        self.assertReportEqual(expected_report, builder.report())

    def test_format_stderr(self):
        # SETUP
        expected_report = "sys.stderr.write('abc') | x = None"

        # EXEC
        builder = ReportBuilder()
        builder.add_output('abc', 1, target_name='sys.stderr')
        builder.assign(name='x', value=None, line_number=1)

        # VERIFY
        self.assertReportEqual(expected_report, builder.report())


if __name__ == '__main__':
    unittest.main()
