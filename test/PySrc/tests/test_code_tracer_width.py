from space_tracer.main import replace_input, TraceRunner


def test_source_width_positive():
    code = """\
i = 1 + 1
"""
    expected_report = """\
i = 1 +  | i = 2"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--source_width', '8',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_source_width_negative():
    code = """\
i = 1 + 1
"""
    expected_report = """\
i = 1 + | i = 2"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--source_width', '-2',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_source_indent():
    code = """\
i = 1 + 1
"""
    expected_report = """\
    i = 1 + 1 | i = 2"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--source_indent', '4',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_source_indent_small():
    code = """\
i = 1 + 1
"""
    expected_report = """\
  i = 1 + 1 | i = 2"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--source_indent', '2',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_source_indent_negative():
    code = """\
i = 1 + 1
"""
    expected_report = """\
= 1 + 1 | i = 2"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--source_indent', '-2',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_trace_width():
    code = """\
i = 1 + 1
"""
    expected_report = """\
i = 1 + 1 | i ="""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--trace_width', '15',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_trace_width_negative():
    code = """\
i = 1 + 1
s = 'a' * 10
"""
    expected_report = """\
i = 1 + 1    | i = 2
s = 'a' * 10 | s = 'aaaaaa"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--trace_width', '-5',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_trace_width_without_source():
    code = """\
i = 1 + 1
s = 'a' * 10
"""
    expected_report = """\
i = 2
s = 'aaaaaa"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--source_width', '0',
                                              '--trace_width', '-5',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report


def test_trace_offset():
    code = """\
i = 1 + 1
s = 'a' * 10
"""
    expected_report = """\
i = 1 + 1    |  2
s = 'a' * 10 |  'aaaaaaaaaa'"""

    with replace_input(code):
        report = TraceRunner().trace_command(['space_tracer',
                                              '--trace_offset', '3',
                                              '--traced_file', 'foo.py'])

    assert report == expected_report
