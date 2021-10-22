import sys

import pytest

from space_tracer.main import TraceRunner
from test_report_builder import trim_report


def test_assignment():
    code = """\
i = 1
"""
    expected_report = """\
i = 1"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


@pytest.mark.skipif(
    sys.version_info < (3, 6),
    reason='type hints not available before Python 3.6')
def test_annotated_assignment():
    code = """\
i: int = 1
"""
    expected_report = """\
i = 1 """

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


@pytest.mark.skipif(
    sys.version_info < (3, 6),
    reason='type hints not available before Python 3.6')
def test_annotated_variable():
    code = """\
i: int
for i in range(2):
    pass
"""
    expected_report = """\

i = 0 | i = 1
      |"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_increment():
    code = """\
i = 1
i += 1
"""
    expected_report = """\
i = 1
i = 2 """

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_mutable():
    code = """\
a = [1, 2, [3, 4]]
a[0] = 9
a[2][1] = 8
b = a
i, j = 2, 1
a[i][j] = 7
a[0:2] = list(reversed(a[0:2]))
b = a
d = -1
a[i:j:d] = [100]
b = a
"""
    expected_report = """\
a = [1, 2, [3, 4]]
a[0] = 9
a[2][1] = 8
b = [9, 2, [3, 8]]
(i, j) = (2, 1)
a[2][1] = 7
a[0:2] = [2, 9]
b = [2, 9, [3, 7]]
d = -1
a[2:1:-1] = [100]
b = [2, 9, 100]
"""

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_mutable_increment():
    code = """\
a = [1, 2, [3, 4]]
a[0] += 9
a[2][1] += 8
b = a
i, j = 2, 1
a[i][j] += 7
b = a
"""
    expected_report = """\
a = [1, 2, [3, 4]]
a[0] = 10
a[2][1] = 12
b = [10, 2, [3, 12]]
(i, j) = (2, 1)
a[2][1] = 19
b = [10, 2, [3, 19]]
"""

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_slice():
    code = """\
a = [1, 2, 3, 4, 5]
i, j = 1, 4
a[i:j] = [20, 30]
b = a
a[2:] = [300]
b = a
a[:2] *= 2
b = a
a[:2] += [21]
b = a
"""
    expected_report = """\
a = [1, 2, 3, 4, 5]
(i, j) = (1, 4)
a[1:4] = [20, 30]
b = [1, 20, 30, 5]
a[2:] = [300]
b = [1, 20, 300]
a[:2] *= 2
b = [1, 20, 1, 20, 300]
a[:2] += [21]
b = [1, 20, 21, 1, 20, 300] """

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_slice_magic():
    """ All augmented assignments on slices, possible with mocks. """
    code = """\
from unittest.mock import MagicMock


class Foo(MagicMock):
    def __repr__(self):
        return 'Foo()'

foo = Foo()
foo[1] = 3
foo[1:10] = 3
foo[1:10:2] = 3
foo[...] = 3
foo[1, 2:3] = 3
foo[1:10] += 3
foo[1:10] -= 3
foo[1:10] *= 3
foo[1:10] /= 3
foo[1:10] //= 3
foo[1:10] %= 3
foo[1:10] **= 3
foo[1:10] >>= 3
foo[1:10] <<= 3
foo[1:10] &= 3
foo[1:10] ^= 3
foo[1:10] |= 3
"""
    expected_report = """\







foo = Foo()
foo[1] = 3
foo[1:10] = 3
foo[1:10:2] = 3
foo[...] = 3
foo[1, 2:3] = 3
foo[1:10] += 3
foo[1:10] -= 3
foo[1:10] *= 3
foo[1:10] /= 3
foo[1:10] //= 3
foo[1:10] %= 3
foo[1:10] **= 3
foo[1:10] >>= 3
foo[1:10] <<= 3
foo[1:10] &= 3
foo[1:10] ^= 3
foo[1:10] |= 3 """

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_index_expression():
    code = """\
a = [1, 2]
i = 0
a[i+1] = 3
"""
    expected_report = """\
a = [1, 2]
i = 0
a[1] = 3
"""

    report = TraceRunner().trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


@pytest.mark.skipif(sys.version_info < (3, 8),
                    reason='Walrus operator added in 3.8.')
def test_walrus():
    code = """\
a = (b := 2) + 3
"""
    expected_report = """\
b = 2 | a = 5"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_starred():
    code = """\
a, *b = 'apple'
print(a, b)
"""
    expected_report = """\
(a, *b) = ('a', 'p', 'p', 'l', 'e')
print("a ['p', 'p', 'l', 'e']")"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report
