import sys

import pytest

from space_tracer.main import TraceRunner


@pytest.mark.skipif(sys.version_info < (3, 10),
                    reason='Structural pattern matching added in 3.10.')
def test_structural_pattern_match():
    code = """\
x = 10

match x:
    case 20:
        print('twenty')
    case y:
        print(f'Found {y}.')
"""
    expected_report = """\
x = 10




y = 10
print('Found 10.')"""

    report = TraceRunner().trace_code(code)

    # Treat match_case similar to assign targets.
    assert report == expected_report


@pytest.mark.skipif(sys.version_info < (3, 10),
                    reason='Structural pattern matching added in 3.10.')
def test_structural_pattern_match_tuple():
    code = """\
x = (10, 11)

match x:
    case 20:
        print('twenty')
    case y, z:
        print(f'Found {y=} and {z=}.')
"""
    expected_report = """\
x = (10, 11)




(y, z) = (10, 11)
print('Found y=10 and z=11.')"""

    report = TraceRunner().trace_code(code)

    # Treat match_case similar to assign targets.
    assert report == expected_report


@pytest.mark.skipif(sys.version_info < (3, 10),
                    reason='Structural pattern matching added in 3.10.')
def test_structural_pattern_match_star():
    code = """\
x = (10, 11, 12)

match x:
    case 20:
        print('twenty')
    case y, *z:
        print(f'Found {y=} and {z=}.')
"""
    expected_report = """\
x = (10, 11, 12)




(y, *z) = (10, 11, 12)
print('Found y=10 and z=[11, 12].')"""

    report = TraceRunner().trace_code(code)

    # Treat match_case similar to assign targets.
    assert report == expected_report
