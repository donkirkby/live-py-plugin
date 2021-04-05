import sys

import pytest

from space_tracer.main import TraceRunner


@pytest.mark.skipif(sys.version_info < (3, 10, 0, 'alpha', 6),
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
