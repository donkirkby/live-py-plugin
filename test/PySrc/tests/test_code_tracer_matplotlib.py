import pytest

from space_tracer.main import TraceRunner, replace_input
from test_live_image import replace_image

try:
    import matplotlib
except ImportError:
    matplotlib = None


@pytest.fixture(name='is_matplotlib_cleared')
def clear_matplotlib():
    """ We used to clear the matplotlib state between tests.

    Now, we just check that matplotlib is installed.
    """
    if matplotlib is None:
        pytest.skip('Matplotlib is not installed.')
    yield True


def test_show(is_matplotlib_cleared):
    code = """\
import matplotlib.pyplot as plt

data = [1, 2]
plt.plot(data)
plt.show()
"""
    expected_report = """\
create_image
    0
    0
    image='...'
"""
    tracer = TraceRunner()

    report = tracer.trace_turtle(code,
                                 0,
                                 0,
                                 '--millisecond_limit=10000')

    assert replace_image(report) == expected_report


def test_clear_plots(is_matplotlib_cleared):
    code1 = """\
import matplotlib.pyplot as plt

data = [1, 2]
plt.plot(data)
plt.show()
"""
    code2 = """\
import matplotlib.pyplot as plt

data = [2, 1]
plt.plot(data)
plt.show()
"""
    tracer = TraceRunner()

    report1 = tracer.trace_turtle(code1)
    tracer.trace_turtle(code2)
    report2 = tracer.trace_turtle(code1)

    assert report1 == report2


def test_show_with_canvas_size(is_matplotlib_cleared):
    code = """\
import matplotlib.pyplot as plt

data = [1, 2]
plt.plot(data)
plt.show()
"""
    expected_report = """\
create_image
    0
    0
    image='...'
"""
    tracer = TraceRunner()

    report = tracer.trace_turtle(code, width=400, height=200)

    assert expected_report == replace_image(report)


def test_zoom_big(is_matplotlib_cleared):
    code = """\
import matplotlib.pyplot as plt

f = plt.gcf()
print(f.dpi)
"""
    expected_report = """\



print('125.0')"""
    tracer = TraceRunner()

    with replace_input(code):
        report = tracer.trace_command(['space_tracer',
                                       '--traced_file', 'example.py',
                                       '--source_width', '0',
                                       '--live',
                                       '-x1200',
                                       '-y600',
                                       '--zoomed',
                                       'example.py'])

    assert report == expected_report


def test_zoom_small(is_matplotlib_cleared):
    code = """\
import matplotlib.pyplot as plt

f = plt.gcf()
print(f.dpi)
"""
    expected_report = """\



print('25.0')"""
    tracer = TraceRunner()

    with replace_input(code):
        report = tracer.trace_command(['space_tracer',
                                       '--traced_file', 'example.py',
                                       '--source_width', '0',
                                       '--live',
                                       '-x240',
                                       '-y120',
                                       '--zoomed',
                                       'example.py'])

    assert report == expected_report


def test_numpy_random():
    code = """\
import numpy as np 

x = np.random.normal(size=3)
"""
    tracer = TraceRunner()

    original_report = tracer.trace_code(code)

    for _ in range(5):
        report = tracer.trace_code(code)
        assert original_report == report
