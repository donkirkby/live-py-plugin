import pytest
import sys

from space_tracer.main import TraceRunner
from test_live_image import replace_image

try:
    import matplotlib
except ImportError:
    matplotlib = None


@pytest.fixture(name='is_matplotlib_cleared')
def clear_matplotlib():
    if matplotlib is None:
        pytest.skip('Matplotlib is not installed.')
    for should_yield in (True, False):
        to_delete = [module_name
                     for module_name in sys.modules
                     if module_name.startswith('matplotlib')]
        for module_name in to_delete:
            del sys.modules[module_name]
        if should_yield:
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

    report = tracer.trace_turtle(code)

    assert expected_report == replace_image(report)


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
