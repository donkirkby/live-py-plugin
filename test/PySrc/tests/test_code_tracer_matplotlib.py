import re

import pytest
import sys

from space_tracer.canvas import Canvas
from space_tracer.code_tracer import CodeTracer
from test_report_builder import trim_report


@pytest.fixture(name='is_matplotlib_cleared')
def clear_matplotlib():
    for should_yield in (True, False):
        to_delete = [module_name
                     for module_name in sys.modules
                     if module_name.startswith('matplotlib')]
        for module_name in to_delete:
            del sys.modules[module_name]
        if should_yield:
            yield True


def replace_image(report):
    report = trim_report(report)
    report = re.sub(r"image='[a-zA-Z0-9+/=]*'", "image='...'", report)
    return report


def test_show(is_matplotlib_cleared):
    assert is_matplotlib_cleared
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
    canvas = Canvas(width=100, height=100)
    tracer = CodeTracer(canvas)

    report = tracer.trace_turtle(code)

    assert expected_report == replace_image(report)


def test_numpy_random():
    code = """\
import numpy as np 

x = np.random.normal(size=3)
"""
    tracer = CodeTracer()

    original_report = tracer.trace_code(code)

    for _ in range(5):
        report = tracer.trace_code(code)
        assert original_report == report
