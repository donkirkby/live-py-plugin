from space_tracer.main import TraceRunner


def test_loop():
    code = """\
i = 1
for j in range(3):
    i += j
"""
    expected_report = """\
i = 1
j = 0 | j = 1 | j = 2
i = 1 | i = 2 | i = 4"""

    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_loop_target_list():
    code = """\
for a,b in [(1,2)]:
    c = a + b
"""
    expected_report = """\
a = 1 | b = 2
c = 3"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_loop_starred_target_list():
    code = """\
words = ['foo', 'bar']
for (head, *tail) in words:
    print(head, tail)
"""
    expected_report = """\
words = ['foo', 'bar']
head = 'f' | tail = ['o', 'o'] | head = 'b' | tail = ['a', 'r']
print("f ['o', 'o']")          | print("b ['a', 'r']")"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_loop_target_list_attribute():
    code = """\
class Foo:
    def do_foo(self, x):
        for self.i in range(x):
            print(self.i)

foo = Foo()
foo.do_foo(3)
"""
    expected_report = """\

x = 3
self.i = 0 | self.i = 1 | self.i = 2
print('0') | print('1') | print('2')


"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_loop_target_list_attribute_complex():
    code = """\
class Foo:
    def do_foo(self, x):
        self.state = [None]
        for self.state[0] in range(x):
            print(self.state)

foo = Foo()
foo.do_foo(3)
"""
    expected_report = """\

x = 3
self.state = [None]
             |              |
print('[0]') | print('[1]') | print('[2]')


"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_nested_loop():
    code = """\
n = 0
for i in range(2):
    n += i
    for j in range(3):
        n += j
"""
    expected_report = """\
n = 0
i = 0                 | i = 1
n = 0                 | n = 4
j = 0 | j = 1 | j = 2 | j = 0 | j = 1 | j = 2
n = 0 | n = 1 | n = 3 | n = 4 | n = 5 | n = 7"""
    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_for_else():
    code = """\
i = 1
for j in range(3):
    i += j
else:
    i *= 10
"""
    expected_report = """\
i = 1
j = 0 | j = 1 | j = 2
i = 1 | i = 2 | i = 4

i = 40"""
    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_while_else():
    code = """\
i = 0
while i < 2:
    i += 1
else:
    i *= 10
"""
    expected_report = """\
i = 0
      |
i = 1 | i = 2

i = 20"""
    report = TraceRunner().trace_code(code)

    assert report == expected_report


def test_loop_conditional():
    code = """\
for i in range(3):
    if i == 1:
        c = 5
c = 2
"""
    expected_report = """\
i = 0 | i = 1 | i = 2
      |       |
      | c = 5 |
c = 2"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report == expected_report


def test_infinite_loop_by_count():
    code = """\
n = 0
while True:
    n += 1
"""
    expected_report = """\
n = 0
      |       |
n = 1 | n = 2 | RuntimeError: live coding message limit exceeded"""
    tracer = TraceRunner()
    tracer.message_limit = 8

    report = tracer.trace_code(code)

    assert report == expected_report


def test_infinite_loop_by_width():
    code = """\
n = 0
while True:
    n += 1
"""
    expected_report = """\
n = 0
      |       |
n = 1 | n = 2 | RuntimeError: live coding message limit exceeded"""
    tracer = TraceRunner()
    tracer.max_width = 20

    report = tracer.trace_code(code)

    assert report == expected_report


def test_infinite_loop_pass():
    code = """\
while True:
    pass
"""
    expected_report = """\
RuntimeError: live coding message limit exceeded"""
    tracer = TraceRunner()
    tracer.message_limit = 3

    report = tracer.trace_code(code)

    assert report in (expected_report + '\n', '\n' + expected_report)


def test_infinite_loop_pass_in_function():
    code = """\
def foo():
    while True:
        pass

foo()
"""
    expected_report1 = """\

RuntimeError: live coding message limit exceeded


RuntimeError: live coding message limit exceeded"""
    expected_report2 = """\


RuntimeError: live coding message limit exceeded

RuntimeError: live coding message limit exceeded"""
    tracer = TraceRunner()
    tracer.message_limit = 3

    report = tracer.trace_code(code)

    assert report in (expected_report1, expected_report2)
