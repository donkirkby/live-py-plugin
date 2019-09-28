from space_tracer.traced_finder import TracedFinder


def test_find_function():
    source = """\
# some comment

def foo(n):
    return n + 1
"""
    traced = 'foo'
    expected_line_number = 3

    finder = TracedFinder(source, traced)

    node = finder.traced_node
    assert expected_line_number == node.lineno


def test_named_function():
    source = """\
# some comment

def foo(n):
    return n + 1

def bar(n):
    return n - 1
"""
    traced = 'foo'
    expected_line_number = 3

    finder = TracedFinder(source, traced)

    node = finder.traced_node
    assert expected_line_number == node.lineno


def test_class():
    source = """\
class Foo(object):
    pass

class Bar(object):
    pass
"""
    traced = 'Bar'
    expected_line_number = 4

    finder = TracedFinder(source, traced)

    node = finder.traced_node
    assert expected_line_number == node.lineno


def test_method():
    source = """\
class Foo(object):
    def baz():
        pass

class Bar(object):
    def baz():
        pass
"""
    traced = 'Bar.baz'
    expected_line_number = 6

    finder = TracedFinder(source, traced)

    node = finder.traced_node
    assert expected_line_number == node.lineno


def test_nested_method():
    source = """\
def foo():
    def baz():
        pass

def bar():
    def baz():
        pass
"""
    traced = 'bar.baz'
    expected_line_number = 6

    finder = TracedFinder(source, traced)

    node = finder.traced_node
    assert expected_line_number == node.lineno


def test_suffix_not_allowed():
    source = """\
def foo(n):
    return n + 1

def bar(n):
    return n - 1
"""
    traced = 'parent.child.bar'

    finder = TracedFinder(source, traced)

    node = finder.traced_node
    assert node is None
