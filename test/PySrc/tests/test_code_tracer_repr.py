from space_tracer.main import TraceRunner
from test_report_builder import trim_report


def test_repr():
    code = """\
class Dog(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'Dog(%r)' % self.name

dog = Dog('Spot')
animal = dog
"""
    expected_report = """\

name = 'Spot'
self.name = 'Spot'




dog = Dog('Spot')
animal = Dog('Spot') """
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_call():
    code = """\
class Dog(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return 'Dog(%r)' % self.name

dog1 = Dog('Spot')
dog2 = Dog('Fido')
s = repr(dog2)
"""
    expected_report = """\

name = 'Spot'      | name = 'Fido'
self.name = 'Spot' | self.name = 'Fido'


return "Dog('Fido')"

dog1 = Dog('Spot')
dog2 = Dog('Fido')
s = "Dog('Fido')"
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_return():
    code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

def bar(x):
    return Foo(x)

y = bar(23)
"""
    expected_report = """\

x = 23
self.x = 23




x = 23
return Foo(23)

y = Foo(23)
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_yield():
    code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

def bar(x):
    yield Foo(x)

y = list(bar(23))
"""
    expected_report = """\

x = 23
self.x = 23




x = 23
yield Foo(23)

y = [Foo(23)]
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_yield_tuple():
    code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

def bar(x):
    yield x, Foo(x)

y = list(bar(23))
"""
    expected_report = """\

x = 23
self.x = 23




x = 23
yield (23, Foo(23))

y = [(23, Foo(23))]
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_lambda():
    code = """\
class Foo:
    def __init__(self, x):
        self.x = x

    def __repr__(self):
        return 'Foo({!r})'.format(self.x)

y = list(map(lambda n: Foo(n), range(2)))
"""
    expected_report = """\

x = 0      | x = 1
self.x = 0 | self.x = 1




(0 => Foo(0)) | (1 => Foo(1)) | y = [Foo(0), Foo(1)] 
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_mock():
    code = """\
from unittest.mock import Mock

m = Mock(name='foo')
"""
    expected_report = """\


m = Mock(name='foo')
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_magic_mock():
    code = """\
from unittest.mock import MagicMock

m = MagicMock(name='foo')
"""
    expected_report = """\


m = MagicMock(name='foo')
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_unnamed_mock():
    code = """\
from unittest.mock import Mock

m = Mock()
"""
    expected_report = """\


m = Mock()
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)


def test_repr_enum():
    code = """\
from enum import Enum

Animal = Enum('Animal', 'ANT BEE CAT DOG')
eric = Animal.BEE
"""
    expected_report1 = """\



eric = <Animal.BEE: 2>"""
    expected_report2 = """\



eric = Animal.BEE"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert report in (expected_report1, expected_report2)


def test_error():
    code = """\
from __future__ import print_function

class Foo(object):
    def __repr__(self):
        raise RuntimeError('Bad representation.')

def create_foo():
    return Foo()

print('Start.')
foo = create_foo()
print('End.')
"""
    expected_report = """\







return <Foo object>

print('Start.')

print('End.')
"""
    tracer = TraceRunner()

    report = tracer.trace_code(code)

    assert trim_report(expected_report) == trim_report(report)
