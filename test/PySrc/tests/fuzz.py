import re
import typing
from ast import Assign, Constant, Module, Name, stmt, Store, unparse, fix_missing_locations

from pythonfuzz.main import PythonFuzz

from space_tracer.main import TraceRunner, replace_input


class CodeContext:
    def __init__(self, data: typing.Iterator[bytes]):
        self.data = data
        self.parent = Module(body=[], type_ignores=[])

    def generate_source(self) -> str:
        for statement in self.generate_statements():
            self.parent.body.append(statement)
        # noinspection PyTypeChecker
        fix_missing_locations(self.parent)
        # noinspection PyTypeChecker
        return unparse(self.parent)

    def generate_statements(self) -> typing.Iterator[stmt]:
        for value in self.data:
            yield Assign(targets=[Name(id='x', ctx=Store())],
                         value=Constant(value=value))


@PythonFuzz
def fuzz(data):
    context = CodeContext(iter(data))
    source = context.generate_source()

    runner = TraceRunner()
    with replace_input(source):
        report = runner.trace_command(['space_tracer',
                                       '--trace_offset=1000000',
                                       '-'])
    trimmed_report = re.sub(r'\s*\|\s*$', '', report, flags=re.MULTILINE)
    if source != trimmed_report:
        print("### Source ###")
        print(source)
        print()
        print("### Report ###")
        print(trimmed_report)
        raise RuntimeError("Source and report differ.")


fuzz()
