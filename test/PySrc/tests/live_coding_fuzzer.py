#!/usr/bin/python3.9

""" Run a fuzz test on randomly generated source code.

Check that the source code display matches the original source code, and you
haven't hit some weird failure.

This gets run by the ClusterFuzzLite project at
https://google.github.io/clusterfuzzlite/ To test it locally, you can clone
oss-fuzz, then build an external fuzzer and run it.

    git clone https://github.com/google/oss-fuzz.git
    git clone https://github.com/donkirkby/live-py-plugin.git
    cd oss-fuzz
    python3 infra/helper.py build_image --external ../live-py-plugin/
    python3 infra/helper.py build_fuzzers --external ../live-py-plugin/ --sanitizer address
    python3 infra/helper.py check_build --external ../live-py-plugin/ --sanitizer address
    python3 infra/helper.py run_fuzzer --external --corpus-dir=../live-py-plugin/corpus/ ../live-py-plugin/ live_coding_fuzzer -- -runs=10000

Then you can repeat the steps with --sanitizer undefined. Add -help=1 to the
last command, if you want to see libfuzzer options.

To reproduce a crash, find the crash file and then request it in the reproduce
command:

    ls build/out/live-py-plugin/crash-*
    python3 infra/helper.py run_fuzzer --external --corpus-dir=../live-py-plugin/corpus/ ../live-py-plugin/ live_coding_fuzzer -- -runs=10000
    python3 infra/helper.py reproduce --external ../live-py-plugin/ live_coding_fuzzer build/out/live-py-plugin/crash-1234...
"""

import re
import sys
import typing
from ast import Assign, Call, Constant, Expr, fix_missing_locations, For, \
    Load, Module, Name, stmt, Store, unparse
from random import choice
import linecache
import os
import tracemalloc

try:
    import atheris
except ImportError:
    atheris = None

if atheris is None:
    from space_tracer.main import TraceRunner, replace_input
else:
    # noinspection PyUnresolvedReferences
    with atheris.instrument_imports():
        from space_tracer.main import TraceRunner, replace_input


class CodeContext:
    def __init__(self, data: typing.Iterator[bytes]):
        self.data = data
        self.parent = Module(body=[], type_ignores=[])
        self.local_names = []

    def generate_source(self) -> str:
        for statement in self.generate_statements():
            self.parent.body.append(statement)
        # noinspection PyTypeChecker
        fix_missing_locations(self.parent)
        # noinspection PyTypeChecker
        return unparse(self.parent)

    def generate_ints(self,
                      stop: int = 256,
                      start: int = 0) -> typing.Iterator[int]:
        assert stop - start <= 256
        for value in self.data:
            yield int(value) % (stop - start) + start

    def generate_names(self) -> typing.Iterator[str]:
        for value in self.data:
            yield f'x{value}'

    def generate_statements(self) -> typing.Iterator[stmt]:
        generators = [self.generate_assignments,
                      self.generate_prints,
                      self.generate_for_loops]
        for statement_type in self.generate_ints(3):
            generator = generators[statement_type]
            for statement in generator():
                yield statement
                break

    def generate_assignments(self) -> typing.Iterator[stmt]:
        for name, value in zip(self.generate_names(), self.generate_ints()):
            self.local_names.append(name)
            yield Assign(targets=[Name(id=name, ctx=Store())],
                         value=Constant(value=value))

    def generate_prints(self) -> typing.Iterator[stmt]:
        for scope_choice in self.generate_ints(100):
            if self.local_names:
                name = choice(self.local_names)
            else:
                name = 'x'
            if scope_choice == 0:
                for name in self.generate_names():
                    break
            yield Expr(value=Call(func=Name(id='print', ctx=Load()),
                                  args=[Name(id=name, ctx=Load())], keywords=[]))

    def generate_for_loops(self) -> typing.Iterator[stmt]:
        for iter_name, loop_count, child_count in zip(self.generate_names(),
                                                      self.generate_ints(10),
                                                      self.generate_ints(10)):
            children = [statement
                        for (statement, _) in zip(self.generate_statements(),
                                                  range(child_count))]
            yield For(target=Name(id=iter_name, ctx=Store()),
                      iter=Call(func=Name(id='range', ctx=Load()),
                                args=[Constant(value=loop_count)],
                                keywords=[]),
                      body=children,
                      orelse=[])

def test_one_input(data):
    """ This gets called over and over with a random bytes object. """
    context = CodeContext(iter(data))
    source = context.generate_source()

    with replace_input(source):
        runner = TraceRunner()
        try:
            report = runner.trace_command(['space_tracer',
                                           '--live',
                                           '--trace_offset=1000000',
                                           '--trace_width=0',  # no limit
                                           '-'])
        except BaseException:
            print("!!! Source !!!")
            print(source)
            raise
    trimmed_report = re.sub(r'\s*\|\s*$', '', report, flags=re.MULTILINE)
    if source != trimmed_report:
        with replace_input(source):
            runner = TraceRunner()
            report2 = runner.trace_command(['space_tracer',
                                            '--live',
                                            '-'])
        print("### Source ###")
        print(source)
        print()
        print("### Report ###")
        print(report2)
        raise RuntimeError("Source and report differ.")


def display_top(snapshot, key_type='lineno', limit=3):
    snapshot = snapshot.filter_traces((
        tracemalloc.Filter(False, "<frozen importlib._bootstrap>"),
        tracemalloc.Filter(False, "<unknown>"),
    ))
    top_stats = snapshot.statistics(key_type)

    print("Top %s lines" % limit)
    for index, stat in enumerate(top_stats[:limit], 1):
        frame = stat.traceback[0]
        # replace "/path/to/module/file.py" with "module/file.py"
        filename = os.sep.join(frame.filename.split(os.sep)[-2:])
        print("#%s: %s:%s: %.1f KiB"
              % (index, filename, frame.lineno, stat.size / 1024))
        line = linecache.getline(frame.filename, frame.lineno).strip()
        if line:
            print('    %s' % line)

    other = top_stats[limit:]
    if other:
        size = sum(stat.size for stat in other)
        print("%s other: %.1f KiB" % (len(other), size / 1024))
    total = sum(stat.size for stat in top_stats)
    print("Total allocated size: %.1f KiB" % (total / 1024))


def demo():
    data = b'lorem ipsum'
    tracemalloc.start()

    for i in range(1_000):
        if i % 100 == 0:
            print('.', end='', flush=True)
        test_one_input(data)

    snapshot = tracemalloc.take_snapshot()
    display_top(snapshot)
    print('Done.')


# noinspection PyUnresolvedReferences
def main():
    if '--demo' in sys.argv:
        demo()
    else:
        if atheris is None:
            raise ImportError(
                'Atheris not found. Did you want to run with --demo?')
        atheris.Setup(sys.argv, test_one_input)
        atheris.Fuzz()


main()
