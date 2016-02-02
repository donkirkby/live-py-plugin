from itertools import izip_longest

import code_tracer


def dump(source):
    """ Format the source and its live coding display for posting as text. """

    tracer = code_tracer.CodeTracer()
    report = tracer.trace_code(source)
    source_lines = source.splitlines()
    report_lines = report.splitlines()
    source_width = max(map(len, source_lines))
    indent = 4
    for source_line, report_line in izip_longest(source_lines,
                                                 report_lines,
                                                 fillvalue=''):
        print(indent * ' ' + source_line +
              (source_width-len(source_line))*' ' + ' | ' + report_line)

if __name__ in ('__main__', '__live_coding__'):
    source = """\
x = 2
for i in range(3):
    x += i * 3
print(x)
"""
    dump(source)
