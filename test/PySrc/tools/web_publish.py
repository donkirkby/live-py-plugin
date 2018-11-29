""" Copy Python scripts to web site.

This updates the /docs/demo/code_tracer.py file with the contents of
report_builder.py and code_tracer.py from the main project.
To test out the web site locally:

    cd docs/demo
    python -m http.server
"""
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, FileType


def parse_args():
    parser = ArgumentParser(description='Generate Python code for web site.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--out',
                        type=FileType('w'),
                        default='../../../docs/demo/code_tracer.py')
    parser.add_argument('--report_builder',
                        type=FileType(),
                        default='../../../plugin/PySrc/report_builder.py')
    parser.add_argument('--code_tracer',
                        type=FileType(),
                        default='../../../plugin/PySrc/code_tracer.py')
    return parser.parse_args()


def main():
    args = parse_args()
    for line in args.report_builder:
        args.out.write(line)
    args.out.write('\n\n')
    for line in args.code_tracer:
        args.out.write(line)
    print('Done.')


main()
