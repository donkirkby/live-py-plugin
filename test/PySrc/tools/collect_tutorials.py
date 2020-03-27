import json
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, FileType
from pathlib import Path


def main():
    parser = ArgumentParser(description='Collect markdown files, and write JSON.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    project_path = Path(__file__).parent.parent.parent.parent
    parser.add_argument('--source',
                        type=Path,
                        default=project_path / 'html' / 'tutorials')
    parser.add_argument('--target',
                        type=FileType('w'),
                        default=str(project_path / 'html' / 'src' /
                                    'tutorials.json'))
    args = parser.parse_args()

    tutorials = {}
    # source_file: Path
    for source_file in args.source.rglob('*.md'):
        name = str(source_file.relative_to(args.source).with_suffix(''))
        if name == 'README':
            continue
        source = source_file.read_text()
        tutorials[name] = source
    json.dump(tutorials, args.target)


main()
