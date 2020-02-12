import json
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, FileType
from pathlib import Path


def main():
    parser = ArgumentParser(description='Collect markdown files, and write JSON.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    project_path = Path(__file__).parent.parent.parent.parent
    parser.add_argument('--source',
                        type=Path,
                        default=project_path / 'html' / 'lessons')
    parser.add_argument('--target',
                        type=FileType('w'),
                        default=str(project_path / 'html' / 'src' /
                                    'lessons.json'))
    args = parser.parse_args()

    lessons = {}
    # source_file: Path
    for source_file in args.source.rglob('*.md'):
        name = source_file.name[:-3]
        if name == 'README':
            continue
        source = source_file.read_text()
        lessons[name] = source
    json.dump(lessons, args.target)


main()
