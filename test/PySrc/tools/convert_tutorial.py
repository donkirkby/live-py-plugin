import re

from subprocess import run
from tempfile import NamedTemporaryFile

from pathlib import Path

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, FileType

from link_finder import find_links


def parse_args():
    parser = ArgumentParser(description='Convert a tutorial to markdown.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('source',
                        type=FileType(),
                        help='source file to convert from')
    parser.add_argument('target',
                        nargs='?',
                        default='cpython',
                        help='target file or folder to convert to')
    return parser.parse_args()


def main():
    args = parse_args()
    root = Path(__file__).parent.parent.parent.parent
    tutorials: Path = root / 'html' / 'tutorials'
    target = (tutorials / args.target).resolve()
    source = Path(args.source.name)
    docs_path = source.parent.parent
    links = {}
    for child in ('tutorial', 'reference', 'library'):
        links.update(find_links(docs_path, child))
    if target.is_dir():
        basename = source.with_suffix('.md').name
        target = target / basename
    if target.exists():
        confirmation = input(f'Target {target.name} exists. Recreate? Y/[N] ')
        if confirmation.lower() != 'y':
            return
    temp_path = None
    try:
        with NamedTemporaryFile(mode='w+',
                                prefix='tutorial_',
                                suffix=source.suffix,
                                delete=False) as temp_source:
            temp_path = Path(temp_source.name)
            for line in source.open():
                # line = re.sub(r':keyword:`([^`]+)`', r'``\1``', line)
                line = re.sub(r':((keyword)|(attr)):`([^`]+)`', r'``\4``', line)
                line = re.sub(r':((func)|(meth)):`([^`]+)`', r'``\4()``', line)
                line = re.sub(r':dfn:`([^`]+)`', r'*\1*', line)
                line = re.sub(r':pep:`([^`]+)`', r'PEP \1', line)
                line = re.sub(r'^.. sectionauthor::.*$', r'', line)
                match = re.search(r':((ref)|(term)):`([^`]+)`', line)
                if match:
                    ref_text = match.group(4)
                    match2 = re.match(r'(.*)<(.*)>', ref_text)
                    if match2:
                        link_key = match2.group(2)
                        link_text = match2.group(1)
                    else:
                        link_key = ref_text
                        link_text = None

                    if match.group(1) == 'term':
                        link_path = 'glossary.html'
                        link_section = link_key
                    else:
                        link_path, link_section = links[link_key]
                    if not link_text:
                        link_text = link_section
                    formatted_key = link_key.lower().replace(' ', '-')
                    url = 'https://docs.python.org/3/' + link_path
                    formatted_link = f'`{link_text}<{url}#{formatted_key}>`_'
                    line = line[:match.start()] + formatted_link + line[match.end():]
                temp_source.write(line)
        run(['pandoc', '-o', target, str(temp_path)])
    finally:
        if temp_path:
            temp_path.unlink()
    print('Done.')


main()
