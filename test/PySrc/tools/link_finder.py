import re

from pathlib import Path


def find_links(docs_folder: Path, child: str) -> dict:
    links = {}
    source_folder = docs_folder / child
    for source_file in source_folder.iterdir():
        if source_file.suffix == '.rst':
            with source_file.open() as source:
                for line in source:
                    match = re.fullmatch(r'\.\. _(.*):\s*', line)
                    if match:
                        section = ''
                        while not section:
                            section = next(source).strip(' \n*')
                        section = re.sub(r':[^:]+:`([^`]+)`', r'\1', section)
                        key = match.group(1)
                        path = (child + '/' +
                                source_file.with_suffix('.html').name)
                        links[key] = (path, section)
    return links
