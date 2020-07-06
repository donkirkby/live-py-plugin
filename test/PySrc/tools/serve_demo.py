#!/usr/bin/env python

""" Copy web site files and launch Jekyll server.

Copies from pyodide source, space_tracer package, and React app.
"""
from contextlib import contextmanager

from subprocess import run

import re

from pathlib import Path

import shutil
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import os


def parse_args():
    # noinspection PyTypeChecker
    parser = ArgumentParser(description='Run the demo web site.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--no_update',
                        '-n',
                        action='store_true',
                        help='Serve files without updating them.')
    parser.add_argument('--update_only',
                        '-u',
                        action='store_true',
                        help='Update files without serving them.')
    parser.add_argument('--demo_dir',
                        help='directory to serve from',
                        default=os.path.abspath(
                            os.path.join(__file__,
                                         '..',
                                         '..',
                                         '..',
                                         '..',
                                         'docs',
                                         'demo')))
    parser.add_argument('--pyodide_dir',
                        help='directory to copy Pyodide from',
                        default=os.path.abspath(
                            os.path.join(__file__,
                                         '..',
                                         '..',
                                         '..',
                                         '..',
                                         '..',
                                         'pyodide')))
    parser.add_argument('--react_dir',
                        help='directory to copy React app from',
                        default=os.path.abspath(
                            os.path.join(__file__,
                                         '..',
                                         '..',
                                         '..',
                                         '..',
                                         'html',
                                         'build')))
    return parser.parse_args()


def copy_pyodide(pyodide_dir, demo_dir):
    if not os.path.isdir(pyodide_dir):
        print(f'Pyodide directory {pyodide_dir!r} not found.')
        return

    for file_name in ('pyodide.js',
                      'pyodide.asm.wasm',
                      'pyodide.asm.js',
                      'pyodide.asm.data.js',
                      'pyodide.asm.data',
                      'packages.json',
                      'space-tracer.data',
                      'space-tracer.js'):
        source_path = os.path.join(pyodide_dir, 'build', file_name)
        target_path = os.path.join(demo_dir, 'pyodide', file_name)
        copy_if_needed(source_path, target_path)


def copy_if_needed(source_path, target_path, file_name=None):
    if file_name is None:
        file_name = os.path.basename(target_path)
    source_stat = os.stat(source_path)
    source_time = source_stat.st_mtime
    source_size = source_stat.st_size
    try:
        target_stat = os.stat(target_path)
        target_time = target_stat.st_mtime
        target_size = target_stat.st_size
    except FileNotFoundError:
        target_time = 0
        target_size = 0
    if source_time <= target_time and source_size == target_size:
        print(file_name, 'is up to date.')
    else:
        shutil.copy(source_path, target_path)
        print(file_name, 'copied.')


def copy_react(react_dir, demo_dir):
    if not os.path.isdir(react_dir):
        print(f'React directory {react_dir!r} not found.')
        return

    copied_files = set()
    for source_dir, child_names, file_names in os.walk(react_dir):
        for child_name in child_names:
            source_path = os.path.join(source_dir, child_name)
            rel_path = os.path.relpath(source_path, react_dir)
            target_path = os.path.join(demo_dir, rel_path)
            os.makedirs(target_path, exist_ok=True)
        for file_name in file_names:
            if file_name == 'favicon.ico':
                continue
            source_path = os.path.join(source_dir, file_name)
            rel_path = os.path.relpath(source_path, react_dir)
            target_path = os.path.join(demo_dir, rel_path)
            if rel_path == 'index.html':
                copy_index(Path(source_path), Path(demo_dir))
            else:
                copy_if_needed(source_path, target_path, rel_path)
                copied_files.add(target_path)

    for target_dir, child_names, file_names in os.walk(demo_dir):
        if os.path.basename(target_dir) == 'pyodide':
            continue
        for file_name in file_names:
            file_path = os.path.join(target_dir, file_name)
            if file_path not in copied_files:
                rel_path = os.path.relpath(file_path, demo_dir)
                if rel_path != 'index.md':
                    os.remove(file_path)
                    print(f'Deleted {rel_path!r}.')


def copy_index(source_path: Path, demo_dir: Path):
    index_markdown = """\
---
title: Live Python in the Browser
layout: react
is_react: True
---
"""
    index_source = source_path.read_text()
    dest_file_path = demo_dir / 'index.md'
    includes_path = demo_dir.parent / '_includes'

    dest_file_path.write_text(index_markdown)

    match = re.search(r'<link href=".*" rel="stylesheet">',
                      index_source,
                      re.MULTILINE | re.DOTALL)
    dest_file_path = includes_path / 'head-scripts.html'
    dest_file_path.write_text(wrap_react(match.group(0)))

    match = re.search(r'<div id="root"></div>(.*)</body>',
                      index_source,
                      re.MULTILINE | re.DOTALL)
    dest_file_path = includes_path / 'footer-scripts.html'
    dest_file_path.write_text(wrap_react(match.group(1)))


def wrap_react(source):
    return f"""\
{{% if page.is_react %}}
    {source}
{{% endif %}}
"""


@contextmanager
def hacked_jekyll(docs_path: Path):
    """ Ugly hack to let Jekyll 3.8.5 serve WASM.

    This is only used for local testing, GitHub seems to serve it correctly.
    Related links:
    WASM added for Jekyll 4.0.
    https://github.com/jekyll/jekyll/commit/5157bdc753d7f761e77ce15fe3cf626305639626
    GitHub pages hasn't upgraded to Jekyll 4.0 yet.
    https://github.com/github/pages-gem/issues/651
    """
    gem_home = Path(os.environ['GEM_HOME'])
    jekyll_path = gem_home / 'gems' / 'jekyll-3.8.5' / 'lib' / 'jekyll'
    mime_types_path = jekyll_path / 'mime.types'

    gems_lock_path = docs_path / 'Gemfile.lock'
    gems_lock = gems_lock_path.read_text()
    if 'jekyll (= 3.8.5)' not in gems_lock:
        print('jekyll is not version 3.8.5, not hacking it.')
        original_mime_types = None
    else:
        wasm_type = '\napplication/wasm   wasm\n'
        original_mime_types = mime_types_path.read_text()
        if 'wasm' in original_mime_types:
            print('wasm already in mime types, not hacking it.')
            original_mime_types = None
        else:
            mime_types_path.write_text(original_mime_types + wasm_type)
    try:
        yield
    finally:
        if original_mime_types is not None:
            mime_types_path.write_text(original_mime_types)


def main():
    args = parse_args()
    if not args.no_update:
        copy_pyodide(args.pyodide_dir, args.demo_dir)
        copy_react(args.react_dir, args.demo_dir)

    if not args.update_only:
        docs_path = Path(args.demo_dir).parent
        with hacked_jekyll(docs_path):
            try:
                run(['bundle', 'exec', 'jekyll', 'serve'], cwd=docs_path)
            except KeyboardInterrupt:
                print('Shutting down...')


main()
