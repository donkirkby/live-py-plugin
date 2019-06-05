#!/usr/bin/env python

""" Run the demo using the simple HTTP server.

Only useful for testing, but it sets the MIME types correctly.
"""
import errno
import shutil
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from http.server import SimpleHTTPRequestHandler
import os
import socketserver
from time import sleep


def parse_args():
    parser = ArgumentParser(description='Run the demo web site.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--port',
                        type=int,
                        default=8000)
    parser.add_argument('--no_update',
                        '-n',
                        action='store_true',
                        help='Serve files without updating them.')
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
    parser.add_argument('--tracer_dir',
                        help='directory to copy code tracer from',
                        default=os.path.abspath(
                            os.path.join(__file__,
                                         '..',
                                         '..',
                                         '..',
                                         '..',
                                         'plugin',
                                         'PySrc')))
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
                      'packages.json'):
        source_path = os.path.join(pyodide_dir, 'build', file_name)
        target_path = os.path.join(demo_dir, 'pyodide', file_name)
        copy_if_needed(source_path, target_path)


def copy_if_needed(source_path, target_path, file_name=None):
    if file_name is None:
        file_name = os.path.basename(target_path)
    source_time = os.stat(source_path).st_mtime
    try:
        target_time = os.stat(target_path).st_mtime
    except FileNotFoundError:
        target_time = 0
    if source_time <= target_time:
        print(file_name, 'is up to date.')
    else:
        shutil.copy(source_path, target_path)
        print(file_name, 'copied.')


def copy_tracer(tracer_dir, demo_dir):
    if not os.path.isdir(tracer_dir):
        print(f'Code tracer directory {tracer_dir!r} not found.')
        return

    target_path = os.path.join(demo_dir, 'code_tracer.py')
    source_names = ('report_builder.py', 'code_tracer.py')
    source_paths = [os.path.join(tracer_dir, source_name)
                    for source_name in source_names]
    source_time = max(os.stat(source_path).st_mtime
                      for source_path in source_paths)
    try:
        target_time = os.stat(target_path).st_mtime
    except FileNotFoundError:
        target_time = 0

    if source_time <= target_time:
        print('Code tracer is up to date.')
    else:
        with open(target_path, 'w') as target:
            for source_path in source_paths:
                with open(source_path) as source:
                    shutil.copyfileobj(source, target)
        print('Code tracer copied.')


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
            copy_if_needed(source_path, target_path, rel_path)
            copied_files.add(target_path)

    for target_dir, child_names, file_names in os.walk(demo_dir):
        if os.path.basename(target_dir) == 'pyodide':
            continue
        for file_name in file_names:
            if file_name == 'code_tracer.py':
                continue
            file_path = os.path.join(target_dir, file_name)
            if file_path not in copied_files:
                os.remove(file_path)
                rel_path = os.path.relpath(file_path, demo_dir)
                print(f'Deleted {rel_path!r}.')


def launch(port, retries=20):
    try:
        httpd = socketserver.TCPServer(("", port), SimpleHTTPRequestHandler)
    except OSError as ex:
        if ex.errno != errno.EADDRINUSE or retries <= 0:
            raise
        print('Address still in use...')
        sleep(5)
        httpd = launch(port, retries-1)
    return httpd


def main():
    args = parse_args()
    if not args.no_update:
        copy_pyodide(args.pyodide_dir, args.demo_dir)
        copy_tracer(args.tracer_dir, args.demo_dir)
        copy_react(args.react_dir, args.demo_dir)

    SimpleHTTPRequestHandler.extensions_map.update({
        '.wasm': 'application/wasm',
    })
    os.chdir(args.demo_dir)
    SimpleHTTPRequestHandler.directory = args.demo_dir

    print("Serving at port", args.port, 'from', SimpleHTTPRequestHandler.directory)
    httpd = launch(args.port)

    print("Launched.")
    httpd.serve_forever()


main()
