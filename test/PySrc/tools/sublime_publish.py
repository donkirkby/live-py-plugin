""" Copy Python scripts to Sublime Text package.

This creates the /docs/sublime-package/vX.Y.Z.zip file with the contents of the
sublime and space_tracer folders, then it updates the version number in the
package.json file.
"""
import json
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, ArgumentError, FileType
from datetime import datetime, timezone
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED


def parse_args():
    parser = ArgumentParser(description='Package plugin for Sublime Text.',
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--out',
                        type=existing_folder,
                        default='../../../docs/sublime-package')
    parser.add_argument('--sublime',
                        type=existing_folder,
                        default='../../../sublime')
    parser.add_argument('--space_tracer',
                        type=existing_folder,
                        default='../../../plugin/PySrc/space_tracer')
    parser.add_argument('--license',
                        type=FileType(),
                        default='../../../license.txt')
    return parser.parse_args()


def existing_folder(folder_name: str) -> Path:
    path = Path(folder_name)
    if not path.is_dir():
        raise ArgumentError(None, f'Folder does not exist: {folder_name!r}.')
    return path


def main():
    args = parse_args()
    space_tracer_path: Path = args.space_tracer
    about = {}
    exec((space_tracer_path / "about.py").read_text(), about)
    next_version = about['__version__']

    out_path: Path = args.out
    package_details_path = out_path / 'package.json'
    package_details = json.loads(package_details_path.read_text())

    releases: list = package_details['packages'][0]['releases']
    for release in releases:
        if release['version'] == next_version:
            exit(f'Version {next_version} has already been published.')
    next_release = create_release(next_version)
    releases.insert(0, next_release)
    package_details_path.write_text(json.dumps(package_details, indent=2))

    print('Packaging', next_version)
    package_path = out_path / f'python_live_coding_v{next_version}.zip'
    with ZipFile(package_path, 'w', ZIP_DEFLATED) as package_zip:
        package_root = Path('python_live_coding')
        # noinspection PyUnusedLocal
        source_path: Path
        for source_path in (args.sublime, args.space_tracer):
            if source_path == args.space_tracer:
                arc_path_folder = package_root / 'space_tracer'
            else:
                arc_path_folder = package_root
            # noinspection PyUnusedLocal
            child_path: Path
            for child_path in source_path.rglob('*'):
                if '__pycache__' in str(child_path):
                    continue
                if child_path.suffix == '.pyc':
                    continue
                relative_path = child_path.relative_to(source_path)
                arc_path = arc_path_folder / relative_path
                print(arc_path)
                package_zip.write(child_path, arc_path)
        license_text = args.license.read()
        arc_path = package_root / 'LICENSE'
        print(arc_path)
        package_zip.writestr(str(arc_path), license_text)
    print('Done.')


def create_release(version):
    utc_now = datetime.now(timezone.utc)
    package_url = (f'https://donkirkby.github.io/live-py-plugin/'
                   f'sublime-package/python_live_coding_v{version}.zip')
    next_release = dict(version=version,
                        url=package_url,
                        date=utc_now.strftime('%Y-%m-%d %H:%M:%S'),
                        sublime_text='>=3000')
    return next_release


main()
