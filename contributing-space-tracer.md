## Space Tracer Development
`space_tracer` is the command-line version that you can install with `pip`. It
doesn't require any special development tools, beyond Python and the
[git download instructions] if you don't already have it.

[git download instructions]: https://git-scm.com/downloads

### Deploying a new release of Space Tracer
The details are at [packaging.python.org], but the main steps are:

1. Update the version number in `about.py` and development status in
   `pyproject.toml`.
2. Activate the latest Python's virtual environment.

        source .tox/py39/bin/activate

3. Install the build tools.

        python -m pip install --upgrade setuptools wheel twine

4. Build the release files.

        python setup.py sdist bdist_wheel

5. Upload the release to PyPI. You'll need a user name and password.

        ls dist/*
        twine upload dist/*

6. Check that the new version is on the [package page], and try installing it.

        pip install --no-cache space_tracer

7. Remove the uploaded files and recreate the tox environment. (The browser
    version uses these files, so don't remove them until you've deployed that.)

        rm dist/*
        deactivate
        tox -r -epy39

8. Commit the version number changes, push, and create a release on GitHub.
    (Finish the other releases before marking the release on GitHub, if you're
    releasing more.)

[packaging.python.org]: https://packaging.python.org/tutorials/packaging-projects/
[package page]: https://pypi.org/project/space-tracer/
