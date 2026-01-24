import setuptools

with open("space_tracer.md") as f:
    long_description = f.read()
about = {}
with open("plugin/PySrc/space_tracer/about.py") as f:
    exec(f.read(), about)

# noinspection PyUnresolvedReferences
setuptools.setup(
    name=about['__title__'],
    version=about['__version__'],
    author=about['__author__'],
    author_email=about['__author_email__'],
    description=about['__description__'],
    long_description=long_description,
    long_description_content_type="text/markdown",
    packages=setuptools.find_packages('plugin/PySrc/'),
    package_data={'space_tracer': ['py.typed']},
    package_dir={'': 'plugin/PySrc/'}
)
