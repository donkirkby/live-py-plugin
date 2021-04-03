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
    url=about['__url__'],
    packages=setuptools.find_packages('plugin/PySrc/'),
    package_dir={'': 'plugin/PySrc/'},
    entry_points=dict(console_scripts=[
        'space_tracer = space_tracer:main']),
    classifiers=[  # from https://pypi.org/classifiers/
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Debuggers",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Development Status :: 5 - Production/Stable",
        "Environment :: Console"
    ],
)
