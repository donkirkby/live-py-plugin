import setuptools

with open("space_tracer.md", "r") as f:
    long_description = f.read()

# noinspection PyUnresolvedReferences
setuptools.setup(
    name="space_tracer",
    version="2.25",
    author="Don Kirkby",
    author_email="donkirkby@gmail.com",
    description="Trade time for space when debugging your code.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="http://donkirkby.github.io/live-py-plugin/",
    packages=setuptools.find_packages('plugin/PySrc/'),
    package_dir={'': 'plugin/PySrc/'},
    entry_points=dict(console_scripts=[
        'space_tracer = space_tracer:main']),
    classifiers=[
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Debuggers",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Development Status :: 4 - Beta",
        "Environment :: Console"
    ],
)
