# Contributing to the Live Coding in Python Project
If you like this project and want to make it better, help out. It could be as
simple as sending [@donkirkby@hachyderm.io] a nice note on Mastodon, you could
report a bug, or pitch in with some development work. There are usually some
small issues labeled as [good first issues] or some larger ones with
[help wanted].

## Bug Reports and Enhancement Requests
Please create issue descriptions [on GitHub][issues]. Be as specific as possible.
Which version are you using? What did you do? What did you expect to happen? Are
you planning to submit your own fix in a pull request? Please include a small
code sample and what you would like the live code display to show for that code.

## Development environments
Depending on what you want to work on, there are several possible develpment
environments to choose from. How to set up and use each environment is described
in its own `contributing-*.md` file.

* Browser tutorials - this is the easiest environment to set up, and a great
  place to start, because it all runs in your browser without installing
  anything. You can edit a tutorial in a markdown file, including the code
  blocks that become live coding samples. You can also edit the Javascript code
  that displays the live coding samples. See `contributing-browser-tutorial.md`
  for details.
* Browser full - you can still run this in your browser without installing
  anything, but it takes a few minutes to start, and takes more storage space on
  GitHub's codespaces. You can make changes to the live coding Python library,
  as well as editing all the features you can edit in the browser tutorials
  environment. See `contributing-browser-full.md` for details.
* PyCharm - this takes a while to install, but you can make changes to the live
  coding Python library or the PyCharm plugin, as well as stepping through the
  code in debug mode. See `contributing-pycharm.md` for details.
* Emacs - lets you edit the Emacs minor mode. See `contributing-emacs.md` for
  details.
* SublimeText - lets you edit the SublimeText plugin. See
  `contributing-sublime.md` for details.
* Space Tracer - the command-line version that you can edit in any Python
  environment. See `contributing-space-tracer.md` for details of how to publish
  a new release on PyPI.

## Coding Guidelines
The Python code should follow PEP8 guidelines, and the Java code should be
compatible with Java 11. The plugin verifier will check for Java compatibility
with the different versions of the IntelliJ platform.

If you're adding features, please make sure to add tests as well. PyCharm tests
are at `test/PySrc/tests`, Javascript tests are at `html/src`, and Java tests
are at `pycharm/src/test`. See the `.github/workflows` folder for details on how
each set of tests gets run.

You can also run the fuzzing test like this:

    $ . .tox/py311/bin/activate
    (py311) $ pip install --extra-index-url https://gitlab.com/api/v4/projects/19904939/packages/pypi/simple pythonfuzz
    Looking in indexes: https://pypi.org/simple, https://gitlab.com/api/v4/projects/19904939/packages/pypi/simple
    [...]
    Successfully installed pythonfuzz-1.0.10
    (py311) $ cd test/PySrc/tests/
    (py311) $ PYTHONPATH=../../../plugin/PySrc/ python fuzz.py --runs 5000 --max-input-size 100
    #0 READ units: 1
    #1 NEW     cov: 0 corp: 1 exec/s: 39 rss: 45.2890625 MB
    #2 NEW     cov: 2742 corp: 2 exec/s: 195 rss: 45.58203125 MB
    #27 NEW     cov: 2770 corp: 3 exec/s: 224 rss: 45.9609375 MB
    [...]
    #4908 NEW     cov: 4114 corp: 187 exec/s: 69 rss: 127.57421875 MB
    #4948 NEW     cov: 4117 corp: 188 exec/s: 32 rss: 128.6015625 MB
    #4970 NEW     cov: 4118 corp: 189 exec/s: 67 rss: 128.6015625 MB
    did 5000 runs, stopping now.
    (py311) $

It generates random Python source code, runs it through space tracer, then
strips off the live coding display and compares the stripped report with the
source code. Any differences trigger a failure.

## Adding Support For a New Editor

If you want to use live coding with a new editor, you can add basic support
with two features:

First, launch a process like this:

    PYTHONPATH=plugin/PySrc/ python -m space_tracer -

Then pass the source code from the editor as the standard input for that
process, and capture the standard output.

Second, display that standard output beside the source code, and keep the two
windows synchronized as they scroll up and down.

If you use your editor's plug-in system to package that up with the
`plugin/PySrc/space_tracer` folder, then you're ready to publish your first
release.

To add more features, look at the options by running:

    python plugin/PySrc/code_tracer.py -h

The canvas commands are for turtle graphics and matplotlib: `bgcolor`,
`create_line`, `create_polygon`, `create_text`, and `create_image`. Look at
`SplitFileEditor.TurtleCanvas` in the `pycharm` project for an example.

The driver command and arguments let you use another script or unit tests to
call the live coding file.

[issues]: https://github.com/donkirkby/live-py-plugin/issues?state=open
[@donkirkby@hachyderm.io]: https://hachyderm.io/@donkirkby
[good first issues]: https://github.com/donkirkby/live-py-plugin/labels/good%20first%20issue
[help wanted]: https://github.com/donkirkby/live-py-plugin/labels/help%20wanted
[milestone]: https://github.com/donkirkby/live-py-plugin/milestones
