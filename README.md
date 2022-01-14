Live Coding in Python
=====================

[![Python Build Status]][actions]
[![HTML Build Status]][actions]
[![Code Coverage]][codecov]
[![PyCharm downloads]][pycharm plugin]
[![Sublime downloads]][sublime plugin]
[![Package Version]][pypi]

Visualize your Python code while you type it in PyCharm, Emacs, Sublime Text, or
even your browser.

[Package Version]: https://badge.fury.io/py/space-tracer.svg
[pypi]: https://pypi.org/project/space-tracer/
[Python Build Status]: https://github.com/donkirkby/live-py-plugin/actions/workflows/py-build.yml/badge.svg?branch=master
[HTML Build Status]: https://github.com/donkirkby/live-py-plugin/actions/workflows/html-build.yml/badge.svg?branch=master
[actions]: https://github.com/donkirkby/live-py-plugin/actions
[Code Coverage]: https://codecov.io/github/donkirkby/live-py-plugin/coverage.svg?branch=master
[codecov]: https://codecov.io/github/donkirkby/live-py-plugin?branch=master
[PyCharm downloads]: https://img.shields.io/jetbrains/plugin/d/9742?label=PyCharm%20%E2%86%93
[pycharm plugin]: https://plugins.jetbrains.com/plugin/9742
[Sublime downloads]: https://img.shields.io/packagecontrol/dt/Live%20Coding%20in%20Python?label=Sublime%20%E2%86%93
[sublime plugin]: https://packagecontrol.io/packages/Live%20Coding%20in%20Python

To see how to use one of the Live Coding in Python plugins, watch the
[demo video][video] or read the getting started pages for [PyCharm], [Emacs],
or [Sublime Text]. Want to try it without installing anything? Try the
[browser version]. You can also try [Space Tracer], the command-line tool that
trades time for space when you debug. You might also find some useful examples
in the [tools folder][tools]. To learn more, read about [how it works][how].

![Screenshot of a star diagram][screenshot]

Special thanks to [James Davies] for contributing the Sublime Text support, and
to [Antti Kaihola][akaihola] and [Christoph Paulik][cpaulik] for
contributing the Emacs support. Thanks to [all the contributors][hatrack] for
helping in all kinds of ways, and thanks to [JetBrains] for the free PyCharm and
IDEA licenses.

If you like this project, check out some of my [other projects][projects].

[how]: http://donkirkby.github.io/live-py-plugin/howitworks.html
[screenshot]: https://donkirkby.github.io/live-py-plugin/images/demo_star.png
[akaihola]: https://github.com/akaihola
[cpaulik]: https://github.com/cpaulik
[JetBrains]: https://www.jetbrains.com/?from=live-py-plugin
[browser version]: https://donkirkby.github.io/live-py-plugin/demo/
[PyCharm]: https://donkirkby.github.io/live-py-plugin/starting_pycharm.html
[Emacs]: https://donkirkby.github.io/live-py-plugin/starting_emacs.html
[Sublime Text]: https://donkirkby.github.io/live-py-plugin/starting_sublime_text.html
[Space Tracer]: https://donkirkby.github.io/live-py-plugin/space_tracer.html
[James Davies]: https://github.com/Derfies

Feature Comparison
------------------
So far, the Emacs mode seems to be the most popular way to use live coding in
Python. The PyCharm plugin has the most features.


| Feature              | PyCharm |  Emacs  | Sublime | browser | space_tracer |
| -------------------- | ------- | ------- | ------- | ------- | ------------ |
| variable assignments |    Y    |    Y    |    Y    |    Y    |       Y      |
| looping              |    Y    |    Y    |    Y    |    Y    |       Y      |
| function calls       |    Y    |    Y    |    Y    |    Y    |       Y      |
| errors and exceptions|    Y    |    Y    |    Y    |    Y    |       Y      |
| print function       |    Y    |    Y    |    Y    |    Y    |       Y      |
| unit tests           |    Y    |    Y    |    Y    |         |       Y      |
| turtle graphics      |    Y    |         |         |    Y    |              |
| matplotlib preview   |    Y    |         |    Y    |         |              |
| pyglet preview       |    Y    |         |    Y    |         |              |

If you find the project useful, help us [make it better][contributing].


Installing the PyCharm plugin
-----------------------------
This assumes you already have [PyCharm] installed, you have configured a
Python interpreter, and you can run a Python script normally.

1. From the File menu, choose Settings....
2. Click on the Plugins section.
3. Click the Browse Repositories... button.
4. Type live coding in the search box, and click on the "Live Coding in Python"
    entry in the list below.
5. Click the green Install button on the right.
6. Click the Restart PyCharm button.
7. Close all the dialog boxes by clicking OK, then let PyCharm restart when it
    asks.
8. Open a Python file.
9. Run the Python file normally, as a script or a unit test.
10. With the same run configuration selected in the drop down, choose Start
    Live Coding from the Run menu.

Uninstalling the PyCharm plugin
-------------------------------
1. From the File menu, choose Settings....
2. Click on the Plugins section.
3. In the list of plugins, click on Live Coding in Python.
4. Click the Uninstall button on the right.
5. Click the Restart PyCharm button.
6. Close all the dialog boxes by clicking OK, then let PyCharm restart.

Installing the Emacs mode
-------------------------
It's probably easiest to use the MELPA package archive, as described in this
section, but the next section gives instructions for installing without MELPA.

1. [Install GNU Emacs] if you don't already have it.
2. [Install MELPA][melpa]. You probably want to follow the instructions
    for the stable version, instead of the default bleeding-edge version.
3. Launch the package installer with `M-x package-list-packages`.
4. Find the package with `C-s live-py-mode`.
5. Mark the package to install with `i`, then execute the installation with
    `x`.
6. Open any Python file, and activate live-py-mode with `M-x live-py-mode`.
   You should see an extra window on the right that shows the results of running
   your code.
7. Type `C-h m` and scroll down to the **Live-Py** section to see all the
    advanced features that let you run other versions of Python or drive your
    live coding from another script or a unit test.

[melpa]: https://melpa.org/#/getting-started

Installing the Emacs mode without MELPA
---------------------------------------
1. [Install GNU Emacs] if you don't already have it.
2. Clone the latest version of the live-py Emacs mode:

        git clone https://github.com/donkirkby/live-py-plugin.git

3. Copy the Emacs Lisp file and the supporting Python files into a directory
   which is in your Emacs `load-path`. For example:

        cd live-py-plugin
        mkdir -p ~/.emacs.d/packages
        cp emacs-live-py-mode/live-py-mode.el plugin/PySrc/space_tracer ~/.emacs.d/packages

   Add ~/.emacs.d/ to your `load-path` in `~/.emacs.d/init.el` or `~/.emacs`:

        (add-to-list 'load-path "~/.emacs.d/packages")
4. Load the Lisp library in your `~/.emacs.d/init.el` or `~/.emacs`:

        (require 'live-py-mode)
5. Restart Emacs.
6. Open any Python file, and activate live-py-mode with `M-x live-py-mode`.
   You should see an extra window on the right that shows the results of running
   your code.
7. Type `C-h m` and scroll down to the **Live-Py** section to see all the
    advanced features that let you run other versions of Python or drive your
    live coding from another script or a unit test.

Uninstalling the Emacs mode
---------------------------
If you installed with MELPA, just use it to uninstall. If not, follow these
steps:

1. Remove the files you copied into `~/.emacs.d/`:
2. Revert additions to `~/.emacs.d/init.el` or `~/.emacs`.
3. Restart Emacs.

Installing the Sublime Text plugin
----------------------------------
It's easiest to install from package control, but you can find a manual method
in the CONTRIBUTING file.

1. Install [package control].
2. Open the command palette by typing
    <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd>.
3. Type "Package" and then select Package Control: Install Package.
4. Start typing "Live Coding in Python" and select it when it appears in the
    list.
5. When it finishes installing, you should see a new "Live Coding" menu.
6. Live coding should now work for simple scripts.
7. Navigate to Preferences -> Package settings -> Live Coding.
8. Set the path to your preferred Python executable, particularly if you want to
    use a virtual environment. The default is `python`.
9. Save and close this pane.
10. Open or create a Python file.
11. Navigate to Live Coding -> Start
12. This should bring up a new pane on the right with the live coding display.
13. Begin typing in the left pane and see the code trace results on the right.

[package control]: https://packagecontrol.io/installation

Uninstalling the Sublime Text plugin
------------------------------------

1. Open Sublime Text 3.
2. Open the command palette by typing
    <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd>.
3. Type "Package" and then select Package Control: Remove Package.
4. Start typing "Live Coding in Python" and select it when it appears in the
    list.

[video]: https://www.youtube.com/watch?v=Vdr2l3yNFH4
[Install GNU Emacs]: http://www.gnu.org/software/emacs/
[tools]: https://github.com/donkirkby/live-py-plugin/tree/master/test/PySrc/tools
[projects]: https://donkirkby.github.io/
[contributing]: https://github.com/donkirkby/live-py-plugin/blob/master/CONTRIBUTING.md
[hatrack]: https://labhr.github.io/hatrack/#repo=donkirkby/live-py-plugin
