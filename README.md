live-py-plugin
==============

[![Build Status]][travis]
[![Code Coverage]][codecov]

Live coding in Python with PyCharm, Emacs, Sublime Text, Eclipse, or even a browser.

[Build Status]: https://travis-ci.org/donkirkby/live-py-plugin.svg?branch=master
[travis]: https://travis-ci.org/live-py-plugin
[Code Coverage]: https://codecov.io/github/donkirkby/live-py-plugin/coverage.svg?branch=master
[codecov]: https://codecov.io/github/donkirkby/live-py-plugin?branch=master

To see how to use it, watch the [demo video][video] or read the 
[getting started page][starting]. Want to try it without installing anything?
Try the [browser version]. This project includes the [Space Tracer] that trades
time for space when you debug, and the Space Tracer can also be installed as a
command-line tool. You might also find some useful examples in the
[tools folder][tools]. To learn more, read about [how it works][how].

![Screenshot of a star diagram][screenshot]

Special thanks to [James Davies] for contributing the Sublime Text support, and
to [Antti Kaihola][akaihola] and [Christoph Paulik][cpaulik] for
contributing the Emacs support. Thanks to [all the contributors][hatrack] for
helping in all kinds of ways, and thanks to [JetBrains] for the free PyCharm and
IDEA licenses.

If you like this project, check out some of my [other projects][projects].

[how]: http://donkirkby.github.io/live-py-plugin/howitworks
[screenshot]: http://donkirkby.github.com/live-py-plugin/images/demo_star.png
[akaihola]: https://github.com/akaihola
[cpaulik]: https://github.com/cpaulik
[JetBrains]: https://www.jetbrains.com/?from=live-py-plugin
[browser version]: https://donkirkby.github.io/live-py-plugin/demo/
[Space Tracer]: http://donkirkby.github.io/live-py-plugin/space_tracer
[James Davies]: https://github.com/Derfies

Feature Comparison
------------------
So far, the Emacs mode seems to be the most popular way to use live coding in
Python. The PyCharm plugin has the most features.


| Feature              | PyCharm |  Emacs  | Sublime | Eclipse | browser |
| -------------------- | ------- | ------- | ------- | ------- | ------- |
| variable assignments | &check; | &check; | &check; | &check; | &check; |
| looping              | &check; | &check; | &check; | &check; | &check; |
| function calls       | &check; | &check; | &check; | &check; | &check; |
| errors and exceptions| &check; | &check; | &check; | &check; | &check; |
| print function       | &check; | &check; | &check; | &check; | &check; |
| unit tests           | &check; | &check; | &cross; | &check; | &cross; |
| turtle graphics      | &check; | &cross; | &cross; | &check; | &cross; |
| matplotlib preview   | &check; | &cross; | &check; | &check; | &cross; |
| pyglet preview       | &check; | &cross; | &check; | &check; | &cross; |

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

1. Install [GNU Emacs][emacs] if you don't already have it.
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
1. Install [GNU Emacs][emacs] if you don't already have it.
2. Clone the latest version of the live-py Emacs mode:

        git clone https://github.com/donkirkby/live-py-plugin.git

3. Copy the Emacs Lisp file and the supporting Python files into a directory
   which is in your Emacs `load-path`. For example:

        cd live-py-plugin
        cp emacs-live-py-mode/live-py-mode.el plugin/PySrc/*.py ~/.emacs.d/

   Add ~/.emacs.d/ to your `load-path` in `~/.emacs.d/init.el` or `~/.emacs`:

        (add-to-list 'load-path "~/.emacs.d")
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
We'll be adding it to package control soon, but here's the manual method:

1. Open Sublime Text 3.
2. Navigate to Preferences -> Browse packages...
3. Create a `python_live_coding` folder under the `Packages` folder.
4. Download the source code for this project, and copy all the files from the
    `sublime` folder into the `python_live_coding` package folder you just
    created.
5. Also copy the `plugin/PySrc/space_tracer` folder into the same
    `python_live_coding` package folder.
6. New menus should now be available, and the live coding should work for
    simple scripts.
7. Navigate to Preferences -> Package settings -> Python Live Coding
    -> Settings - Default
8. Set path to preferred Python executable, particularly if you want to use a
    virtual environment.
9. Save and close this pane.
10. Open a new window.
11. Navigate to Live Coding -> Start
12. This should bring up a new pane on the right with the live coding display.
13. Begin typing in the left pane and see the code trace results on the right.

Uninstalling the Sublime Text plugin
------------------------------------

1. Open Sublime Text 3.
2. Navigate to Preferences -> Browse packages...
3. Delete the `python_live_coding` package folder.

Installing the Eclipse plugin
-----------------------------

1. Install the [PyDev plugin][pydev] and Eclipse if you don't already have them.
   It's been tested with PyDev 4.5.5, Eclipse 4.4, Python 2.7 and 3.4. It
   seems to be particularly sensitive to changes in PyDev.
2. In Eclipse, choose Help: Eclipse Marketplace... from the menu.
3. Search for Live Coding in Python, and install it.
4. Restart Eclipse.
5. Open any Python file, and from the Live Coding menu, choose Start Live Coding.
   You should see an extra panel on the right that shows the results of running
   your code.
6. To try the turtle graphics features, open the Live Coding menu, and choose
    Start Live Turtle. The panel on the right shows the turtle graphics display.
7. You can also use another script or a unit test to drive your live code.
    Run your driver script or unit test as usual in Eclipse, then click on the
    small arrow next to the big live coding arrow in the toolbar. You should see
    all the different Python and unit test launches listed there. Click on the
    one you want, and it will call the live version of your code. To run a
    single unit test method, open the test file, and type
    <kbd>Ctrl</kbd>+<kbd>F9</kbd>, then choose the test method you want to run.
    Edit your run configurations to give the new configuration a meaningful name,
    then choose it from the live coding arrow's menu.

If you don't want to use the Eclipse marketplace, you can also install from the
[update site][update].

[update]: http://donkirkby.github.io/live-py-plugin/update

Uninstalling the Eclipse plugin
-------------------------------

1. In Eclipse, choose Help: Installation Details from the menu.
2. Select Live Coding in Python, and click the Uninstall... button.
3. Restart Eclipse.

[pydev]: http://pydev.org/download.html
[video]: https://www.youtube.com/watch?v=Vdr2l3yNFH4
[starting]: http://donkirkby.github.com/live-py-plugin/gettingstarted
[emacs]: http://www.gnu.org/software/emacs/
[tools]: https://github.com/donkirkby/live-py-plugin/tree/master/test/PySrc/tools
[projects]: http://donkirkby.github.io/
[contributing]: https://github.com/donkirkby/live-py-plugin/blob/master/CONTRIBUTING.md
[hatrack]: https://labhr.github.io/hatrack/#repo=donkirkby/live-py-plugin
