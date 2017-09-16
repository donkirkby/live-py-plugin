---
title: Live Coding in Python
subtitle: An Eclipse Plug In and an Emacs Mode
---
To see how to use it, watch the [demo video][video] or read the 
[getting started page][starting]. You might also find some useful examples in
the [tools folder][tools]. To learn more, read about [how it works][how].

![Screenshot of a star diagram][screenshot]

Special thanks to [Antti Kaihola][akaihola] and [Christoph Paulik][cpaulik] for
contributing the Emacs support. Thanks to [all the contributors][hatrack] for
helping in all kinds of ways.

If you like this project, check out some of my [other projects][projects].

[how]: http://donkirkby.github.io/live-py-plugin/howitworks
[screenshot]: http://donkirkby.github.com/live-py-plugin/images/demo_star.png
[akaihola]: https://github.com/akaihola
[cpaulik]: https://github.com/cpaulik

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

Installing the PyCharm plugin
-----------------------------
The PyCharm plugin is very new, and doesn't work well yet. Try it out if you're
adventurous, and report any bugs you find. So far, the Emacs mode seems to be
the most popular way to use live coding in Python.

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
9. From the Run menu, choose Start Live Coding.

Uninstalling the PyCharm plugin
-------------------------------
1. From the File menu, choose Settings....
2. Click on the Plugins section.
3. In the list of plugins, click on Live Coding in Python.
4. Click the Uninstall button on the right.
5. Click the Restart PyCharm button.
6. Close all the dialog boxes by clicking OK, then let PyCharm restart.


Working Features
----------------
- turtle graphics, including filled polygons (Eclipse only)
- local variable assignments
- looping
- function calls, and multiple calls.
- compile errors
- runtime exceptions
- infinite loops halted.
- print statements work.

If you find the project useful, help us [make it better][contributing].

[pydev]: http://pydev.org/download.html
[video]: https://www.youtube.com/watch?v=Vdr2l3yNFH4
[starting]: http://donkirkby.github.com/live-py-plugin/gettingstarted
[emacs]: http://www.gnu.org/software/emacs/
[tools]: https://github.com/donkirkby/live-py-plugin/tree/master/test/PySrc/tools
[projects]: http://donkirkby.github.io/
[contributing]: https://github.com/donkirkby/live-py-plugin/blob/master/CONTRIBUTING.md
[hatrack]: https://labhr.github.io/hatrack/#repo=donkirkby/live-py-plugin
