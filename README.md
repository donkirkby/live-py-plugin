live-py-plugin
==============

Live coding in Python implemented as an Eclipse plugin or an Emacs minor mode.

To see how it works, watch the [demo video][video] or read the 
[blog post][blog]. You might also find some useful examples in the 
[tools folder][tools].

Special thanks to [Antti Kaihola][akaihola] and [Christoph Paulik][cpaulik] for contributing the Emacs support.

If you like this project, check out some of my [other projects][projects].

[akaihola]: https://github.com/akaihola
[cpaulik]: https://github.com/cpaulik

Installing the Eclipse plugin
-----------------------------

1. Install the [PyDev plugin][pydev] and Eclipse if you don't already have them.
   It's been tested with PyDev 4.1.0, Eclipse 4.4, Python 2.7 and 3.4. It
   seems to be particularly sensitive to changes in PyDev.
2. Download the latest version of the live-py plugin jar file from the 
   [releases page][releases].
3. Copy the plugin jar file to Eclipse's dropins folder. On my workstation I 
   was able to do that in two different ways.
    1. Copy it to the system-wide Eclipse installation.
    
             sudo cp live-py_2.0.0.201209171018.jar /usr/lib/eclipse/dropins/
    2. Copy it to your `.eclipse` folder in your home directory. This doesn't 
       require sudo permission.
       
             mkdir ~/.eclipse/org.eclipse.platform_3.7.0_155965261/dropins
             cp live-py_2.0.0.201209171018.jar \
             ~/.eclipse/org.eclipse.platform_3.7.0_155965261/dropins
4. Restart Eclipse.
5. Open any Python file, and from the Pydev menu, choose Live Coding.
   You should see an extra panel on the right that shows the results of running
   your code.
6. To try the turtle graphics features, open the Window menu, and choose 
   Show View: Other.... Then under PyDev, click Live Coding Canvas and click OK.

Uninstalling the Eclipse plugin
-------------------------------

1. Delete the jar file from the dropins directory.
2. Restart Eclipse.

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

[melpa]: https://melpa.org/#/getting-started

Uninstalling the Emacs mode
---------------------------
If you installed it with MELPA, just use it to uninstall. If not, follow these
steps:

1. Remove the files you copied into `~/.emacs.d/`:
2. Revert additions to `~/.emacs.d/init.el` or `~/.emacs`.
3. Restart Emacs.

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
[releases]: https://github.com/donkirkby/live-py-plugin/releases
[video]: http://www.youtube.com/watch?v=LV3aFRHlAEQ
[blog]: http://donkirkby.blogspot.ca/2012/11/live-coding-in-python-v2.html
[emacs]: http://www.gnu.org/software/emacs/
[tools]: https://github.com/donkirkby/live-py-plugin/tree/master/test/PySrc/tools
[issues]: https://github.com/donkirkby/live-py-plugin/issues?state=open
[projects]: http://donkirkby.github.io/
[contributing]: https://github.com/donkirkby/live-py-plugin/blob/master/CONTRIBUTING.md
