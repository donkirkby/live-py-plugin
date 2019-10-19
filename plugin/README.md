The Eclipse plugin is currently unsupported. I stopped using it a few years ago,
and when I went to build version 3, I couldn't get it to work.

The Python source code is still the core of this project, and it lives in the
`PySrc` folder.

## Installing the Eclipse plugin

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

## Uninstalling the Eclipse plugin

1. In Eclipse, choose Help: Installation Details from the menu.
2. Select Live Coding in Python, and click the Uninstall... button.
3. Restart Eclipse.

[pydev]: http://pydev.org/download.html

## Eclipse Development ##
Live Coding in Python is an Eclipse Plug-in, so you need the
[Plug-in Development Environment (PDE)][pde]. You might find the
[PDE tutorial][tutorial] helpful.

If you want to see what's currently being worked on, check out the latest
[milestone].

### Running from source code ###
1. You need to install the PDE. You can either install the standard edition of
   Eclipse that includes it, or you can open the Help menu, choose Eclipse
   Marketplace..., click on the Yoxos marketplace, search for PDE, and install it.
2. Open the plugin.xml file with the Plug-in Manifest Editor.
3. Click on the green play button to launch the Eclipse application.
4. Create a new PyDev project, add a Python file, and then turn on Live Coding.
5. Now you can hack on the live-py source code and see the results.

### Creating an Install Package for the Eclipse plugin ###
Be sure that you test everything with the latest versions of [PyDev][pdrel] and
[Python][pyrel].

1. Check that all the Java unit tests run.
2. Use `tox` to check that all the Python unit tests run in the latest versions
   of both Python 2 and Python 3.
3. Open these files and increment the Version field in each file.
    * `plugin/plugin.xml`
    * `feature/feature.xml`
    * `emacs-live-py-mode/live-py-mode.el`
5. Look in the `docs` folder with the update project nested inside, and
    open the update project's `site.xml`.
6. Click the Add Feature... button, type "live", select `live_py_feature`
    from the list, and click OK.
7. Drag `live_py_feature` under the devtools group, and click the Build button.
8. Commit the new version files, push, and create a release on GitHub. (Finish
    the PyCharm release before marking the release on GitHub, if you're releasing
    both).
9. Update the version number on the [Eclipse marketplace][mkt].

[pde]: https://eclipse.org/pde/
[tutorial]: http://www.vogella.com/tutorials/EclipsePlugIn/article.html
[pdrel]: http://pydev.org/history_pydev.html
[pyrel]: https://www.python.org/downloads/
[mkt]: https://marketplace.eclipse.org/content/live-coding-python/edit
