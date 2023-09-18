## Sublime Text Development
Manually install the package using symbolic links to the files and
`space_tracer` folder, so changes will be loaded when you restart Sublime Text.
You'll also need the [git download instructions] if you don't already have it.

1. Open Sublime Text 3.
2. Navigate to Preferences -> Browse packages...
3. Create a `Python Live Coding` folder under the `Packages` folder.
4. Download the source code for this project, and link most of the files from
    the `sublime` folder into the `python_live_coding` package folder you just
    created. By using symbolic links, your Sublime package files will stay the
    same as the ones in this project. Copy the
    `python_live_coding.sublime-settings` file, because your settings might be
    different from the defaults.
5. Also link the `plugin/PySrc/space_tracer` folder into the same
    `Python Live Coding` package folder.
6. New menus should now be available, and the live coding should work for
    simple scripts.
7. Navigate to Preferences -> Package settings -> Python Live Coding
    -> Settings - Default
8. Set path to preferred Python executable, particularly if you want to use a
    virtual environment.
9. Save and close this pane.
10. Open or create a Python file.
11. Navigate to Live Coding -> Start
12. This should bring up a new pane on the right with the live coding display.
13. Begin typing in the left pane and see the code trace results on the right.
14. To debug problems, click on the View menu, and choose Show Console. You can
    add `logger.info()` calls to `python_live_coding.py`, and then restart
    Sublime Text. Instead of restarting, you can save the plugin settings file
    to trigger a reload, although that doesn't always work.

[git download instructions]: https://git-scm.com/downloads

### Publish a new release for the Sublime Text plugin
1. Update the version number in `about.py` and run the
   `test/PySrc/tools/sublime_publish.py` script.
2. Commit the version number changes and the new package zip, push, and create a
    release on GitHub. (Finish the other releases before marking the release on
    GitHub, if you're releasing more.)
