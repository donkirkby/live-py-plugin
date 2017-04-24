# Contributing to the Live Coding in Python Project #
If you like this project and want to make it better, help out. It could be as
simple as sending Don a nice note on [Google+][g+], you could report a bug,
or pitch in with some development work.

## Bug Reports and Enhancement Requests ##
Please create issue descriptions [on GitHub][issues]. Be as specific as possible.
Which version are you using? What did you do? What did you expect to happen? Are
you planning to submit your own fix in a pull request? Please include a small
code sample and what you would like the live code display to show for that code.

## Eclipse Development ##
Live Coding in Python is an Eclipse Plug-in, so you need the [Plug-in Development Environment (PDE)][pde]. You might
find the [PDE tutorial][tutorial] helpful.

If you want to see what's currently being worked on, check out the [waffle board][waffle].

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
3. Open plugin/plugin.xml and feature/feature.xml, and increment the Version
    field in each file.
4. Commit the version changes, and push to GitHub.
5. Look in the `docs` folder with the update project nested inside, and
    open the update project's `site.xml`.
6. Click the Add Feature... button, type "live", select `live_py_feature`
    from the list, and click OK.
7. Drag `live_py_feature` under the devtools group, and click the Build button.
8. Commit the new version files, push, and create a release on GitHub.
9. Update the version number on the [Eclipse marketplace][mkt].

## PyCharm Development ##
To write a PyCharm plugin, you use IntelliJ [IDEA], and follow the [plugin development guidelines][idea-dev].

1. Download and install it, then open the `live-py-plugin/pycharm` project.
2. Install the [Python plugin][idea-py]. It will probably prompt you when you open the project.
3. Use Git to clone the IDEA source code. (It takes a while.)

        git clone git://git.jetbrains.org/idea/community.git idea

4. Check out the Git label that matches the version of IDEA you are using. Find the build number in the Help: About
    dialog. List the available tags, then check out the one that matches.

        cd idea
        git tag
        git checkout tags/x.y

5. Back in IDEA, [configure] the IntelliJ platform plugin SDK. Don't forget to add a source path for the IDEA source
    code that you downloaded.
6. In that SDK's classpath, add an entry for the Python plugin. Look in the IDEA configuration folder, something like
    this:

        /home/user/.IdeaIC2016.3/config/plugins/python/lib/python.jar

7. You might have to add a Python SDK as well. One good way to get a useful Python configuration is to run tox in the
    `live-py-plugin` folder. Then configure a Python SDK using `live-py-plugin/.tox/py36/bin/python3.6`, for example.
8. From the Run menu, choose Run..., and configure a Plugin launch. Then launch it. It will prompt you to set up a new
    project.
9. In the new project, create a `.py` file. Then install the Python plugin when it prompts you. That will make you
    restart IDEA.
10. Configure a Python SDK in the new project. It's in the File menu under Project Structure.

[IDEA]: https://www.jetbrains.com/idea/download
[idea-dev]: https://www.jetbrains.com/help/idea/2016.3/plugin-development-guidelines.html
[idea-py]: https://plugins.jetbrains.com/idea/plugin/631-python
[configure]: https://www.jetbrains.com/help/idea/2016.3/configuring-intellij-platform-plugin-sdk.html

## Emacs Development ##
Install the `live-py-mode` package as described in the README file, then replace all the files in
`~/.emacs.d/elpa/live-py-mode-X.Y` with symbolic links to your git repository.

### Debugging live-py-mode in Emacs ###
When adding temporary debugging messages it is recommended to open a second
Emacs frame for the Messages buffer by typing `C-x 5 2` in the Messages
buffer.

Run the commands listed in `.travis.yml` in the section "script:" manually
to check for compiler warnings and test regressions.

[issues]: https://github.com/donkirkby/live-py-plugin/issues?state=open
[g+]: http://google.com/+donkirkby
[pde]: https://eclipse.org/pde/
[tutorial]: http://www.vogella.com/tutorials/EclipsePlugIn/article.html
[waffle]: https://waffle.io/donkirkby/live-py-plugin
[pdrel]: http://pydev.org/history_pydev.html
[pyrel]: https://www.python.org/downloads/
[mkt]: https://marketplace.eclipse.org/content/live-coding-python/edit
[100]: https://github.com/donkirkby/live-py-plugin/issues/100
