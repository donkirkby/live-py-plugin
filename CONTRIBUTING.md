# Contributing to the Live Coding in Python Project
If you like this project and want to make it better, help out. It could be as
simple as sending [@donkirkby] a nice note on Twitter, you could report a bug,
or pitch in with some development work. There are usually some issues labeled as
[good first issues] or [help wanted].

## Bug Reports and Enhancement Requests
Please create issue descriptions [on GitHub][issues]. Be as specific as possible.
Which version are you using? What did you do? What did you expect to happen? Are
you planning to submit your own fix in a pull request? Please include a small
code sample and what you would like the live code display to show for that code.

## PyCharm Development
You can work on the PyCharm version of the plugin with IntelliJ [IDEA], and
follow the [plugin development guidelines][idea-dev].

If you're trying to find the code for some feature of PyCharm, put a breakpoint
in `ActionUtil.performActionDumbAware()`, then use the feature and step
through the code after the breakpoint.

### Running from Source Code
1. Download and install IntelliJ IDEA, then open the `live-py-plugin/pycharm` project.
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

### Publish a new release for the PyCharm plugin
1. Check that all the Python unit tests pass, by running tox.
2. Update the version number and change notes in `about.py` and
    `pycharm/resources/META-INF/plugin.xml`
3. Right-click the plugin module in the Project view and select Prepare Plugin
    Module 'livepy' For Deployment in the context menu. It should produce a
   `livepy.jar` file. If it produces `livepy.zip`, look inside to see what extra
   jar file it included.
4. Install the new plugin jar into your IntelliJ or PyCharm. Sometimes it
    behaves differently as a jar. From the File menu, choose Settings....
5. Navigate down to the plugins section, click on the gear icon at the top,
    and choose Install plugin from disk... from the menu.
6. Select the jar file you just created, and click the Restart button.
7. Once it restarts, open a Python file, and check that live coding works.
8. Commit the version number changes, push, and create a release on GitHub.
    (Finish the other releases before marking the release on GitHub, if you're
    releasing more.)
9. Upload the jar file to the plugin repository by clicking the Update plugin
    button on the [plugin page].

[IDEA]: https://www.jetbrains.com/idea/download
[idea-dev]: https://www.jetbrains.com/help/idea/2016.3/plugin-development-guidelines.html
[idea-py]: https://plugins.jetbrains.com/idea/plugin/631-python
[configure]: https://www.jetbrains.com/help/idea/2016.3/configuring-intellij-platform-plugin-sdk.html
[plugin page]: https://plugins.jetbrains.com/plugin/9742

## Emacs Development
Install the `live-py-mode` package as described in the README file, then replace all the files in
`~/.emacs.d/elpa/live-py-mode-X.Y` with symbolic links to your git repository.

Releases are built automatically on each commit, and published at [melpa.org].
Stable releases are built from each tag, but do update the version number.

### Debugging live-py-mode in Emacs
When adding temporary debugging messages with the `message` function, it is
recommended to open a second Emacs frame for the Messages buffer by typing
`C-x 5 2` in the Messages buffer.

Run the commands listed in `.github/workflows/python-package.yml` in the section
"Test with Emacs" manually to check for compiler warnings and test regressions.


## Sublime Text Development
Manually install the package using symbolic links to the files and
`space_tracer` folder, so changes will be loaded when you restart Sublime Text.

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

### Publish a new release for the Sublime Text plugin
1. Update the version number in `about.py` and run the `sublime_publish.py`
    script.
2. Commit the version number changes and the new package zip, push, and create a
    release on GitHub. (Finish the other releases before marking the release on
    GitHub, if you're releasing more.)

## Space Tracer Development
`space_tracer` is the command-line version that you can install with `pip`. It
doesn't require any special development tools.

### Deploying a new release of Space Tracer
The details are at [packaging.python.org], but the main steps are:

1. Update the version number in `about.py` and development status in `setup.py`.
2. Activate the latest Python's virtual environment.

        source .tox/py39/bin/activate

3. Install the build tools.

        python -m pip install --upgrade setuptools wheel twine

4. Build the release files.

        python setup.py sdist bdist_wheel

5. Upload the release to PyPI. You'll need a user name and password.

        ls dist/*
        twine upload dist/*

6. Check that the new version is on the [package page], and try installing it.

        pip install --no-cache space_tracer

7. Remove the uploaded files and recreate the tox environment. (The browser
    version uses these files, so don't remove them until you've deployed that.)

        rm dist/*
        deactivate
        tox -r -epy39

8. Commit the version number changes, push, and create a release on GitHub.
    (Finish the other releases before marking the release on GitHub, if you're
    releasing more.)

[packaging.python.org]: https://packaging.python.org/tutorials/packaging-projects/
[package page]: https://pypi.org/project/space-tracer/

## Browser Development
The browser version uses the [Pyodide] project to run Python code in the
browser. To start working on the Javascript code, [install nvm].

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
    nvm install 16
    nvm use 16
    cd /path/to/live-py-plugin/html
    npm install
    npm start

This lets you test the basic Javascript without Pyodide or the theme. See the
next section for how to run the full site. The web site uses the
[Bulma Clean theme], which is based on [Bulma]. The [Bulma colours] can be
particularly helpful to learn about.

### Updating and Testing
To deploy the latest version to the web site, you'll need the `space-tracer`
package distribution files, so follow those instructions at least as far as
running `setup.py`.

The first time you build, you'll need to clone the [Pyodide] project from
GitHub, and install Docker. After that, follow these steps for each release.

1. Update the version number in `html/meta.yaml`.
2. Copy the package distribution files into the Pyodide project. If you cloned
   Pyodide and this project as subfolders of the same parent, then you can do
   something like this:

       cd /path/to/live-py-plugin
       sudo rm -rf ../pyodide/packages/space-tracer/
       mkdir ../pyodide/packages/space-tracer
       cp html/meta.yaml ../pyodide/packages/space-tracer/
       cd ../pyodide/packages/space-tracer/
       tar xzf ../../../live-py-plugin/dist/space_tracer-4.0.1.tar.gz

   Replace the version number with whatever you just built.
3. Find the `remove_modules.txt` file in the pyodide project, and remove
   `turtle.py` from the list.
4. After the package files are in place, run Pyodide's `run_docker` script. See
   the Pyodide project for full instructions. Sometimes, I've had to remove all
   build products and rebuild. Either `make clean` or `git clean -f -Xd`.

       sudo ./run_docker --pre-built PYODIDE_PACKAGES=core,space-tracer,matplotlib make

To update the ReactJS files, change to the `html` folder, and run
`npm run build`. You can also use `npm start` to test the ReactJS files without
Pyodide.

After updating the files, test them locally with GitHub pages (next section).
Then commit the file changes, push, and create a release on GitHub. (Finish the
other releases before marking the release on GitHub, if you're releasing more.) 

[Pyodide]: https://github.com/iodide-project/pyodide
[install nvm]: https://github.com/nvm-sh/nvm#installing-and-updating
[Bulma Clean theme]: https://github.com/chrisrhymes/bulma-clean-theme
[Bulma]: https://bulma.io/documentation/
[Bulma colours]: https://bulma.io/documentation/overview/colors/

### Testing GitHub Pages locally
GitHub generates all the web pages from markdown files, but it can be useful to
test out that process before you commit changes. See the detailed instructions
for setting up [Jekyll], but the main command is this:

    cd docs
    bundle exec jekyll serve

You can also run the `html/serve.sh` script to do the same thing.

[Jekyll]: https://help.github.com/en/github/working-with-github-pages/testing-your-github-pages-site-locally-with-jekyll

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
[@donkirkby]: https://twitter.com/donkirkby
[melpa.org]: https://melpa.org
[good first issues]: https://github.com/donkirkby/live-py-plugin/labels/good%20first%20issue
[help wanted]: https://github.com/donkirkby/live-py-plugin/labels/help%20wanted
[milestone]: https://github.com/donkirkby/live-py-plugin/milestones
