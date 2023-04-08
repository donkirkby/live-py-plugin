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

## Coding Guidelines
The Python code should follow PEP8 guidelines, and the Java code should be
compatible with Java 11. The plugin verifier will check for Java compatibility
with the different versions of the IntelliJ platform.

If you're adding features, please make sure to add tests as well. PyCharm tests
are at `test/PySrc/tests`, Javascript tests are at `html/src`, and Java tests
are at `pycharm/src/test`. See the `.github/workflows` folder for details on how
each set of tests gets run.

## PyCharm Development
There are two levels of PyCharm development. It's probably best to start with
the [Python code] that runs in all versions of the Live Coding in Python project
as well as the [Python tests].
The second level is to work on the Java code of the PyCharm extension. For
either level, you'll need IntelliJ [IDEA], and follow the
[plugin development guidelines][idea-dev]. You'll also need the
[git download instructions] if you don't already have it.

If you're trying to find the Java code for some feature of PyCharm,
put a breakpoint in `ActionUtil.performDumbAwareWithCallbacks()`, then use the
feature and step through the code after the breakpoint.

### Running from Source Code
1. Download and install IntelliJ IDEA, then open the `live-py-plugin`
   project. Wait a while for IDEA to download all the tools for the Gradle
   project. 
2. Install the Python plugin. It will probably prompt you when you open the
   project. Note that there's a [full version][idea-py] of the Python plugin for
   IDEA Ultimate and [community edition][idea-py-ce] for IDEA Community Edition.
3. You might have to add a Python SDK as well. One good way to get a useful
   Python configuration is to run tox in the `live-py-plugin` folder. Then
   configure a Python SDK using `live-py-plugin/.tox/py36/bin/python3.6`, for
   example.
4. If the Gradle window isn't already open, from the Help menu, choose Find
   Action... and search for Gradle with the elephant icon. Click on it to open
   the Gradle window.
5. In the Gradle window, double-click on livepy: Tasks: intellij: runIde to
   launch a second copy with your plugin. It will prompt you to set up a new
   project.
6. In the new project, create a `.py` file. Then install the Python plugin when
   it prompts you. That will make you restart IDEA.
7. Configure a Python SDK in the new project. It's in the File menu under
   Project Structure.

Once you've got everything working with the current IDEA version selected in
`build.gradle.kts`, you might want to use different versions to reproduce a bug
or test compatibility with new features. To do that, find the exact build
number you want to use. Start with the [IDEA build number ranges], then go to
the [IDEA source code], and filter the tag names for the release you want.
Finally, go to the Python plugin page, and find which version is compatible
with the IDEA version you want. Remember the Python plugin has a
[full version][idea-py] and a [community edition][idea-py-ce] to go with the
two types of IDEA release.

[IDEA build number ranges]: https://plugins.jetbrains.com/docs/intellij/build-number-ranges.html
[IDEA source code]: https://github.com/JetBrains/intellij-community

### Publish a new release for the PyCharm plugin
1. Check that all the Python unit tests pass, by running tox.
2. Update the version number in `about.py` and `pycharm/build.gradle.kts`, and
    the change notes in `pycharm/src/main/resources/META-INF/plugin.xml`.
3. Run the `buildPlugin` Gradle task. It should produce a `livepy-X.Y.Z.zip`
   file for the new version.
4. Install the new plugin zip file into your IntelliJ or PyCharm. Sometimes it
    behaves differently as a zip file. From the File menu, choose Settings....
5. Navigate down to the plugins section, click on the gear icon at the top,
    and choose Install plugin from disk... from the menu.
6. Select the zip file you just created, and click the Restart button.
7. Once it restarts, open a Python file, and check that live coding works.
8. Commit the version number changes, push, and create a release on GitHub.
    (Finish the other releases before marking the release on GitHub, if you're
    releasing more.)
9. Upload the zip file to the plugin repository by clicking the Update plugin
    button on the [plugin page].

[Python code]: plugin/PySrc/space_tracer
[Python tests]: test/PySrc/tests
[IDEA]: https://www.jetbrains.com/idea/download
[idea-dev]: https://www.jetbrains.com/help/idea/2016.3/plugin-development-guidelines.html
[idea-py]: https://plugins.jetbrains.com/idea/plugin/631-python
[idea-py-ce]: https://plugins.jetbrains.com/plugin/7322-python-community-edition
[configure]: https://www.jetbrains.com/help/idea/2016.3/configuring-intellij-platform-plugin-sdk.html
[plugin page]: https://plugins.jetbrains.com/plugin/9742

## Emacs Development
Install the `live-py-mode` package as described in the README file, then replace all the files in
`~/.emacs.d/elpa/live-py-mode-X.Y` with symbolic links to your git repository.
You'll also need the [git download instructions] if you don't already have it.

### Publish a new release for the Emacs package
Releases are built automatically on each commit, and published at [melpa.org].
Stable releases are built from each tag, but there are some helpful steps to
follow for each release.

1. Update the version number in `emacs-live-py-mode/live-py-mode.el`.
2. Click the README badge for Emacs downloads to see the current number, then
   update it in the badge's image URL.

### Debugging live-py-mode in Emacs
When adding temporary debugging messages with the `message` function, it is
recommended to open a second Emacs frame for the Messages buffer by typing
`C-x 5 2` in the Messages buffer.

Run the commands listed in `.github/workflows/python-package.yml` in the section
"Test with Emacs" manually to check for compiler warnings and test regressions.


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

### Publish a new release for the Sublime Text plugin
1. Update the version number in `about.py` and run the
   `test/PySrc/tools/sublime_publish.py` script.
2. Commit the version number changes and the new package zip, push, and create a
    release on GitHub. (Finish the other releases before marking the release on
    GitHub, if you're releasing more.)

## Space Tracer Development
`space_tracer` is the command-line version that you can install with `pip`. It
doesn't require any special development tools, beyond Python and the
[git download instructions] if you don't already have it.

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
browser. There are two levels of browser development. The easiest to start on
is writing tutorials in markdown files. Any text editor will do, you can even
edit in your browser on GitHub. See the section below on how to test GitHub
Pages locally, and the section below that lists all the features of browser
tutorials.

If you're more ambitious and interested in working on the Javascript code,
[install nvm].

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
    nvm install 16
    nvm use 16
    cd /path/to/live-py-plugin/html
    npm install
    npm start

This lets you test the basic Javascript without Pyodide or the theme. See the
next section for how to run the full site. The web site uses the
[Bulma Clean theme], which is based on [Bulma]. The [Bulma colours] can be
particularly helpful to learn about. You'll also need the
[git download instructions] if you don't already have it.

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

       sudo ./run_docker PYODIDE_PACKAGES=core,space-tracer,matplotlib make

To update the ReactJS files, change to the `html` folder, and run
`npm run build`. You can also use `npm start` to test the ReactJS files without
Pyodide.

After updating the files, test them locally with GitHub pages (next section).
Then commit the file changes, push, and create a release on GitHub. (Finish the
other releases before marking the release on GitHub, if you're releasing more.) 

If you want to debug some of the unit tests in Chrome, set a break point and
launch them in debug mode.

1. Pick one of your unit tests, and add a `debugger;` statement where you want
   to pause the debugger.
2. Instead of `npm test`, launch the tests like this:

       npm run test:debug

3. That will print out a message like, "Debugger listening on
   ws://127.0.0.1:9229/..." You might need to know the port number 9229 in a
   later step.
4. Open your browser, go to `chrome://inspect` and click on "Open Dedicated
   DevTools for Node".
5. You may see a list of available node instances you can connect to, or it
   might automatically open one. If you see a list, click on the address
   that you saw in the terminal.
6. You should see the Chrome developer tools, and it should be paused at the
   first line. Click the Resume button in the top right. When the tests get to
   the `debugger;` statement you just added, execution will pause.
7. Step through your test, look at variables, and use all the regular debugging
   tools.

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

### Browser Tutorials
To write a tutorial page, just add a markdown file somewhere under the `docs`
folder, and include `is_react: True` in the front matter.

    ---
    title: Live Python in the Browser
    layout: react
    is_react: True
    hero_image: ../images/some_topic.jpg
    image: /images/some_topic.jpg
    modules: numpy, pillow
    ---

The title just sets the title header, and the React settings turn on the
tutorial features. The images are a little more tricky. `hero_image` is the
image at the top of the page, and its path is relative to the page address.
`image` is the preview image for social media posts, and it's relative to the
site's home page. `modules` lets you load optional Python modules for the page.
Some available modules are Matplotlib, Pillow, and Numpy.

Within a tutorial page, a plain code block will just be displayed with a live
coding display next to it. However, there are extra features you can include.

* **Goal code** - If you include a `### Goal ###` section in the code block,
  it will be executed and compared to the user's code.

      print('This code is visible.')
      ### Goal ###
      print('This code is invisible and the output is compared.')

* **Canvas code samples** - If you want to display graphics instead of
  the live coding display, mark the code block with the `### Canvas ###`
  header. Goal code is also supported for canvas code samples.
* **Static code samples** - If you don't want a code sample to be a live sample,
  you can mark it with the `### Static ###` header.
* **REPL code samples** - If a code sample contains ">>>", it will be treated as
  a static code sample.
* **Live code samples** - If you need to override the REPL detector, mark your
  code sample with the `### Live ###` header.
* **Footnotes** - If you link to `#footnoteX` where `X` is any number, then the
  `<a>` tag will be named `footnoteXref`. Conversely, links to `#footnoteXref`
  will be named `footnoteX`. This lets you link to a footnote and back to the
  reference in the text. You're responsible for keeping the numbers in synch.
  For example, you could put this in the body of the page:
  `[[1]](#footnote1)`. Then you could put its mate at the bottom:
  `[[1]](#footnote1ref)`.

For visual tutorials like turtle graphics or Matplotlib, I like to use an image
of the start display and the end display as the hero image and social media
preview. For example, if I wanted the reader to turn this:

    import matplotlib.pyplot as plt
    plt.plot([1, 2, 5, 3])
    plt.show()

Into this:

    import matplotlib.pyplot as plt
    plt.plot([1, 5, 2, 3])
    plt.show()

I'd plot the two side by side:

    import matplotlib.pyplot as plt
    
    f = plt.figure(figsize=(16, 8), facecolor='ivory')
    f.dpi = 40
    plt.subplot(121, aspect=0.5)
    
    plt.plot([1, 2, 5, 3])
    
    plt.subplot(122, aspect=0.5)
    
    plt.plot([1, 5, 2, 3])

    plt.annotate('',
                 (0.52, 0.5),
                 xytext=(0.49, 0.5),
                 xycoords='figure fraction',
                 arrowprops=dict(width=5, headwidth=15))
    plt.savefig('hero.png')
    plt.show()

The DPI setting is good for previewing the result, and then I usually comment it
out.

For turtle tutorials, you need to install some tools like [SvgTurtle] and
[svglib] to convert the turtle commands to SVG and then to PNG.

    import matplotlib.pyplot as plt
    from io import StringIO, BytesIO
    from PIL import Image
    from reportlab.graphics import renderPM
    from svg_turtle import SvgTurtle
    from svglib.svglib import svg2rlg
    
    
    def main():
        f = plt.figure(figsize=(16, 8), facecolor='ivory')
        # f.dpi = 40
        t = SvgTurtle(600, 300)
        plt.subplot(121, aspect=0.5)
        
        t.forward(100)
        t.right(30)
        t.forward(50)
    
        display_turtle(t)
    
        t.reset()
        plt.subplot(122, aspect=0.5)
    
        t.forward(50)
        t.right(45)
        t.forward(100)
    
        display_turtle(t)
        
        plt.annotate('',
                     (0.53, 0.5),
                     xytext=(0.5, 0.5),
                     xycoords='figure fraction',
                     arrowprops=dict(width=5, headwidth=15))
        plt.savefig('hero.png')
        plt.show()
    
    
    def display_turtle(t):
        drawing = svg2rlg(StringIO(t.to_svg()))
        png_bytes = BytesIO()
        renderPM.drawToFile(drawing, png_bytes, 'PNG')
        img = Image.open(png_bytes)
        plt.imshow(img)
        plt.xticks([])
        plt.yticks([])
    
    
    main()

New pages can be converted from reStructured text using the `convert_tutorial.py`
script.

[SvgTurtle]: https://donkirkby.github.io/svg-turtle/
[svglib]: https://github.com/deeplook/svglib

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
[melpa.org]: https://melpa.org
[good first issues]: https://github.com/donkirkby/live-py-plugin/labels/good%20first%20issue
[help wanted]: https://github.com/donkirkby/live-py-plugin/labels/help%20wanted
[milestone]: https://github.com/donkirkby/live-py-plugin/milestones
[git download instructions]: https://git-scm.com/downloads
