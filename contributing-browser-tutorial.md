## Browser Tutorial Development
The browser version uses the [Pyodide] project to run Python code in the
browser. There are two levels of browser development. The easiest to start on
is writing tutorials in markdown files. Any text editor will do, you can even
edit in your browser on GitHub. To start writing tutorials without having to
install any development tools, edit the project in a GitHub codespace.

1. Go to the [GitHub project page], and click the big green Code button.
2. In the pop up, click on the Codespaces tab.
3. Click the "Create codespace on master" button, and wait for it to finish
   setting up your codespace. Then wait for it to finish up running the
   `postCreateCommand` to install more development tools.
4. When the installation is finished, click in the terminal window at the
   bottom, and launch the web server.

       cd html
       ./serve.sh

5. That should take a few seconds to generate the web pages, and then launch
   a web server on port 4000.
6. You should see a pop up that asks if you want to open port 4000 in your
   browser. Click the "Open in Browser" button, and you should see a copy of the
   current web site.
7. Switch back to the GitHub codespace tab, navigate to the `docs/demo` folder,
   and make some small changes to an existing markdown file. The file will
   automatically be saved, and the web server will automatically regenerate the
   web page.
8. Go back to the browser tab with the web site, and refresh the page. You
   should see your changes.
9. Make some bigger changes, or add a new page. See the [browser tutorials]
   section for a description of all the available features, and remember to
   add any new pages to the menu items in the `docs/_data/navigation.yml` file.

## Editing Javascript
After writing tutorials, the next easiest thing to work on is the Javascript.
The source code is all in the `html` folder, and you can test your changes like
this:

1. If you're running the web server, stop it with <kbd>Ctrl</kbd>+<kbd>C</kbd>.
2. Run the tests to check that you haven't broken anything: `npm test`
3. If tests are broken, edit the application code or the test code to fix them.
   The tests will automatically rerun after the changes. Type `q` to stop the
   tests when they're all passing.
4. Build a new version of the web site, then launch the web server again.

       npm run build
       ./serve.sh

5. Switch back to the browser tab with the web site, and refresh the page. You
   should see your changes.

## Working Locally
If you're more ambitious and interested in working locally instead of in a
GitHub codespace, [install nvm].

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

## Updating Javascript
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
[GitHub project page]: https://github.com/donkirkby/live-py-plugin
[browser tutorials]: #browser-tutorials
[install nvm]: https://github.com/nvm-sh/nvm#installing-and-updating
[Bulma Clean theme]: https://github.com/chrisrhymes/bulma-clean-theme
[Bulma]: https://bulma.io/documentation/
[Bulma colours]: https://bulma.io/documentation/overview/colors/
[git download instructions]: https://git-scm.com/downloads

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
