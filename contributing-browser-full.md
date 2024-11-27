## Full Browser Development
If you want to work on the core Python library that displays the live coding
results, you can still do that in a GitHub codespace without installing any of
your own development tools. It just takes a few minutes to start, and takes more
storage space than the browser tutorial environment on GitHub's codespaces.

1. Go to the [GitHub project page], and click the big green Code button.
2. In the pop up, click on the Codespaces tab.
3. In the top right of the Codespaces tab, next to the plus sign, click on the
   three dots, and click "New with options...".
4. Click on the dev container configurations, and choose `live-py-plugin-full`.
5. Click on the machine types, and choose one with at least 64GB of storage.
   Currently, that's the 8-core machine.
6. Click the "Create codespace" button, and wait for it to finish setting up
   your codespace. Then wait for it to finish up running the `postCreateCommand`
   to install more development tools.
7. When the installation is finished, click in the terminal window at the
   bottom, and launch the web server.

       cd html
       ./serve.sh

8. That should take a few seconds to generate the web pages, and then launch
   a web server on port 4000.
9. You should see a pop up that asks if you want to open port 4000 in your
   browser. Click the "Open in Browser" button, and you should see a copy of the
   current web site.
10. Switch back to the GitHub codespace tab, navigate to the
    `plugin/PySrc/space_tracer` folder, and make some changes to the Python
    source code. You can also change the unit tests under `test/PySrc/tests`.
11. Stop the web server by clicking in the terminal and typing
    <kbd>Ctrl</kbd>+<kbd>C</kbd>. Then run the tests by running `tox` in the
    project's top directory.
   
        cd ..
        tox

    Not all versions of Python are installed by default, so you can run the tests
    in Python 3.10 with `tox -e py310`. You can install a missing Python version
    with this:

        sudo apt update
        sudo apt install python3.9
    
    For some versions of Python, you might have to register the [dead snakes]
    package repository.
12. Once the tests pass, you can deploy the new Python code on your local copy
    of the web site.

        cd html
        npm run build
        ./serve.sh

13. Go back to the browser tab with the web site, and refresh the page. You
    should see your changes.

[dead snakes]: https://launchpad.net/~deadsnakes/+archive/ubuntu/ppa
[GitHub project page]: https://github.com/donkirkby/live-py-plugin

### Building locally
If you don't want to work in a dev container, you can set up all the tools to
build the web site on your workstation.

The first time you build, you'll need to clone the [Pyodide] project from
GitHub, and install Docker. After that, follow these steps for each release.

1. Update the version number in `html/meta.yaml` and in the `srcFiles` list in
   `html/deploy.js`.
2. Find the `Makefile.envs` file in the pyodide project, and remove
   `turtle.py` from the list.
3. Run `npm run build`. You should see a message that it rebuilt space tracer
   in pyodide.

[Pyodide]: https://github.com/iodide-project/pyodide
