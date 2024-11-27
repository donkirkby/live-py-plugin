## PyCharm Development
There are two levels of PyCharm development. It's probably best to start with
the [Python code] that runs in all versions of the Live Coding in Python project
as well as the [Python tests]. The second level is to work on the Java code of
the PyCharm extension. For either level, you'll need IntelliJ [IDEA], and follow
the [plugin development guidelines][idea-dev]. You'll also need the
[git download instructions] if you don't already have it.

If you're trying to find the Java code for some feature of PyCharm,
put a breakpoint in `ActionUtil.performDumbAwareWithCallbacks()`, then use the
feature and step through the code after the breakpoint.

[git download instructions]: https://git-scm.com/downloads

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
    the change notes in `pycharm/src/main/resources/META-INF/plugin.xml`. Keep
    the version in sync with `html/meta.yaml`.
3. Run the `verifyPlugin` and `buildPlugin` Gradle tasks. Check that you're
    verifying against the latest release, and that the `sinceBuild` matches
    the oldest verified release. You should get a `livepy-X.Y.Z.zip` file for
    the new version under `pycharm/build/distributions`.
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
