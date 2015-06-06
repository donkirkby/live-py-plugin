Contributing to the Live Coding in Python Project
=================================================
If you like this project and want to make it better, help out. It could be as
simple as sending Don a nice note on [Google+][g+], you could report a bug,
or pitch in with some development work.

Bug Reports and Enhancement Requests
------------------------------------
Please create issue descriptions [on GitHub][issues]. Be as specific as possible.
Which version are you using? What did you do? What did you expect to happen? Are
you planning to submit your own fix in a pull request? Please include a small
code sample and what you would like the live code display to show for that code.

Development
-----------
Live Coding in Python is an Eclipse Plug-in, so you need the [Plug-in Development Environment (PDE)][pde]. You might find the [PDE tutorial][tutorial] helpful.

If you want to see what's currently being worked on, check out the [waffle board][waffle].

Running from source code
------------------------
1. You need to install the PDE. You can either install the standard edition of Eclipse that includes it, or you can open the Help menu, choose Eclipse Marketplace..., click on the Yoxos marketplace, search for PDE, and install it.
2. Open the plugin.xml file with the Plug-in Manifest Editor.
3. Click on the green play button to launch the Eclipse application.
4. Create a new PyDev project, add a Python file, and then turn on Live Coding.
5. Now you can hack on the live-py source code and see the results.

Creating an Install Package for the Eclipse plugin
--------------------------------------------------
1. Open live-py/plugin.xml, and increment the Version field.
2. Click the Export Wizard in the bottom right corner.
3. Change the destination to Directory, and click Finish.
4. Go to the directory you chose, and find the .jar file. You can distribute
   that as your install package.

[issues]: https://github.com/donkirkby/live-py-plugin/issues?state=open
[g+]: http://google.com/+donkirkby
[pde]: https://eclipse.org/pde/
[tutorial]: http://www.vogella.com/tutorials/EclipsePlugIn/article.html
[waffle]: https://waffle.io/donkirkby/live-py-plugin

