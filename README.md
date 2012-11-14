live-py-plugin
==============

Live coding in Python implemented as an Eclipse plug in.

To see how it works, watch the [demo video][video] or read the 
[blog post][blog].

Installing
----------

1. Install the [PyDev plugin][pydev] and Eclipse if you don't already have them.
2. Download the latest version of the live-py plugin jar file from the 
   [downloads page][downloads].
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

Uninstalling
------------

1. Delete the jar file from the dropins directory.
2. Restart Eclipse.

Working Features
----------------
- turtle graphics, including filled polygons
- local variable assignments
- looping
- function calls, and multiple calls.
- compile errors
- runtime exceptions
- infinite loops halted.
- print statements work.

To do
-----
- PyUnit integration
- canvas coordinates?
- Any syntax error currently blanks out the results, should just show previous 
result with error marker.

Creating an Install Package
---------------------------
1. Open live-py/plugin.xml, and increment the Version field.
2. Click the Export Wizard in the bottom right corner.
3. Change the destination to Directory, and click Finish.
4. Go to the directory you chose, and find the .jar file. You can distribute
   that as your install package.

[pydev]: http://pydev.org/download.html
[downloads]: https://github.com/donkirkby/live-py-plugin/downloads
[video]: http://www.youtube.com/watch?v=LV3aFRHlAEQ
[blog]: http://donkirkby.blogspot.ca/2012/11/live-coding-in-python-v2.html
