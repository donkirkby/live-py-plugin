live-py-plugin
==============

Live coding in Python implemented as an Eclipse plug in.

Installing
----------

1. Install the [PyDev plugin][pydev] and Eclipse if you don't already have them.
2. Download the latest version of the live-py plugin jar file from the [downloads page][downloads].
3. Copy the plugin jar file to Eclipse's dropins folder. On my workstation I was able to do that in two different ways.
    1. Copy it to the system-wide Eclipse installation.
    
             sudo cp live-py_1.0.0.201206132143.jar /usr/lib/eclipse/dropins/
    2. Copy it to your `.eclipse` folder in your home directory. This doesn't require sudo permission.
       
             mkdir ~/.eclipse/org.eclipse.platform_3.7.0_155965261/dropins
             cp live-py_1.0.0.201206132219.jar \
             ~/.eclipse/org.eclipse.platform_3.7.0_155965261/dropins
4. Restart Eclipse.
5. Open any Python file, and add a comment like this:

        # echo on
   You should see an extra column next to the line numbers that shows the results of any code that runs. It may be less distracting to turn off the line numbers.

Uninstalling
------------

1. Delete the jar file from the dropins directory.
2. Restart Eclipse.

Working Features
----------------
- local variable assignments
- looping
- function calls, and multiple calls.
- compile errors
- runtime exceptions
- infinite loops halted.

To do
-----
- print statements don't work.
- Any syntax error currently blanks out the results, should just show previous result with error marker.
- Figure out how to add a horizontal scroll bar.
- Move ruler to right side.

[pydev]: http://pydev.org/download.html
[downloads]: https://github.com/donkirkby/live-py-plugin/downloads
