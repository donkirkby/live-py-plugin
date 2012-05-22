live-py-plugin
==============

Live coding in Python implemented as an Eclipse plug in.

Working:
- local variable assignments
- looping
- function calls

Bugs:
- Second call to a function isn't aligned.
- nested loops not supported
- conditional lines in loops are sometimes too short (minor)

To do:
- Any error currently blanks out the results, should just show previous result
with error marker.
- Figure out how to scroll horizontally.
- Move ruler to right side.