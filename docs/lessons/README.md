# Live Lessons for Matplotlib #

This folder holds lesson files for a new feature that will soon be added to the
PyCharm version: Matplotlib lessons with live feedback. Feel free to contribute
your ideas for new lessons.

Each lesson contains two files:

1. A lesson file with instructions in the docstring and the starting version of
    a Python script. It also has a pointer to the goal file.
2. A goal file with the final version of the Python script.

The user will open the lesson file and start the live turtle/matplotlib mode.
PyCharm will notice the link to the goal file and display both versions of the
plot. When the user makes the starting version match the goal version, they
will merge into one.

If you have an idea for a lesson, you can describe it in an issue, or you can
create a pull request with the two files. You can copy `example_lesson.py` and
`example_goal.py` as a starting point.