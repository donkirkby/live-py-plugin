---
title: Getting Started with Live Coding in VS Code
subtitle: Instantly Visualize Your Code
---

Live Coding in Python lets you run your Python code as you type it. For
example, this code assigns a variable and prints it.

    s = 'Hello'
    s += ', World!'

When you start live coding, a panel opens on the right showing what each
variable contains after every line runs. You don't even have to save the file.

## Installation

1. Open VS Code.
2. Go to the Extensions view (Cmd+Shift+X / Ctrl+Shift+X).
3. Search for "Live Py" and click Install.

Or install from a `.vsix` file: Cmd+Shift+P → "Extensions: Install from
VSIX..." and select the file.

## Quick Start

1. Open a Python file (`.py`).
2. Click the turtle icon in the editor title bar, or press
   **Cmd+Shift+L** (Mac) / **Ctrl+Shift+L** (Windows/Linux).
3. A panel opens on the right showing the live trace.
4. Edit your code — the trace updates automatically.
5. To stop, click the stop icon or press **Cmd+Shift+K** / **Ctrl+Shift+K**.

You can also use the command palette: Cmd+Shift+P → "Live Py: Start
Live Coding".

## Live Coding Display

Here's a simple example showing how the display works:

    s = 'Hello'                              | s = 'Hello'
    s += ', World!'                          | s = 'Hello, World!'

The left side is your code. The right side shows the value of each variable
after that line executes.

Loops show multiple iterations in columns:

    for i in range(3):                       | i = 0      | i = 1      | i = 2
        print(i)                             | print('0') | print('1') | print('2')

## Turtle Graphics

If your code uses the `turtle` module, a canvas panel automatically appears
showing the drawing:

    import turtle as t

    t.bgcolor('ivory')
    t.fillcolor('blue')
    t.begin_fill()
    for _ in range(4):
        t.forward(100)
        t.right(90)
    t.end_fill()

The extension detects turtle/matplotlib usage automatically — no need to
switch modes manually.

## Matplotlib Preview

Matplotlib plots are also displayed automatically:

    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots()
    ax.plot([1, 2, 3], [1, 4, 9])
    plt.show()

## Switching Between Files

When you switch to a different Python file, the trace updates automatically.
If the new file uses turtle or matplotlib, the canvas panel appears. If not,
it disappears.

## Settings

Open VS Code settings (Cmd+, / Ctrl+,) and search for "Live Py" to
configure:

- **Python Path** — which Python interpreter to use
- **Debounce** — delay before updating (default 300ms)
- **Canvas Size** — width and height for turtle/matplotlib
- **Driver** — a script to call your code (e.g., a test runner)
- **Time Limit** — maximum execution time (default 10 seconds)

## Live Unit Tests

You can use a driver script to run unit tests on the file you're editing.
Set the `livePy.driver` setting to your test command:

    "-m unittest test_my_module.py"

Then open your source file and start live coding. The test results will appear
in the trace display, updating as you edit.

## Keyboard Shortcuts

| Action | Mac | Windows/Linux |
|--------|-----|---------------|
| Start Live Coding | Cmd+Shift+L | Ctrl+Shift+L |
| Stop Live Coding | Cmd+Shift+K | Ctrl+Shift+K |

## Learn More

There are some extra features available if you install the
[space tracer library][space_tracer]. You can find descriptions of all the
other Live Coding in Python plugins and tools by visiting the
[project home page][livepy].

[space_tracer]: starting_space_tracer.md
[livepy]: index.md
