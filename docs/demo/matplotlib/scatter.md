---
title: Matplotlib Scatter Tutorial
layout: react
is_react: True
hero_image: ../../images/matplotlib/scatter.png
image: /images/matplotlib/scatter.png
modules: matplotlib
---
The challenge in this tutorial is to combine x and y positions with other data
in Matplotlib. If you're new to Matplotlib, you might want to try the
[Matplotlib introduction] first. If you already know the basics, then change the
code below to make your plot look like the goal plot by changing the appearance
of the points and using the colour of each point to show the angle between the x
axis and the line from the origin to the point. It starts as red at -180°, goes
through yellow and green at -90°, cyan at 0°, blue then purple at 90°, and
finally back to red at 180°. You can experiment on your own, or read the rest of
the tutorial to learn the concepts you need. The progress bar shows how close
you are to the goal, and the canvas differences, below right, highlight the
differences in red (too light) or blue (too dark).

If you really mess up the script, you can click the reset button to go back to
the start.

[Matplotlib introduction]: intro.md

## Table of Contents
* [The Challenge]
* [Changing Colour]
* [Colour Maps]
* [Numpy Basics]
* [Calculating Angles]
* [Point Markers]

[The Challenge]: #the-challenge
[Changing Colour]: #changing-colour
[Colour Maps]: #colour-maps
[Numpy Basics]: #numpy-basics
[Calculating Angles]: #calculating-angles
[Point Markers]: #point-markers

## The Challenge
    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    plt.scatter(X, Y)
    
    plt.xlim([-1.5, 1.5])
    plt.ylim([-1.5, 1.5])
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    angles = np.arctan2(Y, X)
    
    plt.scatter(X, Y, c=angles, cmap='hsv', alpha=0.5, s=75)
    
    plt.xlim([-1.5, 1.5])
    plt.ylim([-1.5, 1.5])
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()

## Changing Colour
The `scatter()` function has several more options beyond the x and y values. You
can change the colour of each point by passing in an array of values to the `c`
parameter. By default, the colour is a gradient from the lowest value to the
highest value in `c`.

Can you find the value of `c` that will match the goal output?

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    plt.scatter(X, Y, c=X)
    
    plt.xlim([-1.5, 1.5])
    plt.ylim([-1.5, 1.5])
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    plt.scatter(X, Y, c=-Y)
    
    plt.xlim([-1.5, 1.5])
    plt.ylim([-1.5, 1.5])
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()

## Colour Maps
By default, low values are displayed as dark blue and high values *map* to light
green. That's not the only colour map available, though. If you look at the
[colour map documentation], you'll find some with colours that change evenly
as the values change, some that use two different colours for positive and
negative values, and some that cycle through colours to end up back where they
started. Change this example's colour map to `viridis`, `hsv`, and `seismic` to
see which is which.

[colour map documentation]: https://matplotlib.org/stable/tutorials/colors/colormaps.html

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    plt.scatter(X, Y, c=X, cmap='viridis')
    
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()

## Numpy Basics
To calculate the angle to each of the random x and y positions, you need to
understand how Numpy handles large sets of numbers. If you're already familiar
with Numpy, you can probably skip to the next section.

The main goals of the Numpy library are to run calculations faster than they
would run in pure Python and to make the code shorter. It accomplishes both of
those with a technique called vectorization. In the following example, `a` and
`a2` are Python lists that get built in Python loops. `b` and `b2` are Numpy
arrays with the same values, but they get built in single Python commands that
trigger loops to run in Numpy's C code: less code for you to write and much
faster to run! `c` and `c2` are also Numpy arrays, but they have random numbers
in them, the same way we generated random numbers for x and y values in the
challenge code.

When you start working with Numpy, it takes a while to learn how to do all the
things you used to do with Python loops. Generally, if you see a for loop
calculating values for a Numpy array, there's probably a better way to do it.

In this example, you can see that `a2` gets filled with `1-x` for each value in
`a`. The exact same thing happens for `b2`, but without a Python loop. `1 - b`
creates a new Numpy array with each entry equal to one minus the entry in `b`.

Experiment with changing the expressions in this code to see the effect on the
calculations, then try to make the values match the goal output.

    import numpy as np
    
    n = 5
    a = []
    for i in range(n):
        a.append(i/n)
    
    b = np.linspace(0, 1, n, endpoint=False)
    c = np.random.normal(0, 0.5, n)
    
    a2 = []
    for x in a:
        a2.append(1-x)
    
    b2 = 1 - b
    c2 = 1 - c
    
    print(a2)
    print(b2)
    print(c2)
    ### Goal ###
    import numpy as np
    
    n = 5
    a = []
    for i in range(n):
        a.append(i/n)
    
    b = np.linspace(0, 1, n, endpoint=False)
    c = np.random.normal(0, 0.5, n)
    
    a2 = []
    for x in a:
        a2.append(1-2*x)
    
    b2 = 1 - 2*b
    c2 = 1 - 2*c
    
    print(a2)
    print(b2)
    print(c2)

## Calculating Angles
Now that you know a little Numpy, you need to know some trigonometry to switch
from x and y values to angles. `cos()` and `sin()` can calculate x and y values
from an angle, but we want to go the other direction. There are a few options,
but we want to convert x and y values to an angle between -180° and 180°. The
equivalent in radians is -π to π.

Matplotlib provides these related functions:
* `arccos(x)` returns the angles of points on the unit circle with the given x
    coordinates.
* `arcsin(y)` returns the angles of points on the unit circle with the given y
    coordinates.
* `arctan(slope)` returns the angles of points on the unit circle with the given
    slopes.
* `arctan2(y, x)` returns the angles of points on the unit circle with slopes
    calculated from each pair of x and y values, corrected to the right quadrant
    based on the signs of the x and y values.

In this example, the first plot shows that `X` and `Y` are all around the unit
circle. The second plot shows that each of the functions matches the original
angles in part of the unit circle, but gets confused by symmetry in other parts.
Can you figure out which function gives an even sequence from -π to π and remove
the others?

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    plt.subplot(121, aspect=1.0)
    n = 1024
    angles = np.linspace(-np.pi, np.pi)
    X = np.cos(angles)
    Y = np.sin(angles)
    
    plt.plot(X, Y)
    
    plt.subplot(122)
    
    plt.plot(angles, np.arccos(X))
    plt.plot(angles, np.arcsin(Y))
    plt.plot(angles, np.arctan(Y/X))
    plt.plot(angles, np.arctan2(Y, X))
    
    plt.tight_layout()
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    plt.subplot(121, aspect=1.0)
    n = 1024
    angles = np.linspace(-np.pi, np.pi)
    X = np.cos(angles)
    Y = np.sin(angles)
    
    plt.plot(X, Y)
    
    plt.subplot(122)
    
    plt.plot(angles, np.arctan2(Y, X))
    
    plt.tight_layout()
    plt.show()

## Point Markers
When data points are randomly distributed, it can be hard to tell how many
points are overlapping. Setting an alpha value makes the colour partially
transparent, so more overlaps make the colour darker. You can also change
the size of the marker with the `s` parameter.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    plt.scatter(X, Y, c=X, alpha=1.0, s=36)
    
    plt.xlim([-1.5, 1.5])
    plt.ylim([-1.5, 1.5])
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 1024
    X = np.random.normal(0, 1, n)
    Y = np.random.normal(0, 1, n)
    
    plt.scatter(X, Y, c=X, alpha=0.25, s=50)
    
    plt.xlim([-1.5, 1.5])
    plt.ylim([-1.5, 1.5])
    plt.xticks([])
    plt.yticks([])
    plt.tight_layout()
    plt.show()

Now that you have all the skills you need, can you solve [the challenge] at the
start of the tutorial? Here's a summary of the skills you learned:

| Command         | Explanation                                   |
|-----------------|-----------------------------------------------|
| `scatter(x, y)` | Scatter plot points with x and y coordinates. |
| `c=`            | Change the colour of each point.              |
| `cmap=`         | Map from numerical values to colours.         |
| `alpha=`        | Sets transparency level.                      |
| `s=`            | Marker size.                                  |
| `arccos()`      | Calculate the angle with an x coordinate.     |
| `arcsin()`      | Calculate the angle with a y coordinate.      |
| `arctan()`      | Calculate the angle with a slope.             |
| `arctan2()`     | Calculate the angle with y and x coordinates. |

For all the details of `scatter()`, read the [documentation]. This tutorial
was inspired by the work of [Nicolas P. Rougier].

[the challenge]: #the-challenge
[documentation]: https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.scatter.html
[Nicolas P. Rougier]: https://github.com/rougier/matplotlib-tutorial#user-content-scatter-plots
