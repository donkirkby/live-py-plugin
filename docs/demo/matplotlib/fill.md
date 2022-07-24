---
title: Matplotlib Fill Tutorial
layout: react
is_react: True
hero_image: ../../images/matplotlib/fill.png
image: /images/matplotlib/fill.png
---
The challenge in this tutorial is to learn how to make Matplotlib fill in areas.
If you're new to Matplotlib, you might want to try the [Matplotlib introduction]
first. If you already know the basics, then change the code below to make your
plot look like the goal plot by filling in different areas with different
colours. You can experiment on your own, or read the rest of the tutorial to
learn the concepts you need. The progress bar shows how close you are to the
goal, and the canvas differences, below right, highlight the differences in red.

If you really mess up the script, you can click the reset button to go back to
the start.

[Matplotlib introduction]: intro.md

## Table of Contents
* [The Challenge]
* [How to Fill]
* [What to Fill]
* [What not to Fill]
* [Where to Fill]

[The Challenge]: #the-challenge
[How to Fill]: #how-to-fill
[What to Fill]: #what-to-fill
[What not to Fill]: #what-not-to-fill
[Where to Fill]: #where-to-fill

## The Challenge
    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 256
    X = np.linspace(-np.pi, np.pi, n, endpoint=True)
    Y = np.sin(2*X)
    
    plt.axes([0.025, 0.025, 0.95, 0.95])
    
    plt.plot(X, Y+1, color='blue', alpha=1.00)
    plt.plot(X, Y-1, color='blue', alpha=1.00)
    
    plt.xlim(-np.pi, np.pi)
    plt.xticks([])
    plt.ylim(-2.5, 2.5)
    plt.yticks([])
    plt.show()
    
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    n = 256
    X = np.linspace(-np.pi, np.pi, n, endpoint=True)
    Y = np.sin(2*X)
    
    plt.axes([0.025, 0.025, 0.95, 0.95])
    
    plt.fill_between(X, 1, Y+1, facecolor='blue', alpha=0.25)
    plt.plot(X, Y+1, color='blue', alpha=1.00)
    
    plt.fill_between(X, -1, Y-1, (Y-1) > -1, facecolor='blue', alpha=0.25)
    plt.fill_between(X, -1, Y-1, (Y-1) < -1, facecolor='red', alpha=0.25)
    plt.plot(X, Y-1, color='blue', alpha=1.00)
    
    plt.xlim(-np.pi, np.pi)
    plt.xticks([])
    plt.ylim(-2.5, 2.5)
    plt.yticks([])
    plt.show()

## How to Fill
The main thing to learn about in this tutorial is the `fill_between()` function.
A basic call looks like `plt.fill_between(x, y1, y2)`. That's roughly equivalent
to `plt.plot(x, y1)` and `plt.plot(x, y2)`, plus it fills in the area between
the two lines.

Can you find the two lines that will match the goal output?

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 2+3*X
    Y2 = 5
    
    plt.fill_between(X, Y1, Y2)
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 2+3*X
    Y2 = 3+2*X
    
    plt.fill_between(X, Y1, Y2)
    
    plt.show()

## What to Fill
That lets you fill in an area of the plot, but the default colours are intended
for line graphs. They're a bit intense to use for filling, so you can reduce
the alpha value. The lower the alpha value, the more transparent the fill.

In this example, the polynomial curves are more complex, so we don't want the
fill to be too distracting. Adjust the colour parameters to match the goal.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 5*X**2 - 250
    Y2 = X**3
    
    plt.fill_between(X, Y1, Y2, color='blue', alpha=1.00)
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 5*X**2 - 250
    Y2 = X**3
    
    plt.fill_between(X, Y1, Y2, color='green', alpha=0.50)
    
    plt.show()

## What not to Fill
If you look closely, particularly at low alpha values, you can see that the
filled area has a darker edge. If you want to control the fill separately from
the edge, you can use `facecolor` and `edgecolor` instead of `color`.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 5*X**2 - 250
    Y2 = X**3
    
    plt.fill_between(X, Y1, Y2, color='blue', alpha=0.30)
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 5*X**2 - 250
    Y2 = X**3
    
    plt.fill_between(X, Y1, Y2, facecolor='blue', alpha=0.30)
    
    plt.show()

## Where to Fill
The final trick to learn is the `where` parameter. You can use it to only fill
part of the region. You pass in an array of true or false values that correspond
to each entry in the `x`, `y1`, and `y2` arrays. A nice feature of numpy is that
you can write comparison expressions with the `X` and `Y` arrays to produce
arrays of true and false values.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 5*X**2 - 250
    Y2 = X**3
    
    plt.fill_between(X, Y1, Y2, where=X < 2)
    plt.xlim(-11, 11)
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    X = np.linspace(-10, 10)
    Y1 = 5*X**2 - 250
    Y2 = X**3
    
    plt.fill_between(X, Y1, Y2, where=X < 5)
    plt.xlim(-11, 11)
    
    plt.show()

Now that you have all the skills you need, can you solve [the challenge] at the
start of the tutorial? Here's a summary of the skills you learned:

| Command                   | Explanation                         |
|---------------------------|-------------------------------------|
| `fill_between(x, y1, y2)` | Fill between two plots.             |
| `color=`                  | Change the fill colour.             |
| `alpha=`                  | Adjust the transparency.            |
| `facecolor=`              | Only the filled area, not the edge. |
| `where=`                  | Only include part of the area.      |

For all the details of `fill_between()`, read the [documentation]. This tutorial
was inspired by the work of [Nicolas P. Rougier].

[the challenge]: #the-challenge
[documentation]: https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.fill_between.html
[Nicolas P. Rougier]: https://github.com/rougier/matplotlib-tutorial#regular-plots
