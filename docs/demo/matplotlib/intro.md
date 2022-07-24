---
title: Matplotlib Introduction Tutorial
layout: react
is_react: True
hero_image: ../../images/matplotlib/intro.png
image: /images/matplotlib/intro.png
---
There are a few plotting libraries to choose from in Python, but matplotlib
seems the most common. Once you know the basics of Matplotlib, you might find
Seaborn helpful, and it's built on top of Matplotlib, so it's helpful to know
both. This tutorial is an introduction to the basic features of Matplotlib, and
all the examples run right in your browser. You can try playing with the
features to see how they affect the plot.

## Table of Contents
* [Line Style](#line-style)
* [Limits](#limits)
* [Tick Positions](#tick-positions)
* [Tick Labels](#tick-labels)
* [Spine Positions](#spine-positions)
* [Legendary Plot](#legendary-plot)
* [Highlight Features](#highlight-features)
* [Readability](#readability)

This first example shows the default presentation of a sine curve and a cosine
curve. Try adjusting the data range in `linspace()` or multiplying one of the
curves by a coefficient. The plot updates immediately as you make your changes.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t)
    plt.plot(t, cos_t)
    
    plt.show()

## Line Style
That example was very short, because we just used all the default settings. The
next example should look exactly the same, but it now has all those settings
given as explicit values. Can you adjust each setting to match the goal plot
below? The two plots are compared on the bottom right, with differences
highlighted in red.

If you want more details about the settings, read the matplotlib documentation
on [colours] and [line styles].

[colours]: https://matplotlib.org/stable/tutorials/colors/colors.html
[line styles]: https://matplotlib.org/stable/api/_as_gen/matplotlib.lines.Line2D.html#matplotlib.lines.Line2D.set_linestyle

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='tab:blue', linewidth=1.5, linestyle='-')
    plt.plot(t, cos_t, color='tab:orange', linewidth=1.5, linestyle='-')
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='tab:orange', linewidth=1, linestyle='--')
    plt.plot(t, cos_t, color='tab:blue', linewidth=2.5, linestyle='-')
    
    plt.show()

## Limits
Now, I've switched to some brighter colours, and replaced the default limits on
the axes. Can you adjust the limits to match the goal plot?

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    plt.xlim(t.min()*1.1, t.max()*1.1)
    plt.ylim(sin_t.min()*1.1, sin_t.max()*1.1)

    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    plt.xlim(t.min(), t.max())
    plt.ylim(sin_t.min()*1.5, sin_t.max()*1.5)
    
    plt.show()

## Tick Positions
The default ticks don't show the important points at multiples of &pi;, like the
minimum and maximum points of the curves. `xticks()` and `yticks` let you choose
exactly where to put the ticks.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    pi = np.pi
    plt.xticks([-3, -2, -1, 0, 1, 2, 3])
    plt.yticks([-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1])
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    pi = np.pi
    plt.xticks([-pi, -pi/2, 0, pi/2, pi])
    plt.yticks([-1, 0, 1])
    
    plt.show()

## Tick Labels
The important positions are now marked, but it's not immediately clear what the
values mean. It would be nicer to mark them with &pi; and &pi;/2. Luckily,
`xticks()` and `yticks()` accept label text as well as values, and you can use
LaTeX for mathematical symbols like &pi;. The labels are started, can you fill
out the rest to match the goal plot? The `r` prefix in `r'$-\pi$'` means that
Python treats it as a raw string, and won't try to interpret the backslashes
that are meant for LaTeX.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'-pi/2', r'0', r'?', r'??'])
    plt.yticks([-1, 0, 1])
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    plt.show()

## Spine Positions
The default locations for the axes lines and ticks, or spines, is along the
outer edges of the plot. For some plots, it's clearer to put them through the
middle. The `gca()` function is short for "get current axes", and it returns the
set of X and Y axes, so you can adjust their display. In order to hide one of
the spines, set its colour to `'none'`.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    ax.xaxis.set_ticks_position('bottom')
    ax.spines['bottom'].set_position(('data',-1.1))
    ax.spines['bottom'].set_color('black')
    ax.spines['top'].set_color('black')
    ax.yaxis.set_ticks_position('left')
    ax.spines['left'].set_position(('data',-1.1*pi))
    ax.spines['left'].set_color('black')
    ax.spines['right'].set_color('black')
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5)
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    ax.xaxis.set_ticks_position('bottom')
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['bottom'].set_color('black')
    ax.spines['top'].set_color('none')
    ax.yaxis.set_ticks_position('left')
    ax.spines['left'].set_position(('data',0))
    ax.spines['left'].set_color('black')
    ax.spines['right'].set_color('none')
    
    plt.show()

## Legendary Plot
Usually, it's helpful to label what the data is in each line. That's what a
[legend] is for. The `loc` defaults to `'best'`, but you can choose
`'upper right'` or other common locations. The nice thing about `'best'` is that
it will pick the location where it fits best. Watch how it moves after you label
the cosine line. Then try changing `sin_t` to `-sin_t` in the `plot()` call. Can
you finish setting up the legend?

[legend]: https://matplotlib.org/stable/api/legend_api.html

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5, label='sine')
    plt.plot(t, cos_t, color='orange', linewidth=2.5)
    
    plt.legend(loc='best', frameon=True)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_position(('data',0))
    ax.spines['right'].set_color('none')
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5, label='sine')
    plt.plot(t, cos_t, color='orange', linewidth=2.5, label='cosine')
    
    plt.legend(loc='best', frameon=False)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_position(('data',0))
    ax.spines['right'].set_color('none')
    
    plt.show()

## Highlight Features
Often, you want to point out a particular feature in a plot, so matplotlib lets
you annotate with arrows and text. In this example, we've annotated the sine and
cosine of 3&pi;/4. Can you update it to 2&pi;/3?

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5, label='sine')
    plt.plot(t, cos_t, color='orange', linewidth=2.5, label='cosine')

    pi = np.pi
    t0 = 3*pi/4
    plt.plot([t0,t0],[0,np.sin(t0)], color ='black', linewidth=1.5, linestyle="-")
    plt.scatter([t0,],[np.sin(t0),], 50, color ='black')
    
    plt.annotate(
        r'$\sin(\frac{3\pi}{4})=\frac{\sqrt{2}}{2}$',
        xy=(t0, np.sin(t0)), xycoords='data',
        xytext=(+10, +30), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.plot([t0,t0],[0,np.cos(t0)], color ='black', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.cos(t0),], 50, color ='black')
    
    plt.annotate(
        r'$\cos(\frac{3\pi}{4})=-\frac{\sqrt{2}}{2}$',
        xy=(t0, np.cos(t0)), xycoords='data',
        xytext=(-90, -50), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.legend(loc='best', frameon=False)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_position(('data',0))
    ax.spines['right'].set_color('none')
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5, label='sine')
    plt.plot(t, cos_t, color='orange', linewidth=2.5, label='cosine')
    
    t0 = 2*np.pi/3
    plt.plot([t0,t0],[0,np.sin(t0)], color ='blue', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.sin(t0),], 50, color ='blue')
    
    plt.annotate(
        r'$\sin(\frac{2\pi}{3})=\frac{\sqrt{3}}{2}$',
        xy=(t0, np.sin(t0)), xycoords='data',
        xytext=(+10, +30), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.plot([t0,t0],[0,np.cos(t0)], color ='orange', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.cos(t0),], 50, color ='orange')
    
    plt.annotate(
        r'$\cos(\frac{2\pi}{3})=-\frac{1}{2}$',
        xy=(t0, np.cos(t0)), xycoords='data',
        xytext=(-90, -50), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.legend(loc='best', frameon=False)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_position(('data',0))
    ax.spines['right'].set_color('none')
    
    plt.show()

## Readability
Some of these tick labels are hard to read, because the lines cross over
them. Try increasing the font size, fading the lines behind them, getting rid
of one of the zeroes, and aligning the remaining zero to avoid the other axis.
To make the lines draw behind the labels, we change the Z-order of the lines.
It's called that, because it moves objects up and down the Z axis that sticks
up out of the paper, perpendicular to the X and Y axes.

    ### Canvas ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5, label='sine', zorder=2)
    plt.plot(t, cos_t, color='orange', linewidth=2.5, label='cosine', zorder=2)
    
    pi = np.pi
    t0 = 2*pi/3
    plt.plot([t0,t0],[0,np.sin(t0)], color ='blue', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.sin(t0),], 50, color ='blue')
    
    plt.annotate(
        r'$\sin(\frac{2\pi}{3})=\frac{\sqrt{3}}{2}$',
        xy=(t0, np.sin(t0)), xycoords='data',
        xytext=(+10, +30), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.plot([t0,t0],[0,np.cos(t0)], color ='orange', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.cos(t0),], 50, color ='orange')
    
    plt.annotate(
        r'$\cos(\frac{2\pi}{3})=-\frac{1}{2}$',
        xy=(t0, np.cos(t0)), xycoords='data',
        xytext=(-90, -50), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.legend(loc='best', frameon=False)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 0, 1], [r'$-1$', r'$0$', r'$1$'])
    
    ax = plt.gca()
    
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_position(('data',0))
    ax.spines['right'].set_color('none')
    
    labels = ax.xaxis.get_majorticklabels() + ax.yaxis.get_majorticklabels()
    labels[2].set_horizontalalignment('center')
    for label in labels:
        label.set_fontsize(10)
        label.set_bbox(dict(facecolor='none', edgecolor='none', alpha=0.5))
    
    plt.show()
    ### Goal ###
    import numpy as np
    import matplotlib.pyplot as plt
    
    t = np.linspace(-np.pi, np.pi, 256, endpoint=True)
    sin_t = np.sin(t)
    cos_t = np.cos(t)
    
    plt.plot(t, sin_t, color='blue', linewidth=2.5, label='sine', zorder=1)
    plt.plot(t, cos_t, color='orange', linewidth=2.5, label='cosine', zorder=1)
    
    pi = np.pi
    t0 = 2*pi/3
    plt.plot([t0,t0],[0,np.sin(t0)], color ='blue', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.sin(t0),], 50, color ='blue')
    
    plt.annotate(
        r'$\sin(\frac{2\pi}{3})=\frac{\sqrt{3}}{2}$',
        xy=(t0, np.sin(t0)), xycoords='data',
        xytext=(+10, +30), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.plot([t0,t0],[0,np.cos(t0)], color ='orange', linewidth=1.5, linestyle="--")
    plt.scatter([t0,],[np.cos(t0),], 50, color ='orange')
    
    plt.annotate(
        r'$\cos(\frac{2\pi}{3})=-\frac{1}{2}$',
        xy=(t0, np.cos(t0)), xycoords='data',
        xytext=(-90, -50), textcoords='offset points', fontsize=16,
        arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))
    
    plt.legend(loc='best', frameon=False)
    
    pi = np.pi
    plt.xticks(
        [-pi, -pi/2, 0, pi/2, pi],
        [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$'])
    plt.yticks([-1, 1], [r'$-1$', r'$1$'])
    
    ax = plt.gca()
    
    ax.spines['bottom'].set_position(('data',0))
    ax.spines['top'].set_color('none')
    ax.spines['left'].set_position(('data',0))
    ax.spines['right'].set_color('none')
    
    labels = ax.xaxis.get_majorticklabels() + ax.yaxis.get_majorticklabels()
    labels[2].set_horizontalalignment('right')
    for label in labels:
        label.set_fontsize(16)
        label.set_bbox(dict(facecolor='white', edgecolor='none', alpha=0.5))
    
    plt.show()

Now you've seen some basic features of matplotlib, you might find the
[matplotlib tutorials] a good place to learn more. You could also try [Seaborn]
to add more plot types and styles. This tutorial was inspired by the work of
[Nicolas P. Rougier].

[matplotlib tutorials]: https://matplotlib.org/stable/tutorials/index
[Seaborn]: https://seaborn.pydata.org/tutorial.html
[Nicolas P. Rougier]: https://github.com/rougier/matplotlib-tutorial
