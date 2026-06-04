---
title: Matplotlib Multiplot Tutorial
layout: react
is_react: True
hero_image: ../../images/matplotlib/multiplot.png
image: /images/matplotlib/multiplot.png
modules: matplotlib
---
The challenge of this tutorial is to create and arrange subplots in Matplotlib. 
If you are new to Matplotlib, start with the [Matplotlib introduction] first. If 
you already know the basics, revise the challenge plot code below to match the 
goal plot: add a subplot, configure the grid, remove the axes ticks, and adjust 
the position layout. You can experiment on your own, or read the rest of the 
tutorial to learn the concepts you need. The progress bar shows how close you 
are to the goal, and the canvas differences (below right) highlight the 
differences in red (too light) or blue (too dark).

If you really mess up the script, you can click the reset button to go back to
the start.

[Matplotlib introduction]: intro.md

## Table of Contents
* [The Challenge]
* [Subplot Grid]
* [Adding Subplots]
* [Spanning Rows and Columns]
* [x and y Ticks]
* [Customizing Layout]

[The Challenge]: #the-challenge
[Subplot Grid]: #subplot-grid
[Adding Subplots]: #adding-subplots
[Spanning Rows and Columns]: #spanning-rows-and-columns
[x and y Ticks]: #x-and-y-ticks
[Customizing Layout]: #customizing-layout

## The Challenge
    ### Canvas ###
    import matplotlib.pyplot as plt
    
    plt.subplot(2,2,1)
    plt.subplot(2,2,3)
    plt.subplot(2,2,4)
    
    plt.show()
    ### Goal ###
    import matplotlib.pyplot as plt

    plt.subplots_adjust(bottom=0.025, left=0.025, top=0.975, right=0.975)

    plt.subplot(2,1,1)
    plt.xticks([]), plt.yticks([])
    
    plt.subplot(2,3,4)
    plt.xticks([]), plt.yticks([])
    
    plt.subplot(2,3,5)
    plt.xticks([]), plt.yticks([])
    
    plt.subplot(2,3,6)
    plt.xticks([]), plt.yticks([])

    plt.show()

## Subplot Grid
The `subplot` function takes three arguments: `nrows`, `ncols`, and `index`. Set
`nrows` and `ncols` to adjust the grid size in a multi-plot figure. The `index` 
starts at `1` in the upper-left and increases to the right. This example shows the 
position of each index in a three-by-three grid. Note that each subplot uses
`nrows=3`, `ncols=3`, and the `index` determines the subplot location.

    ### Canvas ###
    import matplotlib.pyplot as plt

    plt.suptitle('3 x 3 Grid')

    axes = []
    for index in range(1, 10):

        # Note: Only the value of the index argument
        # changes as each of the 9 subplots are created
        sub = plt.subplot(3, 3, index)

        label = f"index: {index}"
        sub.annotate(label, xy=(0.5, 0.5), xycoords='axes fraction', va='center', ha='center')
        axes.append(sub)

    for ax in axes:
        ax.set_xticks([])
        ax.set_yticks([])

    plt.show()

## Adding Subplots
Can you use `subplot(nrows, ncols, index)` to change this 2 x 2 grid into a 2 x 3 grid?

    ### Canvas ###
    import matplotlib.pyplot as plt

    plt.subplot(2, 2, 1)
    plt.subplot(2, 2, 2)
    plt.subplot(2, 2, 3)
    plt.subplot(2, 2, 4)

    plt.show()
    ### Goal ###
    import matplotlib.pyplot as plt

    plt.subplot(2, 3, 1)
    plt.subplot(2, 3, 2)
    plt.subplot(2, 3, 3)
    plt.subplot(2, 3, 4)
    plt.subplot(2, 3, 5)
    plt.subplot(2, 3, 6)

    plt.show()

## Spanning Rows and Columns
A `subplot` can also span multiple indices. To span more than one `index` on the grid, 
pass the `index` parameter as a tuple `(start-index, end-index)`. For instance, 
`subplot(2, 2, (1,2)` will span the first two indices of a 2 x 2 grid.

Change the `index` parameter on each of these 4 subplots to match the goal.

    ### Canvas ###
    import matplotlib.pyplot as plt

    plt.subplot(3, 3, 1)
    plt.subplot(3, 3, 2)
    plt.subplot(3, 3, 3)
    plt.subplot(3, 3, 4)

    plt.show()
    ### Goal ###
    import matplotlib.pyplot as plt

    plt.subplot(3, 3, (1, 7))
    plt.subplot(3, 3, (2, 6))
    plt.subplot(3, 3, 8)
    plt.subplot(3, 3, 9)

    plt.show()

## x and y Ticks
By default, a `subplot` displays tick marks on the `x` and `y` axes that span the data range. 
If no data is present, ticks span from 0.0 to 1.0. Change ticks by using the `xticks()` and 
`yticks()` functions.

To update tick labels, each function takes a `ticks` parameter, a list for label positions, 
and a `labels` parameter, a list of the corresponding labels: 
`xticks(ticks='position_list', labels='label_list')`. 

**Note**: If you are not saving the subplot to a variable to update later, update ticks 
before generating the next `subplot`. See [xticks documentation] for modifying tick positions 
and labels.

Try using `xticks()` and `yticks()` to change ticks on these two subplots. What kind of list
will remove all ticks?

[xticks documentation]: https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.xticks.html

    ### Canvas ###
    import matplotlib.pyplot as plt

    # First subplot
    plt.subplot(1, 2, 1)
    x, y = [0, 1, 2, 3, 4], [1.5, 4, 3.5, 8, 6.8]
    plt.plot(x, y)
    plt.xticks([0, 1, 2, 3, 4], [0, 1, 2, 3, 4])

    # Second subplot
    plt.subplot(1, 2, 2)

    plt.show()
    ### Goal ###
    import matplotlib.pyplot as plt

    plt.subplot(1, 2, 1)
    plt.plot([0, 1, 2, 3, 4], [1.5, 4, 3.5, 8, 6.8])
    plt.xticks([0, 1, 2, 3, 4], ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'])
    plt.yticks([2, 4, 6, 8])

    plt.subplot(1, 2, 2)
    plt.xticks([]), plt.yticks([])

    plt.show()
    
## Customizing Layout
To adjust the layout boundaries, use the `subplots_adjust()` function, which
takes up to six parameters:
* `left` and `right` position the subplots as a fraction of the canvas _width_
* `bottom` and `top` position the subplots as a fraction of the canvas _height_
* `wspace` adjusts the _width_ padding between subplots
* `hspace` adjusts the _height_ padding between subplots

For a complete overview and visual guide, refer to the [subplots_adjust documentation]. 
Pay close attention to how the `left` and `right` are measured as a fraction of 
the width from the canvas's _left edge_, and the `bottom` and `top` as a fraction 
of the height from the canvas's _bottom edge_. Remember that each setting must 
be a float value between 0.0 and 1.0, inclusive.

Try using `subplots_adjust` to recreate the layout of the Goal plot. A blue
background is provided to make the figure canvas stand out. Adjust the
`subplots_adjust` parameters until your plot matches the Goal.

[subplots_adjust documentation]: https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.subplots_adjust.html

    ### Canvas ###
    import matplotlib.pyplot as plt

    fig = plt.figure()
    fig.set_facecolor('lightblue')

    plt.subplot(2, 2, 1)
    plt.xticks([]), plt.yticks([])

    plt.subplot(2, 2, 2)
    plt.xticks([]), plt.yticks([])

    plt.subplot(2, 2, (3, 4))
    plt.xticks([]), plt.yticks([])

    plt.subplots_adjust(
        left=None,
        right=None,
        bottom=None,
        top=None,
        hspace=None,
        wspace=None
    )

    plt.live_coding_zoom()
    plt.show()
    ### Goal ###
    import matplotlib.pyplot as plt

    fig = plt.figure()
    plt.live_coding_zoom()
    fig.set_facecolor('lightblue')

    plt.subplot(2, 2, 1)
    plt.xticks([]), plt.yticks([])

    plt.subplot(2, 2, 2)
    plt.xticks([]), plt.yticks([])

    plt.subplot(2, 2, (3, 4))
    plt.xticks([]), plt.yticks([])

    plt.subplots_adjust(
        left=0.40,
        right=0.80,
        bottom=0.10,
        top=0.70,
        hspace=1,
        wspace=0.50
    )

    plt.show()

Now that you have all the skills you need, can you solve [the challenge] at the
start of the tutorial? Here's a summary of the skills you learned:

| Command                 | Explanation                                             |
|-------------------------|---------------------------------------------------------|
| `subplot()`             | Add a subplot to the figure.                            |
| `nrows=`, `ncols=`      | Set the number of rows and columns of the subplot grid. |
| `index=`                | Position the subplot within the grid.                   |
| `xticks()`, `yticks()`  | Set tick locations and labels along the x and y axes.   |
| `subplots_adjust()`     | Adjust the layout of the subplots within the figure.    |
| `right=`, `left=`       | Position subplots edge as a fraction of canvas width.   |
| `top=`, `bottom=`       | Position subplots edge as a fraction of canvas height.  |

For more `subplot` details, read the [documentation]. This tutorial
was inspired by the work of [Nicolas P. Rougier].

[the challenge]: #the-challenge
[documentation]: https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.subplot.html
[Nicolas P. Rougier]: https://github.com/rougier/matplotlib-tutorial#user-content-scatter-plots
