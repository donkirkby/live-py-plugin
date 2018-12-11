""" Simple plot

In this section, we want to draw the cosine and sine functions
on the same plot. Starting from the default settings, we'll
enrich the figure step by step to make it nicer.

First step is to get the data for the sine and cosine functions:
:lesson goal file: goal01.py
"""
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

# x is now a numpy array with 256 values ranging from -pi to +pi
# (included). c is the cosine (256 values) and s is the sine
# (256 values).
# To see the plot in PyCharm, first run this file normally.
# That should show the plot in a new window. If it shows up in
# the tool window inside PyCharm, you should probably disable
# the Python Scientific mode under File: Settings.
# Next, choose Run: Start Live Turtle. That should show you two
# plots: the current plot and the goal plot.
# Can you add the sine data to make the first plot match the
# second one?

plt.plot(x, c)  # Copy this line and change it.

# Once they match exactly, the goal plot should disappear.
# Then you can open lesson 2.

plt.show()
