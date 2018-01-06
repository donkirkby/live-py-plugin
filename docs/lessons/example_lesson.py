""" Changing line colours

You can change the colour of a line by passing a colour abbreviation in the
format string of the plot() method. Add a format string to these two plot()
calls to make the heights red and the weights green.
:lesson goal file: example_goal.py
"""
from matplotlib import pyplot as plt

heights = [180, 180, 180, 173]
weights = [72, 72, 64, 62]

plt.plot(heights)
plt.plot(weights)
plt.show()
