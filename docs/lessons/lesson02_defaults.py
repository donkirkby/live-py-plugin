"""
In this lesson, we've instantiated (and commented) figure
settings that influence the appearance of the plot. The
settings have been explicitly set similar to their default
values, so try playing with the values to see their effect.
Try to solve the lesson by making the lines thicker and changing
the color.
:lesson goal file: goal02.py
"""

# Imports
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

# Plot cosine using blue color with a continuous line of width 1.5 (pixels)
plt.plot(x, c, color="blue", linewidth=1.5, linestyle="-")

# Plot sine using orange color with a continuous line of width 1.5 (pixels)
plt.plot(x, s, color="orange", linewidth=1.5, linestyle="-")

# Set x limits
plt.xlim(-4, 4)

# Set x ticks
plt.xticks(np.linspace(-3, 3, 7, endpoint=True))

# Set y limits
plt.ylim(-1, 1)

# Set y ticks
plt.yticks(np.linspace(-1, 1, 5, endpoint=True))

# Show result on screen
plt.show()
