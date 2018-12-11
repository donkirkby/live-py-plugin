"""
Current limits of the figure are a bit too tight and we want to
make some space in order to clearly see all data points.
:lesson goal file: goal03.py
"""

# Imports
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

# Plot cosine using blue color with a continuous line of width 2 (pixels)
plt.plot(x, c, color="blue", linewidth=2.0, linestyle="-")

# Plot sine using red color with a continuous line of width 2 (pixels)
plt.plot(x, s, color="red", linewidth=2.0, linestyle="-")

# Set x limits
plt.xlim(-4, 4)  # Hint: add a 10% border.

# Set x ticks
plt.xticks(np.linspace(-3, 3, 7, endpoint=True))

# Set y limits
plt.ylim(-1, 1)  # Hint: add a 10% border.

# Set y ticks
plt.yticks(np.linspace(-1, 1, 5, endpoint=True))

# Show result on screen
plt.show()
