"""
Current ticks are not ideal because they do not show the
interesting values (+/-pi,+/-pi/2) for sine and cosine. We'll
change them such that they show only these values.
:lesson goal file: goal04.py
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
plt.xlim(x.min()*1.1, x.max()*1.1)

# Set x ticks
plt.xticks(np.linspace(-3, 3, 7, endpoint=True))

# Set y limits
plt.ylim(c.min()*1.1, c.max()*1.1)

# Set y ticks
plt.yticks(np.linspace(-1, 1, 5, endpoint=True))

# Show result on screen
plt.show()
