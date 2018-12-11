"""
Ticks are now properly placed but their labels are not very
explicit. We could guess that 3.142 is pi but it would be better
to make it explicit. When we set tick values, we can also
provide a corresponding label in the second argument list. Note
that the dollar signs mark the labels as latex to allow for nice
rendering of the labels. The r prefix on a string marks it as
raw so the backslashes are included in the string.
:lesson goal file: goal05.py
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
plt.xticks([-np.pi, -np.pi/2, 0, np.pi/2, np.pi],
           [r'?', r'?', r'?', r'?', r'$\pi$'])

# Set y limits
plt.ylim(c.min()*1.1, c.max()*1.1)

# Set y ticks
plt.yticks([-1, 0, 1])

# Show result on screen
plt.show()
