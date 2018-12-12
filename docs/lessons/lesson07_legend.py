"""
Let's add a legend in the upper left corner. This only requires
adding the keyword argument label (that will be used in the
legend box) to the plot commands. There are also options for the
legend command to tell it where to put the legend and how it
looks.
:lesson goal file: goal07.py
"""

# Imports
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

# Plot cosine using blue color with a continuous line of width 2 (pixels)
plt.plot(x, c, color="blue", linewidth=2.0, linestyle="-", label='cos')

# Plot sine using red color with a continuous line of width 2 (pixels)
plt.plot(x, s, color="red", linewidth=2.0, linestyle="-")

plt.legend(loc='upper right', frameon=True)

# Set x limits
plt.xlim(x.min()*1.1, x.max()*1.1)

# Set x ticks
plt.xticks([-np.pi, -np.pi/2, 0, np.pi/2, np.pi],
           [r'$-\pi$', r'$-\pi/2$', r'$0$', r'$+\pi/2$', r'$+\pi$'])

# Set y limits
plt.ylim(c.min()*1.1, c.max()*1.1)

# Set y ticks
plt.yticks([-1, 0, 1])

ax = plt.gca()
ax.spines['right'].set_color('none')
ax.spines['top'].set_color('none')
ax.spines['bottom'].set_position(('data', 0))
ax.xaxis.set_ticks_position('bottom')
ax.spines['left'].set_position(('data', 0))
ax.yaxis.set_ticks_position('left')

# Show result on screen
plt.show()
