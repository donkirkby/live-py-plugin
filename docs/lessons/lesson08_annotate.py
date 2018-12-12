"""
Let's annotate some interesting points using the annotate
command. We chose the 2pi/3 value and we want to annotate both
the sine and the cosine. We'll first draw a marker on the curve
as well as a straight dotted line. Then, we'll use the annotate
command to display some text with an arrow.
If your dashed lines don't quite match the goal, try drawing
them in the opposite direction.
:lesson goal file: goal08.py
"""

# Imports
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

# Plot cosine using blue color with a continuous line of width 2 (pixels)
plt.plot(x, c, color="blue", linewidth=2.0, linestyle="-", label='cosine')

# Plot sine using red color with a continuous line of width 2 (pixels)
plt.plot(x, s, color="red", linewidth=2.0, linestyle="-", label='sine')

plt.legend(loc='upper left', frameon=False)

t = 2*np.pi/3
plt.plot([t, t/2], [-1, -0.2], color='blue', linewidth=1.5, linestyle="--")
plt.scatter([t/2], [-0.2], 50, color='blue')

plt.annotate(r'$\cos(\frac{2\pi}{3})=-\frac{1}{2}$',
             xy=(t, np.cos(t)), xycoords='data',
             xytext=(-90, -50), textcoords='offset points', fontsize=16,
             arrowprops=dict(arrowstyle="-|>", connectionstyle="arc3,rad=.2"))

plt.plot([t, t], [0, np.sin(t)], color='red', linewidth=1.5, linestyle=":")
plt.scatter([t], [np.sin(t)], 50, color='red')

plt.annotate(r'$\sin(\frac{2\pi}{30})=\frac{\sqrt{3}}{2}$',
             xy=(t, np.sin(t)), xycoords='data',
             xytext=(+10, +30), textcoords='offset points', fontsize=16,
             arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=.2"))

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
