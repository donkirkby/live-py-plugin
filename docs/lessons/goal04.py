import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

plt.plot(x, c, color="blue", linewidth=2.0, linestyle="-")
plt.plot(x, s, color="red", linewidth=2.0, linestyle="-")
plt.xlim(x.min()*1.1, x.max()*1.1)
plt.xticks([-np.pi, -np.pi/2, 0, np.pi/2, np.pi])
plt.ylim(c.min()*1.1, c.max()*1.1)
plt.yticks([-1, 0, 1])

plt.show()
