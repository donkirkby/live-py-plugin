import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-np.pi, np.pi, 256, endpoint=True)
c, s = np.cos(x), np.sin(x)

plt.plot(x, c, color="blue", linewidth=2.0, linestyle="-")
plt.plot(x, s, color="red", linewidth=2.0, linestyle="-")
plt.xlim(x.min()*1.1, x.max()*1.1)
plt.xticks(np.linspace(-3, 3, 7, endpoint=True))
plt.ylim(c.min()*1.1, c.max()*1.1)
plt.yticks(np.linspace(-1, 1, 5, endpoint=True))

plt.show()
