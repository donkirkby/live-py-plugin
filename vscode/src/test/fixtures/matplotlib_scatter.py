import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

n = 256
X = np.random.normal(0, 1, n)
Y = np.random.normal(0, 1, n)
angles = np.arctan2(Y, X)

plt.scatter(X, Y, c=angles, cmap='hsv', alpha=0.5, s=75)
plt.xlim([-1.5, 1.5])
plt.ylim([-1.5, 1.5])
plt.tight_layout()
plt.show()
