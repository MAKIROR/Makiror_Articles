import numpy as np
import matplotlib.pyplot as plt

v = np.array([2, 3])
u = np.array([3, 1])

plt.figure(figsize=(6, 6))
plt.axhline(0, color='gray', linewidth=0.5)
plt.axvline(0, color='gray', linewidth=0.5)
plt.grid(color='gray', linestyle='--', linewidth=0.5)

plt.quiver(0, 0, v[0], v[1], angles='xy', scale_units='xy', scale=1, color='b', label='Vector v')
plt.quiver(0, 0, u[0], u[1], angles='xy', scale_units='xy', scale=1, color='g', label='Vector u')

plt.xlim(-1, 5)
plt.ylim(-1, 5)

plt.xlabel('x')
plt.ylabel('y')

plt.legend()

plt.show()
