import numpy as np
import matplotlib.pyplot as plt

v = np.array([2, 2])
u = np.array([3, 1])
projection_length = np.dot(v, u) / np.dot(u, u)
projection = projection_length * u

dot_product = np.dot(v, u)

plt.figure(figsize=(6, 6))
plt.axhline(0, color='gray', linewidth=0.5)
plt.axvline(0, color='gray', linewidth=0.5)
plt.grid(color='gray', linestyle='--', linewidth=0.5)

plt.quiver(0, 0, v[0], v[1], angles='xy', scale_units='xy', scale=1, color='b', label='Vector v')
plt.quiver(0, 0, u[0], u[1], angles='xy', scale_units='xy', scale=1, color='g', label='Vector u')

plt.quiver(0, 0, projection[0], projection[1], angles='xy', scale_units='xy', scale=1, color='r', label='proj_v u')

plt.plot([projection[0], 2], [projection[1], 2], 'k--', label='Perpendicular')

plt.xlim(-1, 5)
plt.ylim(-1, 3)

plt.xlabel('x')
plt.ylabel('y')

plt.legend()

plt.show()