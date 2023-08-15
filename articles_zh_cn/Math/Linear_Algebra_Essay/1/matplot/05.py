import numpy as np
import matplotlib.pyplot as plt

A = np.array([5, 3])
B = np.array([5, 0])
C = np.array([8, 0])

dot_product = np.dot(A, B)

plt.figure(figsize=(6, 6))
plt.axhline(0, color='gray', linewidth=0.5)
plt.axvline(0, color='gray', linewidth=0.5)
plt.grid(color='gray', linestyle='--', linewidth=0.5)

plt.quiver(0, 0, A[0], A[1], angles='xy', scale_units='xy', scale=1, color='b', label='Vector v')
plt.quiver(0, 0, C[0], C[1], angles='xy', scale_units='xy', scale=1, color='g', label='Vector u')
plt.quiver(0, 0, 0, 3, angles='xy', scale_units='xy', scale=1, color='r', label='proj_v_i u_{i-1}')

plt.plot([5, 5], [0, 3], linestyle='dashed', color='black')

plt.xlim(-1, 10)
plt.ylim(-1, 5)

plt.xlabel('x')
plt.ylabel('y')

plt.legend()

plt.show()