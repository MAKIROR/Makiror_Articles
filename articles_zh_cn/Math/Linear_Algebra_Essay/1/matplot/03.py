import numpy as np
import matplotlib.pyplot as plt

A = np.array([2, 3])
B = np.array([0, 3])
C = np.array([2, 0])

dot_product = np.dot(A, B)

plt.figure(figsize=(6, 6))
plt.axhline(0, color='gray', linewidth=0.5)
plt.axvline(0, color='gray', linewidth=0.5)
plt.grid(color='gray', linestyle='--', linewidth=0.5)

plt.quiver(0, 0, A[0], A[1], angles='xy', scale_units='xy', scale=1, color='b')
plt.quiver(0, 0, C[0], C[1], angles='xy', scale_units='xy', scale=1, color='g', label='i-hat')
plt.quiver(2, 0, B[0], B[1], angles='xy', scale_units='xy', scale=1, color='r', label='j-hat')

plt.xlim(0, 5)
plt.ylim(0, 5)

plt.xlabel('x')
plt.ylabel('y')

plt.legend()

plt.show()