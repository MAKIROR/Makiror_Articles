import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(0, 5, 100)
y = 2*x

plt.plot(x, y, color='blue', label='f(x) = 2x')

x1, x2 = 1,2.5
y1, y2 = 2,5

plt.plot([x1, x2], [y1, y2], marker='o', color='red')
plt.plot([x2, x2], [y1, y2], 'r--', color='orange')
plt.plot([x1, x2], [y1, y1], 'r--', color='orange')

plt.xlabel('x')
plt.ylabel('f(x)')
plt.legend()
plt.ylim(0, 6)
plt.xlim(0, 5)

plt.grid(True)
plt.show()
