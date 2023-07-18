import numpy as np
import matplotlib.pyplot as plt

def f(x):
    return np.sin(x) + 2

a = 1
b = 4

x = np.linspace(-1, 6, 100)

y = f(x)

plt.plot(x, y, 'b-', label='f(x)')

plt.fill_between(x, y, where=(x >= a) & (x <= b), color='gray', alpha=0.3)

plt.ylim(0, 6)
plt.xlim(0, 5)

plt.plot([1, 1], [f(1), -1], 'r--', color='black')
plt.text(a, f(a) + 0.2, 'a', ha='right', va='bottom', color='black')
plt.plot([4, 4], [f(4), -1], 'r--', color='black')
plt.text(b, f(b) + 0.2, 'b', ha='left', va='bottom', color='black')

plt.xlabel('x')
plt.ylabel('f(x)')
plt.legend()

plt.show()
