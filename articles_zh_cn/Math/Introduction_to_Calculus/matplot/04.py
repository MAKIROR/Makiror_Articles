import numpy as np
import matplotlib.pyplot as plt

def f(x):
    return x**2

def g(x):
    return np.sin(x) + 2

def h(x):
    return 3 - x**2
L = 2

x = np.linspace(-2, 2, 100)

y_f = f(x)
y_g = g(x)
y_h = h(x)

plt.figure(figsize=(8, 6))
plt.plot(x, y_f, label='f(x) = x^2')
plt.plot(x, y_g, label='g(x) = sin(x) + 2')
plt.plot(x, y_h, label='h(x) = 3 - x^2')

plt.axhline(y=L, color='black', linestyle='--', label='L')

plt.fill_between(x, y_f, y_h, alpha=0.2, color='gray')

plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.grid(True)
plt.ylim(-1, 4)

plt.show()
