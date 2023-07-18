import numpy as np
import matplotlib.pyplot as plt

a = 2
epsilon = 0.3
delta = 0.5

x = np.linspace(0, 5, 100)
y = np.sqrt(x)

plt.plot(x, y, color='blue', label='f(x) = sqrt(x)')
plt.scatter(a, np.sqrt(a), color='red', label='Point (a)')

plt.fill_between(x, np.sqrt(a) - epsilon, np.sqrt(a) + epsilon, color='pink', alpha=0.3, label='ε')
plt.fill_betweenx(np.linspace(0, np.sqrt(a), 100), a - delta, a + delta, color='yellow', alpha=0.3, label='δ')


plt.axvline(x=a - delta, color='green', linestyle='--', label='x = a - δ')
plt.axvline(x=a + delta, color='green', linestyle='--', label='x = a + δ')

plt.xlabel('x')
plt.ylabel('f(x)')
plt.legend()


plt.grid(True)
plt.show()
