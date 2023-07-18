import numpy as np
import matplotlib.pyplot as plt

def f(x):
    return x ** 3

def f_prime(x):
    return 3 * x ** 2

def f_double_prime(x):
    return 6 * x

x = np.linspace(-2, 2, 400)

y = f(x)
y_prime = f_prime(x)
y_double_prime = f_double_prime(x)

plt.figure()

plt.plot(x, y, label="f(x) = x^3", color="blue")
plt.plot(x, y_prime, label="f'(x) = 3x^2", color="red")
plt.plot(x, y_double_prime, label="f''(x) = 6x", color="green")

plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.grid(True)

plt.show()
