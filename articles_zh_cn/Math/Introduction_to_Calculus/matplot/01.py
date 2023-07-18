import numpy as np
import matplotlib.pyplot as plt

n = np.arange(1, 50)
a_n = 1 + 1/n

L = 1

epsilon = 0.1
lower_bound = L - epsilon
upper_bound = L + epsilon

plt.plot(n, a_n, 'b.')
plt.axhspan(lower_bound, upper_bound, facecolor='red',  alpha=0.2)

plt.axhline(y=L, color='black', linestyle='--', label='Limit')
plt.legend()

plt.show()
