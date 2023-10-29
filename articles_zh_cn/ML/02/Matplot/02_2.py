import numpy as np
import matplotlib.pyplot as plt

theta_values = np.linspace(0, 1, 100)

likelihood_values = (theta_values ** 3) * (1 - theta_values)
normalized_likelihood_values = likelihood_values / np.max(likelihood_values)

plt.plot(theta_values, normalized_likelihood_values, label='Likelihood Function')
plt.xlabel('θ')
plt.ylabel('L(θ | HHHT)')
plt.legend()
plt.grid(True)

plt.show()