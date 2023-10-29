import numpy as np
import matplotlib.pyplot as plt

def likelihood(theta):
    return theta * theta

theta_values = np.linspace(0, 1, 100)

likelihood_values = [likelihood(theta) for theta in theta_values]

plt.plot(theta_values, likelihood_values, label='Likelihood Function')
plt.xlabel('θ')
plt.ylabel('L(θ | HH)')
plt.legend()
plt.grid(True)

plt.show()
