import numpy as np
import matplotlib.pyplot as plt

mu = 0
sigma = 1

x = np.linspace(mu - 3 * sigma, mu + 3 * sigma, 1000)
pdf = (1 / (sigma * np.sqrt(2 * np.pi))) * np.exp(-0.5 * ((x - mu) / sigma) ** 2)

plt.plot(x, pdf, label='Gaussian PDF')
plt.xticks([-3, -2, -1, 0, 1, 2, 3], ['-3σ', '-2σ', '-1σ', '0', '1σ', '2σ', '3σ'])
plt.legend()
plt.ylim(0, plt.ylim()[1])
plt.grid(True)
plt.show()