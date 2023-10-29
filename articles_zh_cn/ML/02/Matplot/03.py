import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

x = np.linspace(-5, 5, 100)

mean = 0
std_dev = 1
pdf = norm.pdf(x, loc=mean, scale=std_dev)

plt.figure(figsize=(8, 6))
plt.plot(x, pdf, label='PDF', color='blue')
plt.xlabel('X')
plt.xticks(np.arange(-5, 6, 1))
plt.yticks([])
plt.grid(False)
plt.legend()
plt.show()
