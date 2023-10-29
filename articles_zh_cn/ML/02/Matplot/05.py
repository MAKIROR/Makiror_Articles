import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import binom

n = 10
p = 0.5

x = np.arange(0, n+1)

pmf = binom.pmf(x, n, p)

plt.bar(x, pmf, align='center', alpha=0.5, width=0.1)
plt.xticks(x)
plt.xlabel('k')
plt.ylabel('Probability')
plt.grid(True)
plt.show()