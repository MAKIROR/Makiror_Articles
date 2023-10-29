import numpy as np
import matplotlib.pyplot as plt

probabilities = [0.1, 0.2, 0.15, 0.25, 0.05, 0.1, 0.1, 0.05]

values = [1, 2, 3, 4, 5, 6, 7, 8]

plt.bar(values, probabilities, tick_label=values, align='center', alpha=0.7, width=0.1)
plt.xlabel('x')
plt.grid(True)
plt.show()