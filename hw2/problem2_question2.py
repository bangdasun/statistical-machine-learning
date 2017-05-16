# HW 2 - 2
import matplotlib.pyplot as plt
import numpy as np
import random

x1 = np.linspace(-3, 1, num = 100)
x2_1 = 2 + np.sqrt(4 - (x1 + 1)**2)
x2_2 = 2 - np.sqrt(4 - (x1 + 1)**2)

plt.plot(x1, x2_1, 'k-')
plt.plot(x1, x2_2, 'k-')
plt.xlim(-5, 3)
plt.ylim(-1, 5)
plt.xlabel('$X_{1}$')
plt.ylabel('$X_{2}$')

for i in range(100000):
    x = random.uniform(-5, 3)
    y = random.uniform(-1, 5)
    if ((x + 1)**2 + (y - 2)**2 <= 4):
        plt.plot(x, y, 'r.')
    else:
        plt.plot(x, y, 'b.')
plt.show()
