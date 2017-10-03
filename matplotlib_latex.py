import numpy as np
import matplotlib.pyplot as plt

plt.rc('text', usetex=True)
plt.rc('font', family='serif')

import time
tic=time.time()
plt.text(0,0,r"$\displaystyle\sum_{n=1}^\infty\frac{-e^{i\pi}}{2^n}$!", fontsize=80)
plt.savefig('tex_demo')
plt.show()
print time.time() - tic,'s'
