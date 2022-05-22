"""Solves Problemset 3 in Python"""
# Ecercise 1:
import numpy as np
from matplotlib import pyplot as plt

def generate_moving_average(q: int, num=200):
    """Generates MA(q) process for num observations"""
    epsilon = np.random.normal(0, 1, num+q)
    return [sum(epsilon[i:i+q]) for i in range(num)]

# Plot different q's in Q
T = 200 # Number of observations
Q = [5, 10, 50] # Different q's for MA(q) process
xs = range(T) # x-values for plotting
vis = ['-', '--', ':', '-.'] # some fancy plotting settings

# Building plot step-by-step
for i, q in enumerate(Q):
    plt.plot(xs, generate_moving_average(q, T),vis[i], label=f'q={q}')
plt.legend()
plt.show() # Prints plot into environment

# Implementation in Jupyter Notebook is more convenient... please refer to uebung3.ipynb