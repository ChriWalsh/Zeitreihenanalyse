"""Solves Uebung 0 in Python"""

import numpy as np

# Create 100 i.i.d. data points (White Noise)
x = np.random.normal(1, 2, 100)

# Define mean function
def mean(x: np.array)->float:
    return sum(x)/len(x)

# Define variance function
def var(x: np.array)->float:
    """Calculates empirical variance or vector x"""
    return sum((x-mean(x))**2)/len(x)

# (Tuple-)Assign solutions to variables
numpy_mean, my_mean = np.mean(x), mean(x)
numpy_variance, my_variance = np.var(x), var(x)

# Print results
print(
    f'My mean is {my_mean}, numpy\'s mean is {numpy_mean} \n \
        My variance is {my_variance}, numpy\'s variance is {numpy_variance}'
)
