# -*- coding: utf-8 -*-
"""optimization 3

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1r0xsiv-xVoKlQJObdjX_EWbcQcymPUXF
"""

# Commented out IPython magic to ensure Python compatibility.
import numpy as np
from scipy import linalg as scplinalg
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import cm

import random
# %matplotlib inline

def generate_problem(n, d, s, std=0.06):
  # Generate xs
  # vectors with entries in [0.5, 1] and [-1, -0.5]
  # repectively
  assert s % 2 == 0, "s needs to be divisible by 2"
  xsp = 0.5 * (np.random.rand(s // 2) + 1)
  xsn = - 0.5 * (np.random.rand(s // 2) + 1)
  xsparse = np.hstack([xsp, xsn, np.zeros(d - s)])
  random.shuffle(xsparse)
  # Generate A
  A = np.random.randn(n, d)
  # Generate eps
  y = A @ xsparse + std * np.random.randn(n)
  return xsparse, A, y

X, A, Y = generate_problem(1000, 500, 50)
norm_A = np.linalg.norm(A, ord ='fro')

"""
constructs a random sequence of N_iter values, each sampled
uniformly at random from range(n)
"""
def random_sequence(n, N_iter):
  return np.random.choice(n, (N_iter))

"""
calculates the argument for the proximal operator for pgsa
"""

def argument_pgsa(gamma, x, y, a):
  return x - (gamma * ((a @ x)-y) * a)

"""
the soft thresholding operator in 1 dimension, with threshhold =
gamma*lamb
"""
def soft_thresh_1D(x, gamma, lamb):
  a = gamma * lamb
  if abs(x) <= a:
    return 0
  if x > a:
    return x - a
  if x < -a:
    return x + a

"""
the soft thresholding operator for arbitrary number of dimensions.
"""
def soft_thresh_nD(x, gamma, lamb):
  a = gamma * lamb
  x = np.where(abs(x) <= a, 0, x)
  x = np.where(x > a, x-a, x)
  x = np.where(x < -a, x+a, x)
  return x

"""
computes the argument for the soft-thresholding in rcpga
"""
def argument_rcpga(gamma, x, a, c):
  return x - (gamma/1000 * a @ c)

"""
defines the objective function
"""
def objective_function(A, x, y, lamb):
  return (np.linalg.norm(A @ x - y)**2)/2000 + lamb * np.linalg.norm(x,ord=1)

"""
implementation of the PSGA algorithm
"""
def psga_plot(N_iter, lamb, x, y, A, theta):
  B = np.copy(x)
  #initialises gamma
  gamma = theta*1000/norm_A**2

  #creates random sequence of indices
  indices = random_sequence(1000, N_iter)

  #creates list that contains values of the objective function
  values = [objective_function(A, B, y, lamb)]

  for k in range(N_iter):
    #computes argument of soft thresholding operator
    c = argument_pgsa(gamma, B, y[indices[k]], A[indices[k]])

    #performs operation of soft thresholding and updates x
    B = soft_thresh_nD(c, gamma, lamb)

    #updates gamma
    gamma = gamma * np.sqrt(k+1)/np.sqrt(k+2)

    values.append(objective_function(A, B, y, lamb))
  plt.plot(np.arange(N_iter+1), values)
  plt.xlabel('iterations')
  plt.ylabel('objection function value')
  plt.title(f'PSGA, Lambda = {lamb}')
  plt.show()
  return B, values

x_psga, values_1 = psga_plot(1000000, 0.1, X, Y, A, 1)

"""
implementation of RCPGA algorithm
"""
def rcpga_plot(N_iter, lamb, x, y, A):
  B= np.copy(x)
  #computes the vector containing values of gamma
  Gamma = np.reciprocal(np.linalg.norm(A, axis = 0))**2 * 1000

  #creates random sequence of indices
  indices = random_sequence(500, N_iter)

  #creates list that contains values of the objective function
  values = [objective_function(A, B, y, lamb)]

  for j in indices:
    gamma = Gamma[j]
    #computes argument of soft thresholding operator
    c = argument_rcpga(gamma, B[j], A[:,j], A @ B - y)

    #updates x
    B[j] = soft_thresh_1D(c, gamma, lamb)
  
    values.append(objective_function(A, B, y, lamb))
  
  plt.plot(np.arange(N_iter+1), values)
  plt.xlabel('no. iterations')
  plt.ylabel('objection function value')
  plt.title(f'RCPGA, Lambda = {lamb}')
  plt.show()
  return B

x_rcpga = rcpga_plot(100000, 0.1, X, Y, A)

"""
plots the objective function value of the ergodic mean
along with objective function value of the value of x at each iteration
"""
def psga_ergodic_plot(N_iter, lamb, x, y, A, gamma = 1000/norm_A**2):
  B = np.copy(x)

  indices = random_sequence(1000, N_iter)
  values_1 = [objective_function(A, B, y, lamb)]
  values_2 = [objective_function(A, B, y, lamb)]
  X_s = np.array([B])
  Gammas = gamma
  X_gamma = gamma * B

  for k in range(N_iter):
    c = argument_pgsa(gamma, B, y[indices[k]], A[indices[k]])
    B = soft_thresh_nD(c, gamma, lamb)
    gamma = gamma * np.sqrt(k+1)/np.sqrt(k+2)

    Gammas += gamma
    X_gamma += gamma*B
    ergo_mean = np.reciprocal(Gammas) * X_gamma
    values_1.append(objective_function(A, ergo_mean, y, lamb))
    values_2.append(objective_function(A, B, y, lamb))

  plt.plot(np.arange(N_iter+1), values_1, label = 'ergodic means')
  plt.plot(np.arange(N_iter+1), values_2, label = 'non-ergodic')
  plt.legend(loc="upper right")
  plt.xlabel('iterations')
  plt.ylabel('objection function value')
  plt.title(f'PSGA, Lambda = {lamb}')
  plt.show()

psga_ergodic_plot(100000, 0.1, X, Y, A)