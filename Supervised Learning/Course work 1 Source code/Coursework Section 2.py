#!/usr/bin/env python
# coding: utf-8

# In[24]:


import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from numpy import savetxt


# In[25]:


"""
creates m x-data points byt sampling uniformly over [0,1]^2
"""

def create_xdata(m):
    data = []
    for i in range(m):
        data.append(np.random.uniform(size=(2)))
    return np.array(data)


# In[26]:


"""
creates m y-data points byt sampling uniformly from {0,1}
"""

def create_ydata(m):
    data = []
    for i in range(m):
        data.append(np.random.choice([0,1]))
    return np.array(data) 


# In[27]:


"""
combines the x and y data points to a single data object
"""
def create_data(m):
    X,Y = create_xdata(m),create_ydata(m)
    #return np.array(list(zip(X,Y)))
    return (X,Y)

D = create_data(100)


# In[28]:


"""
returns a list of distances of some test point x to each data point in X
"""
def distances(x,X):
    return np.linalg.norm(X-x,axis=1)


# In[29]:


"""
returns the indices of the k smallest values in an array (the list of distances will be the input to this function)
"""
def k_smallest(X,k):   
    idx = np.argpartition(X,k)
    return idx[:k]


# In[30]:


'''
counts the majority classification of y from the k nearest data points. Takes as input the y-data (Y),
and the list of indices of the smallest k distances (K) Returns the majority class of the k nearest points,
if there is no clear majority it returns a uniform sample from {0,1}
'''
def majority_y(K,Y):
    count = 0
    for i in K:
        count += Y[i]
    count = count/len(K)
    if count < 1/2:
        return 0
    if count > 1/2:
        return 1
    else:                 #corner cases
        return np.random.choice([0,1])


# In[31]:


'''
Takes as input some meshgrid domain on R^2 (Psi,Chi), the generated data (Data), and the set parameter k for
the number of nearest neighbours. vectorizes a function that classifies a single point 
returns the predicted classification over some meshgrid domain
'''
def vec_classifier(Psi,Chi,Data,k):
    def class_test_point(x,y):
        dist = distances(np.array([x,y]),Data[0])
        nearest = k_smallest(dist,k)
        return majority_y(nearest,Data[1])
    vectorized = np.vectorize(class_test_point)
    return vectorized(Psi,Chi)


# In[9]:


'''
creates the meshgrid domain
'''

x = np.linspace(0,1,400)
y = np.linspace(0,1,400)
X,Y = np.meshgrid(x,y)


# In[10]:


'''
defines the classification of the above domain
'''
Z = vec_classifier(X,Y,D,3)


# In[32]:


cmap_light = ListedColormap(['#FFAAAA', '#AAFFAA',])
cmap_bold = ListedColormap(['#FF0000', '#00FF00'])


# In[12]:


'''
plots a classification diagram with each colour corresponding to an area that would classified either 1 or -1
'''
plt.pcolormesh(X, Y, Z, cmap=cmap_light)
plt.scatter(D[0][:,0],D[0][:,1],c=D[1],cmap=cmap_bold)
plt.title(r'A hypothesis $h_{S,v}$ visualized with |S| = 100 and v = 3')
plt.savefig('heat_map.png')
plt.show()


# In[33]:


'''
Takes in a meshgrid domain, the sample data and the value of k specifying the number of nearest neighbours
defines a different classification protocol whereby the flip of a bias coin decides whether 
to return a random classification or one from k-NN. Returns a the classification of all inout points
'''
def vec_coinflip_classifier(Psi,Chi,Data,k):  
    def class_coinflip_point(x,y,k):
        if np.random.binomial(1,0.8) == 0:
            return np.random.choice([0,1])
        else:
            dist = distances(np.array([x,y]),Data[0])
            nearest = k_smallest(dist,k)
            return majority_y(nearest,Data[1])

    vectorized = np.vectorize(class_coinflip_point)
    return vectorized(Psi,Chi,k)


# In[34]:


'''
returns percentage error (for 1000 test points)
takes as input the test and training data, (and k)
computes the prediction of the test points based on the training points
computes the error by comparing the number of mistakes with the test y-data
'''
def test_error(x,y,t_x,t_y,k):
    predicted_y = vec_coinflip_classifier(x[:,0],x[:,1],(t_x,t_y),k)
    error = np.count_nonzero(predicted_y - y)/10
    return error


# In[36]:


'''
puts all the functions above together to create sample data, create training data,
create test data, and predict the classification of the test data using training data,
then find the error of the predictions, returns that error
'''
def run_constant_v(k):
    Sample_data = create_data(100)
    training_x = create_xdata(4000)
    training_y = vec_coinflip_classifier(training_x[:,0],training_x[:,1],Sample_data,3)
    test_x = create_xdata(1000)
    test_y = vec_coinflip_classifier(test_x[:,0],test_x[:,1],Sample_data,3)
    percentage_error = test_error(test_x,test_y,training_x,training_y,k)
    return percentage_error


# In[17]:


'''
finds the errors averaged over 100 runs, for each k from 1 to 49
'''
errors_3 = []
for k in range(1,50):
    error_3 = 0
    for i in range(100):
        error_3 += run_constant_v(k)
    errors_3 += [error_3/100]

total_errors_3 = np.array(errors_3)    
np.savetxt('error_data.csv',total_errors_3)


# In[21]:


plt.scatter(np.arange(1,50,1),total_errors_3)
plt.xlabel('k')
plt.ylabel('percentage generalisation error')
plt.title('k against generalisation error')
plt.savefig('k2_error.png')
plt.show()


# In[38]:


#creating the list of m-values to iterate over
M = [100]
for i in range(1,9):
    M.append(500*i)


# In[39]:


'''
is exactly the same as run_constant_v except that the number of training data points can be specified
'''
def run_2_constant_v(k,m):
    Sample_data = create_data(100)
    training_x = create_xdata(m)
    training_y = vec_coinflip_classifier(training_x[:,0],training_x[:,1],Sample_data,3)
    test_x = create_xdata(1000)
    test_y = vec_coinflip_classifier(test_x[:,0],test_x[:,1],Sample_data,3)
    percentage_error = test_error(test_x,test_y,training_x,training_y,k)
    return percentage_error


# In[40]:


'''
iterates over different numbers of training points from to find the optimal k (value of k 
that minimizes generalisation error chosen from 0<k<50) averaged over 100 runs
'''
optimal_K_3 = []
for m in M:
    sum_optimal_ks_3 = 0
    for i in range(100):
        errors_3 = []
        for k in range(1,50):
            errors_3.append(run_2_constant_v(k,m))
        sum_optimal_ks_3 += min(range(len(errors_3)), key=errors_3.__getitem__)
    optimal_K_3.append(sum_optimal_ks_3/100)

optimal_k_values_3 = np.array(optimal_K_3)
np.savetxt('optimal_ks_3.csv',optimal_k_values_3)


# In[41]:


plt.scatter(np.array(M), optimal_k_values_3)
plt.xlabel('m')
plt.ylabel('optimal k')
plt.title('Number of training points against optimal k')
plt.savefig('optimal_k_m3.png')
plt.show()
