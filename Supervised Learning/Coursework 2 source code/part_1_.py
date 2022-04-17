# -*- coding: utf-8 -*-
"""Part 1 .ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1-fL66kLfc2bFhNCF5VI7B6286f7NAGJn
"""

import torch
import numpy as np
import matplotlib.pyplot as plt
import re

from google.colab import drive
drive.mount('/content/drive', force_remount=True)

"""
load all the data, format into form of class tensor (Y) and data tensor (X), (for each of the three sets of data)
"""

mini_train = list(open('/content/drive/MyDrive/supervisedcoursework/minitrain.dat','r'))
mini_train_raw = [[float(x) for x in re.split(' +',i)] for i in mini_train]
mini_train_tensor = torch.tensor(mini_train_raw)
mini_train_Y = mini_train_tensor[:,0]
mini_train_X = mini_train_tensor[:,1:]

mini_test = list(open('/content/drive/MyDrive/supervisedcoursework/minitest.dat','r'))
mini_test_raw = [[float(x) for x in re.split(' +',i)] for i in mini_test]
mini_test_tensor = torch.tensor(mini_test_raw)
mini_test_Y = mini_test_tensor[:,0]
mini_test_X = mini_test_tensor[:,1:]

data = list(open('/content/drive/MyDrive/supervisedcoursework/zipcombo.dat','r'))
data_raw = [[float(x) for x in list(filter(None,re.split(' |\n',i)))] for i in data]
data_tensor = torch.tensor(data_raw)
Y = data_tensor[:,0]
X = data_tensor[:,1:]

print(Y.size())

"""
function to randomly permute the order of the data, and then split the data into random 80% train and 20% test split, 
returns train_x, train_y, test_x, test_y
"""
def permute_split(D):
  permuted = D[torch.randperm(D.size()[0])]
  split_80 = round(0.8*D.size()[0])
  return (permuted[:split_80])[:,1:], (permuted[:split_80])[:,0], (permuted[split_80:])[:,1:], (permuted[split_80:])[:,0]

T = permute_split(data_tensor)
print(T[2].size())

"""
class for polynomial kernel perceptron using method 1 as defined in report
"""
class polynomial_1():
  def __init__(self, d, train_X, train_Y, test_X, test_Y, classes):
    self.d = d
    self.classes = classes
    self.X_train = train_X
    self.Y_train = train_Y
    self.X_test = test_X
    self.Y_test = test_Y
    self.kernel = (self.X_train @ self.X_train.t())**self.d
    self.alpha = torch.zeros(self.classes, self.X_train.size()[0])

  """
  finds kappa (as defined in the report), for the ith training point
  """
  def kappa_train(self,i):
    return self.alpha @ self.kernel[i]
  """
  takes the argmax of kappa, giving a classification of a data point
  """
  def classifier_train(self,i):
    return torch.argmax(self.kappa_train(i)).item()
  """
  checks if the predicted classification is equal to the actual classification,
  and returns a boolean output
  """
  def correct_train(self, i):
    return self.classifier_train(i) == self.Y_train[i].item()
  """
  constructs a vector of {zeros, minus ones and a one}, this is the correction vector with is mulitplied
  elementwise by the kernel vector associated with the given training point
  """   
  def correction_vector(self,i):
    index = int(self.Y_train[i].item())
    k = self.kappa_train(i)
    val = k[index].item()
    k.apply_(lambda x : 0 if x < val else -1)
    k.index_copy_(0,torch.tensor([index]),torch.tensor([1]).float())
    return k

  """
  updates the alpha matrix, using the correction vector and the Kernel vector of the associated point i
  """
  def update(self,i):
    T = (self.kernel[i].unsqueeze(0)).repeat(self.classes,1)
    self.alpha += T*self.correction_vector(i)[:,None]

  """
  performs 1 round of training, by looping over the training data once
  """
  def train(self):
    error = 1
    self.update(0)
    for i in range(1,self.X_train.size()[0]):
      if not self.correct_train(i):
        self.update(i)
        error += 1
    return 100*error/self.X_train.size()[0]
  
  def training_error(self):
    A = self.alpha @ self.kernel
    C = torch.eq(torch.argmax(A, dim=0),self.Y_train)
    return 100*(1-torch.count_nonzero(C)/C.size()[0])
    
  """
  creates the the matrix containing kernel values between test and train data
  """
  def kernel_test_vectorized(self):
    return (self.X_train @ self.X_test.t())**self.d
  
  """
  creates matrix of kappas for the test data
  """
  def kappa_test_vectorized(self):
    return self.alpha @ self.kernel_test_vectorized()
  

  def classifier_test_vectorized(self):
    return torch.argmax(self.kappa_test_vectorized(), dim =0)
  
  def correct_test_vectorized(self):
    return torch.eq(self.classifier_test_vectorized(),self.Y_test)
  
  def test_vectorized(self):
    corrects = self.correct_test_vectorized()
    return 100*(1-torch.count_nonzero(corrects)/corrects.size()[0])

  """
  creates the confusion matrix, where the (ith, jth) elements gives the 
  error rate of mispredicting digit 'i' with digit 'j'
  """
  
  def confusion_matrix(self):
    confusion = torch.zeros((10,10))
    mask = ~self.correct_test_vectorized()
    wrong_predictions = (torch.masked_select(self.classifier_test_vectorized(),mask)).unsqueeze(1)
    wrong_classes = (torch.masked_select(self.Y_test,mask)).unsqueeze(1)
    index_pairs = torch.concat((wrong_classes,wrong_predictions),dim = 1)
    for i in index_pairs:
      confusion[int(i[0].item())][int(i[1].item())] += 1
    return confusion
  
  """
  finds the 5 most difficult to classify images in a random test set
  """
  def hardest(self):
    X_mask = ((~self.correct_test_vectorized()).unsqueeze(1)).repeat(1,256)
    wrongly_predicted = (torch.masked_select(self.X_test,X_mask)).view(-1,256)
    Y_mask = ~self.correct_test_vectorized()
    actual_predictions = (torch.masked_select(self.Y_test,Y_mask)).numpy()

    
    wrong_kernel_matrix = (self.X_train @ wrongly_predicted.t())**self.d
    wrong_kappa_matrix = (self.alpha @ wrong_kernel_matrix)*10**(-20)
    
    norms = torch.diag(torch.norm(wrong_kappa_matrix,dim=0)**-1)
    normalised_kappa = (wrong_kappa_matrix @ norms).numpy()

    def mask(a,b):
      def single_mask(i):
        return a[int(b[i])][i]
      masks = np.vectorize(single_mask)
      I = np.arange(np.shape(b)[0])
      return torch.tensor(masks(I))
    
    indices = torch.topk(mask(normalised_kappa,actual_predictions), 5, largest = False)[1]
    five_hardest = torch.empty((0,256))
    labels = torch.empty(0)
    for i in indices:
      five_hardest = torch.concat((five_hardest,wrongly_predicted[i].unsqueeze(0)), dim = 0)
      labels = torch.concat((labels, torch.tensor(actual_predictions[i]).unsqueeze(0)), dim = 0)

    return five_hardest, labels

train_X, train_Y, test_X, test_Y = permute_split(data_tensor)
C = polynomial_1(4,train_X, train_Y, test_X, test_Y,10)


C.train()
(C.kernel_test_vectorized()).size()

"""
finds the means and stds of test and training error for 
different d for the polynomial kernel using method 1
"""
def means_and_stds():
  errors = torch.zeros((20,14))
  for i in range(7):
    for j in range(20):
      train_X, train_Y, test_X, test_Y = permute_split(data_tensor)
      C = polynomial_1(i+1,train_X, train_Y, test_X, test_Y,10)
      C.train()
      errors[j][2*i] = C.training_error()
      errors[j][2*i + 1] = C.test_vectorized()
  return errors

errors = means_and_stds()

tester = errors
means = torch.mean(tester,dim=0)
stds = torch.std(tester,dim=0)
print(means)
print(stds)

"""
function to split and permute the data into 80 20, but in this case
the data and labels are returned together not separately, (like in the first 
permute/split function)
"""

def perm_split(D):
  permuted = D[torch.randperm(D.size()[0])]
  split_80 = round(0.8*D.size()[0])
  return permuted[:split_80], permuted[split_80:]

"""
function to split the data into 5 equal parts (unpermuted), with the x and y 
components returned separately
"""
def split_5(D):
  split = round(0.2*D.size()[0])
  return (D[:split])[:,1:], (D[:split])[:,0], (D[split:2*split])[:,1:], (D[split:2*split])[:,0], (D[2*split:3*split])[:,1:], (D[2*split:3*split])[:,0], (D[3*split:4*split])[:,1:], (D[3*split:4*split])[:,0], (D[4*split:])[:,1:], (D[4*split:])[:,0]

"""
Overview of the cross_validation() function below

for i in range(20):
  permute
  split to 80  20
  on 80 split: split in to 5 equal parts (no permutation)
  assign 4 to be training sets, the 5th to be test
  for d in 1 to 7:
    for each traning set:
      define model
      train it
      test on 5th set
      record error
    find average error
  d* is one with smallest average error
  train with d* on entire 80
  test on remaning 20
  record d* and the test error

"""

def cross_validation():
  ds_and_errors = torch.zeros((20,2))
  for _ in range(20): 
    training, testing = perm_split(data_tensor)
    k_fold = split_5(training)
    errors = [400]
    for d in range(1,8):
      error = []
      for i in range(4):
        C = polynomial_1(d, k_fold[2*i], k_fold[2*i + 1], k_fold[8], k_fold[9], 10)
        for n in range(30):
          C.train()
        error.append(C.test_vectorized())
      errors.append(sum(error))

    d_star = np.argmin(np.array(errors))
    model = polynomial_1(d_star, training[:,1:], training[:,0], testing[:,1:], testing[:,0], 10)
    model.train()
    ds_and_errors[_][0] = d_star
    ds_and_errors[_][1] = model.test_vectorized()
  return ds_and_errors

table = cross_validation()

mean = torch.mean(table,dim=0)
std = torch.std(table,dim=0)
print(mean)
print(std)

"""
finds the average confusion matrix
"""
def average_confusion():
  confusion_tensor = torch.empty((0,10,10))
  for _ in range(20): 
    training, testing = perm_split(data_tensor)
    k_fold = split_5(training)
    errors = [400]
    for d in range(1,8):
      error = []
      for i in range(4):
        C = polynomial_1(d, k_fold[2*i], k_fold[2*i + 1], k_fold[8], k_fold[9], 10)
        for i in range(30):
          C.train()
        error.append(C.test_vectorized())
      errors.append(sum(error))

    d_star = np.argmin(np.array(errors))
    model = polynomial_1(d_star, training[:,1:], training[:,0], testing[:,1:], testing[:,0], 10)
    model.train()
    confusion_tensor = torch.concat((confusion_tensor, model.confusion_matrix().unsqueeze(0)),dim=0)

    mean_std_matrix = torch.std_mean(confusion_tensor,0)
    
  return mean_std_matrix

confusion_matrix = average_confusion()
print(confusion_matrix)

"""
shows the most difficult to classify images
"""
def hardest_images():
  train_X, train_Y, test_X, test_Y = permute_split(data_tensor)
  C = polynomial_1(4,train_X, train_Y, test_X, test_Y,10)
  for _ in range(30):
    C.train()
  images, labels = C.hardest()
  plt.figure(figsize=(20, 20))
  for n in range(5):
    plt.subplot(10, 10, n+1)
    plt.imshow((images[n].view(16,16)), interpolation="None", cmap='gray')
    plt.axis('off')

  plt.show()
  print(labels)

hardest_images()

"""
we repeat 1 and 2 for the guassian kernel. this is exactly the same procedure as before,
except we replace the kernel matrix
"""

"""
class for gaussian kernel perceptron using method 1 as defined in report
"""
class gaussian():
  def __init__(self, c, train_X, train_Y, test_X, test_Y, classes):
    self.c = c
    self.classes = classes
    self.X_train = train_X
    self.Y_train = train_Y
    self.X_test = test_X
    self.Y_test = test_Y
    self.kernel = torch.exp(-self.c*torch.cdist(self.X_train, self.X_train))
    self.alpha = torch.zeros(self.classes, self.X_train.size()[0])

  def kappa_train(self,i):
    return self.alpha @ self.kernel[i]
  """
  takes the argmax of kappa, giving a classification of a data point
  """
  def classifier_train(self,i):
    return torch.argmax(self.kappa_train(i)).item()
  """
  checks if the predicted classification is equal to the actual classification,
  and returns a boolean output
  """
  def correct_train(self, i):
    return self.classifier_train(i) == self.Y_train[i].item()
  """
  constructs a vector of {zeros, minus ones and a one}, this is the correction vector with is mulitplied
  elementwise by the kernel vector associated with the given training point
  """   
  def correction_vector(self,i):
    index = int(self.Y_train[i].item())
    k = self.kappa_train(i)
    val = k[index].item()
    k.apply_(lambda x : 0 if x < val else -1)
    k.index_copy_(0,torch.tensor([index]),torch.tensor([1]).float())
    return k
  """
  updates the alpha matrix, using the correction vector and the Kernel vector of the associated point i
  """
  def update(self,i):
    T = (self.kernel[i].unsqueeze(0)).repeat(self.classes,1)
    self.alpha += T*self.correction_vector(i)[:,None]
  """
  performs 1 round of training, by looping over the training data once
  """
  def train(self):
    error = 1
    self.update(0)
    for i in range(1,self.X_train.size()[0]):
      if not self.correct_train(i):
        self.update(i)
        error += 1
    return 100*error/self.X_train.size()[0]
  
  def training_error(self):
    A = self.alpha @ self.kernel
    C = torch.eq(torch.argmax(A, dim=0),self.Y_train)
    return 100*(1-torch.count_nonzero(C)/C.size()[0])

  def kernel_test_vectorized(self):
    return torch.exp(-self.c*torch.cdist(self.X_train, self.X_test))
  
  def kappa_test_vectorized(self):
    return self.alpha @ self.kernel_test_vectorized()
  
  def classifier_test_vectorized(self):
    return torch.argmax(self.kappa_test_vectorized(), dim =0)
  
  def correct_test_vectorized(self):
    return torch.eq(self.classifier_test_vectorized(),self.Y_test)
  
  def test_vectorized(self):
    corrects = self.correct_test_vectorized()
    return 100*(1-torch.count_nonzero(corrects)/corrects.size()[0])

train_X1, train_Y1, test_X1, test_Y1 = permute_split(data_tensor)

for i in [0.5, 1, 1.5, 2, 2.5, 3, 3.5]:
  gaussian_C = gaussian(i,train_X1, train_Y1, test_X1, test_Y1,10)
  gaussian_C.train()
  print(gaussian_C.training_error())
  print(gaussian_C.test_vectorized())

def gaussian_means_and_stds():
  errors = torch.zeros((20,14))
  c = 0
  for i in [0.5, 1, 1.5, 2, 2.5, 3, 3.5]:
    for j in range(20):
      train_X, train_Y, test_X, test_Y = permute_split(data_tensor)
      gaussian_C = gaussian(i ,train_X, train_Y, test_X, test_Y,10)
      for _ in range(10):
        gaussian_C.train()
      errors[j][2*c] = gaussian_C.training_error()
      errors[j][2*c + 1] = gaussian_C.test_vectorized()
    c+=1
  return errors

gaussian_errors = gaussian_means_and_stds()
gaussian_tester = gaussian_errors
gaussian_means = torch.mean(gaussian_tester,dim=0)
gaussian_stds = torch.std(gaussian_tester,dim=0)
print(gaussian_means)
print(gaussian_stds)

def gaussian_cross_validation():
  cs_and_errors = torch.zeros((20,2))
  for _ in range(20): 
    training, testing = perm_split(data_tensor)
    k_fold = split_5(training)
    errors = [400]
    for c in [0.5, 1, 1.5, 2, 2.5, 3, 3.5]:
      error = []
      for i in range(4):
        C = gaussian(c, k_fold[2*i], k_fold[2*i + 1], k_fold[8], k_fold[9], 10)
        for n in range(10):
          C.train()
        error.append(C.test_vectorized())
      errors.append(sum(error))

    c_star = np.argmin(np.array(errors))
    model = gaussian(c_star, training[:,1:], training[:,0], testing[:,1:], testing[:,0], 10)
    model.train()
    cs_and_errors[_][0] = c_star
    cs_and_errors[_][1] = model.test_vectorized()
  return cs_and_errors

gaussian_table = gaussian_cross_validation()
print(gaussian_table)
mean = torch.mean(gaussian_table,dim=0)
std = torch.std(gaussian_table,dim=0)
print(mean)
print(std)

"""
reshapes the Y data as described in the report for method 2
"""
def recast_Y(Y):
  def expand(y):
    minuses = -np.ones(10)
    minuses[int(y)] = 1
    return minuses

  recast = np.vectorize(expand, signature='()->(n)')
  return torch.tensor(recast(Y))

"""
class for polynomial kernel perceptron using method 2 as defined in report
"""
class polynomial_2():
  def __init__(self, d, train_X, train_Y, expanded_train_Y, test_X, test_Y, classes):
    self.d = d
    self.classes = classes
    self.X_train = train_X
    self.Y_train = train_Y
    self.expanded = expanded_train_Y.t()
    self.X_test = test_X
    self.Y_test = test_Y
    self.kernel = (self.X_train @ self.X_train.t())**self.d
    self.alpha = torch.zeros(self.classes, self.X_train.size()[0])

  def kappa_train(self,i):
    return torch.sign((self.alpha * self.expanded) @ self.kernel[i].double())
  
  """
  returns a vectors of booleans, specifying if each of the 10 classifiers
  have correctly classified
  """
  def classifier_vector(self,i):
    return torch.eq(self.kappa_train(i),self.expanded.t()[i])

  """
  checks if the predicted classification is equal to the actual classification,
  and returns a boolean output
  """
  def correct_train(self, i):
    return self.classifier_train(i) == self.Y_train[i].item()
  """
  constructs a vector of {zeros, minus ones and a one}, this is the correction vector with is mulitplied
  elementwise by the kernel vector associated with the given training point
  """   
  def correction_vector(self,i):
    classifiers = ~torch.eq(self.kappa_train(i),self.expanded.t()[i])
    correction = classifiers.long()
    return correction.float()
  """
  updates the alpha matrix, using the correction vector of point i
  """
  def update(self,i):
    j = torch.arange(10).long()
    idx = torch.LongTensor([i,i,i,i,i,i,i,i,i,i])
    self.alpha[j,idx] += self.correction_vector(i)
  """
  performs 1 round of training, by looping over the training data once
  """
  def train(self):
    for i in range(self.X_train.size()[0]):
      self.update(i)
  
  """
  returns a random index of kappa whose element is 1, giving a predicted
  classification of the data point x_i
  """
  def classifier(self,i):
    a = i
    a[a<0] = 0
    indices = (torch.nonzero(a))
    array = np.array([b[0] for b in indices])
    if array.size == 0:
      return np.random.choice(np.array([0,1,2,3,4,5,6,7,8,9]))
    else:
      return np.random.choice(array)
    

  def training_error(self):
    classes = []
    A = torch.sign((self.alpha * self.expanded) @ self.kernel.double())
    for i in range(self.X_train.size()[0]):
      classes.append(self.classifier(A[:,i]))
    C = torch.eq(torch.tensor(classes),self.Y_train)
    return 100*(1-torch.count_nonzero(C)/C.size()[0])
    

  def kernel_test_vectorized(self):
    return (self.X_train @ self.X_test.t())**self.d
  
  def kappa_test_vectorized(self):
    return torch.sign((self.alpha * self.expanded) @ self.kernel_test_vectorized().double())

  def test_error(self):
    classes = []
    A = torch.sign(self.kappa_test_vectorized())
    for i in range(self.X_test.size()[0]):
      classes.append(self.classifier(A[:,i]))
    C = torch.eq(torch.tensor(classes),self.Y_test)
    return 100*(1-torch.count_nonzero(C)/C.size()[0])
  
  def classifier_test_vectorized(self):
    return torch.argmax(self.kappa_test_vectorized(), dim =0)
  
  def correct_test_vectorized(self):
    return torch.eq(self.classifier_test_vectorized(),self.Y_test)
  
  def test_vectorized(self):
    corrects = self.correct_test_vectorized()
    return 100*(1-torch.count_nonzero(corrects)/corrects.size()[0])

train_X, train_Y, test_X, test_Y = permute_split(data_tensor)
expanded = recast_Y(train_Y)
D = polynomial_2(3,train_X, train_Y, expanded, test_X, test_Y,10)
D.train()
print(D.training_error())
print(D.test_error())

a = torch.tensor([0,0,3,-2,4,30,-5])
indices = torch.nonzero(a)
array = np.array([i[0] for i in indices])
print(np.random.choice(array))

def method2_means_and_stds():
  errors = torch.zeros((20,14))
  for i in range(7):
    for j in range(10):
      train_X, train_Y, test_X, test_Y = permute_split(data_tensor)
      expanded = recast_Y(train_Y)
      C = polynomial_2(i+1,train_X, train_Y, expanded, test_X, test_Y,10)
      C.train()
      errors[j][2*i] = C.training_error()
      errors[j][2*i + 1] = C.test_error()
  return errors

method2_errors = method2_means_and_stds()
means = torch.mean(method2_errors,dim=0)
stds = torch.std(method2_errors,dim=0)
print(means)
print(stds)

def method2_cross_validation():
  ds_and_errors = torch.zeros((20,2))
  for _ in range(20): 
    training, testing = perm_split(data_tensor)
    k_fold = split_5(training)
    errors = [400]
    for d in range(1,8):
      error = []
      for i in range(4):
        C = polynomial_2(d, k_fold[2*i], k_fold[2*i + 1], recast_Y(k_fold[2*i + 1]), k_fold[8], k_fold[9], 10)
        for n in range(10):
          C.train()
        error.append(C.test_error())
      errors.append(sum(error))

    d_star = np.argmin(np.array(errors))
    model = polynomial_2(d_star, training[:,1:], training[:,0], recast_Y(training[:,0]), testing[:,1:], testing[:,0], 10)
    model.train()
    ds_and_errors[_][0] = d_star
    ds_and_errors[_][1] = model.test_error()
  return ds_and_errors

method2_table = method2_cross_validation()
mean = torch.mean(method2_table,dim=0)
std = torch.std(method2_table,dim=0)
print(mean)
print(std)