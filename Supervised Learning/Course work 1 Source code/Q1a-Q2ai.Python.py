#Q1-a

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

x = [1,2,3,4]
y = [3,2,0,5]
x0 = np.arange(0, 5,0.1)
x1 = np.arange(0,5)
#k=1
y1 = [2.5 + 0*i for i in x1]
#k=2
y2 =[ 0.4*i + 1.5 for i in x0]
#k=3
y3 =[9 - 7.1*i + (i**2)*1.5 for i in x0]
#k=4
y4 =[-5 + 15.1*i - (i**2)*8.5 + (i**3)*1.33 for i in x0]

plt.scatter(x,y)
plt.plot(y1)
plt.plot(x0, y2)
plt.plot(x0, y3)
plt.plot(x0, y4)




#Q2-ai

#Question a-i)
noise = np.random.normal(0,0.07,30)
xi = np.linspace(start=0, stop=1, num=30)
y = np.square(np.sin(2*np.pi*xi)) + noise 
print(xi)
print(y)
#sine function
x = np.linspace(start=0, stop=1, num=300)
ys = np.square(np.sin(2*np.pi*x))
#print(ys)
plt.figure()
plt.scatter(xi,y)   
plt.plot(x,ys)   
plt.show()
