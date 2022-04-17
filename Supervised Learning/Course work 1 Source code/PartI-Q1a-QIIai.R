#QUESTION 1
#k=1
test.data <- read.table("y.dat.txt",header=TRUE)
df <- data.frame(x=c(1,2,3,4), y=c(3,2,0,5))
model1 <- lm(y ~ 1, data=df)
plot(model)
summary(model)
mean(model$residuals^2)
predict(model1)
#MSE
msek1 <- data.frame(pred = predict(model1), actual = df$y)
msek1
MSE1 <- mean((msek1$actual - msek1$pred)^2)
MSE1
#3.25


#k=2
df <- data.frame(x=c(1,2,3,4), y=c(3,2,0,5))
model2 <- lm(y ~ 1 + x , data=df)
summary(model2)
predict(model2)
#MSE
msek2 <- data.frame(pred = predict(model2), actual = df$y)
msek2
MSE2 <- mean((msek2$actual - msek2$pred)^2)
MSE2
#3.05

#k=3
df <- data.frame(x=c(1,2,3,4), y=c(3,2,0,5))
model3 <- lm(y ~ 1 + x + I(x^2), data=df)
summary(model3)
predict(model3)
#MSE
msek3 <- data.frame(pred = predict(model3), actual = df$y)
msek3
MSE3 <- mean((msek3$actual - msek3$pred)^2)
MSE3
#0.8

#k=4
df <- data.frame(x=c(1,2,3,4), y=c(3,2,0,5))
model4 <- lm(y ~ 1 + x + I(x^2) + I(x^3), data=df)
summary(model4)
predict(model4)
#MSE
msek4 <- data.frame(pred = predict(model4), actual = df$y)
msek4
MSE4 <- mean((msek4$actual - msek4$pred)^2)
MSE4
#5.605843e-29





#-------------------------------------------------------------------------------








##QUESTION 2



##Q2




#2-a-ii)
#data
noise <- rnorm(30, 0, 0.07)
xi <- seq(0, 1, by = 1/29)
g <- (sin(2*pi*xi))^2 + noise
plot(xi, g)

#fit with k = 2, 5, 10, 14, 18
#k=2,degree=1
df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
df2
model2 <- lm(y ~ 1 + x , data=df2)
print(y)
pred_y2 <- predict(model2, list(x=xi))
print(pred_y2)

#k=5, degree=4
df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
df5
model5 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) , data=df5)
print(y)
pred_y5 <- predict(model5, list(x=xi, I(x^2), I(x^3), I(x^4)))
print(pred_y5)

#k=10, degree=9
df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
df10
model10 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data=df10)
print(y)
pred_y10 <- predict(model10, list(x=xi, I(x^2), I(x^3), I(x^4), I(x^5), I(x^6), I(x^7), I(x^8), I(x^9)))
print(pred_y10)


#k=14, degree=13
df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
df14
model14 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13), data=df14)
print(y)
pred_y14 <- predict(model14, list(x=xi, I(x^2), I(x^3), I(x^4), I(x^5), I(x^6), I(x^7), I(x^8), I(x^9), I(x^10), I(x^11), I(x^12), I(x^13)))
print(pred_y14)

#k=18, degree=17
df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
df18
model18 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17), data=df18)
print(y)
pred_y18 <- predict(model14, list(x=xi, I(x^2), I(x^3), I(x^4), I(x^5), I(x^6), I(x^7), I(x^8), I(x^9), I(x^10), I(x^11), I(x^12), I(x^13), I(x^14), I(x^15), I(x^16), I(x^17)))
print(pred_y18)


#plots
x <- xi
plot(x, g, pch=16, cex=0.5)
lines(x, pred_y2, col="orange")
lines(x, pred_y5, col="red")
lines(x, pred_y10, col="green")
lines(x, pred_y14, col="blue")
lines(x, pred_y18, col="purple")
legend("topright",                                    
       legend=c("k=2","k=5","k=10","k=14","k=18"),   
       col=c("orange","red","green","blue","purple"),
       lty=1,lwd=1,
       cex=0.45,
       horiz=TRUE) 





#-------------------------------------------------------------------------------







#2b
##To set up our data first, then fit in to models with different degrees





set.seed(56)
#data
noise <- rnorm(30, 0, 0.07)
xi <- seq(0, 1, by = 1/29)
g <- (sin(2*pi*xi))^2 + noise

#k=1,degree=0
df1 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model1 <- lm(y ~ 1 , data=df1)


#k=2,degree=1
df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model2 <- lm(y ~ 1 + x , data=df2)


#k=3,degree=2
df3 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model3 <- lm(y ~ 1 + x + I(x^2) , data=df3)



#k=4, degree=3
df4 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model4 <- lm(y ~ 1 + x + I(x^2) + I(x^3) , data=df4)


#k=5, degree=4
df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model5 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) , data=df5)


#k=6, degree=5
df6 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model6 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data=df6)


#k=7, degree=6
df7 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model7 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data=df7)


#k=8, degree=7
df8 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model8 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7), data=df8)


#k=9, degree=8
df9 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model9 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data=df9)


#k=10, degree=9
df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model10 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data=df10)


#k=11, degree=10
df11 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model11 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=df11)
mean(model1$residuals^2)

#k=12, degree=11
df12 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model12 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11), data=df12)


#k=13, degree=12
df13 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model13 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12), data=df13)


#k=14, degree=13
df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model14 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13), data=df14)


#k=15, degree=14
df15 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model15 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14), data=df15)


#k=16, degree=15
df16 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model16 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15), data=df16)


#k=17, degree=16
df17 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model17 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16), data=df17)


#k=18, degree=17
df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model18 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17), data=df18)


#MSE 
predict(model1)
mse1 <- data.frame(pred = predict(model1), actual = df1$y)
mse1
MSE1 <- mean((mse1$actual - mse1$pred)^2)
MSE1 

predict(model2)
mse2 <- data.frame(pred = predict(model2), actual = df2$y)
mse2
MSE2 <- mean((mse2$actual - mse2$pred)^2)
MSE2 

predict(model3)
mse3 <- data.frame(pred = predict(model3), actual = df3$y)
mse3
MSE3 <- mean((mse3$actual - mse3$pred)^2)
MSE3 

predict(model4)
mse4 <- data.frame(pred = predict(model4), actual = df4$y)
mse4
MSE4 <- mean((mse4$actual - mse4$pred)^2)
MSE4 

predict(model5)
mse5 <- data.frame(pred = predict(model5), actual = df5$y)
mse5
MSE5 <- mean((mse5$actual - mse5$pred)^2)
MSE5 

predict(model6)
mse6 <- data.frame(pred = predict(model6), actual = df6$y)
mse6
MSE6 <- mean((mse6$actual - mse6$pred)^2)
MSE6

predict(model7)
mse7 <- data.frame(pred = predict(model7), actual = df7$y)
mse7
MSE7 <- mean((mse7$actual - mse7$pred)^2)
MSE7

predict(model8)
mse8 <- data.frame(pred = predict(model8), actual = df8$y)
mse8
MSE8 <- mean((mse8$actual - mse8$pred)^2)
MSE8

predict(model9)
mse9 <- data.frame(pred = predict(model9), actual = df9$y)
mse9
MSE9 <- mean((mse9$actual - mse9$pred)^2)
MSE9 

predict(model10)
mse10 <- data.frame(pred = predict(model10), actual = df10$y)
mse10
MSE10 <- mean((mse10$actual - mse10$pred)^2)
MSE10 

predict(model11)
mse11 <- data.frame(pred = predict(model11), actual = df11$y)
mse11
MSE11 <- mean((mse11$actual - mse11$pred)^2)
MSE11 

predict(model12)
mse12 <- data.frame(pred = predict(model12), actual = df12$y)
mse12
MSE12 <- mean((mse12$actual - mse12$pred)^2)
MSE12 

predict(model13)
mse13 <- data.frame(pred = predict(model13), actual = df13$y)
mse13
MSE13 <- mean((mse13$actual - mse13$pred)^2)
MSE13 

predict(model14)
mse14 <- data.frame(pred = predict(model14), actual = df14$y)
mse14
MSE14 <- mean((mse14$actual - mse14$pred)^2)
MSE14 

predict(model15)
mse15 <- data.frame(pred = predict(model15), actual = df15$y)
mse15
MSE15 <- mean((mse15$actual - mse15$pred)^2)
MSE15 

predict(model16)
mse16 <- data.frame(pred = predict(model16), actual = df16$y)
mse16
MSE16 <- mean((mse16$actual - mse16$pred)^2)
MSE16 

predict(model17)
mse17 <- data.frame(pred = predict(model17), actual = df17$y)
mse17
MSE17 <- mean((mse17$actual - mse17$pred)^2)
MSE17 

predict(model18)
mse18 <- data.frame(pred = predict(model18), actual = df18$y)
mse18
MSE18 <- mean((mse18$actual - mse18$pred)^2)
MSE18 

#transform to ln + plot
te <- c(MSE1, MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, MSE12, MSE13, MSE14, MSE15, MSE16, MSE17, MSE18)
print(te)
log(te)
degree <- c(1:18)
print(degree)
plot(degree, log(te),'l', main="training error vs the polynomial dimension")




#-------------------------------------------------------------------------------





#2c
##


#data
set.seed(23)
noise <- rnorm(30, 0, 0.07)
xti <- runif(30, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
train <- data.frame(y = g,
                    x = xti)


noise <- rnorm(1000, 0, 0.07)
xti <- runif(1000, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
test <- data.frame(y = g,
                   x = xti)


#k=1,degree=0

model1 <- lm(y ~ 1 , data=train)

#k=2,degree=1
model2 <- lm(y ~ 1 + x , data=train)

#k=3,degree=2
model3 <- lm(y ~ 1 + x + I(x^2) , data=train)

#k=4, degree=3
model4 <- lm(y ~ 1 + x + I(x^2) + I(x^3) , data=train)

#k=5, degree=4
model5 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) , data=train)

#k=6, degree=5
model6 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data=train)

#k=7, degree=6
model7 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data=train)

#k=8, degree=7
model8 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7), data=train)

#k=9, degree=8
model9 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data=train)

#k=10, degree=9
model10 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data=train)


#k=11, degree=10
model11 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=train)


#k=12, degree=11
model12 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11), data=train)


#k=13, degree=12
model13 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12), data=train)


#k=14, degree=13
model14 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13), data=train)


#k=15, degree=14
model15 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14), data=train)


#k=16, degree=15
model16 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15), data=train)


#k=17, degree=16
model17 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16), data=train)


#k=18, degree=17
model18 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17), data=train)

#MSE test
predict(model1)
mse1 <- data.frame(pred = predict(model1, test), actual = test$y)
mse1
MSE1 <- mean((mse1$actual - mse1$pred)^2)
MSE1 #0.1294895

predict(model2)
mse2 <- data.frame(pred = predict(model2, test), actual = test$y)
mse2
MSE2 <- mean((mse2$actual - mse2$pred)^2)
MSE2 #0.1321938

predict(model3)
mse3 <- data.frame(pred = predict(model3,test), actual = test$y)
mse3
MSE3 <- mean((mse3$actual - mse3$pred)^2)
MSE3 #0.1231873

predict(model4)
mse4 <- data.frame(pred = predict(model4,test), actual = test$y)
mse4
MSE4 <- mean((mse4$actual - mse4$pred)^2)
MSE4 #0.122615

predict(model5)
mse5 <- data.frame(pred = predict(model5,test), actual = test$y)
mse5
MSE5 <- mean((mse5$actual - mse5$pred)^2)
MSE5 

predict(model6)
mse6 <- data.frame(pred = predict(model6,test), actual = test$y)
mse6
MSE6 <- mean((mse6$actual - mse6$pred)^2)
MSE6 

predict(model7)
mse7 <- data.frame(pred = predict(model7,test), actual = test$y)
mse7
MSE7 <- mean((mse7$actual - mse7$pred)^2)
MSE7

predict(model8)
mse8 <- data.frame(pred = predict(model8,test), actual = test$y)
mse8
MSE8 <- mean((mse8$actual - mse8$pred)^2)
MSE8 

predict(model9)
mse9 <- data.frame(pred = predict(model9,test), actual = test$y)
mse9
MSE9 <- mean((mse9$actual - mse9$pred)^2)
MSE9

predict(model10)
mse10 <- data.frame(pred = predict(model10,test), actual = test$y)
mse10
MSE10 <- mean((mse10$actual - mse10$pred)^2)
MSE10 

predict(model11)
mse11 <- data.frame(pred = predict(model11,test), actual = test$y)
mse11
MSE11 <- mean((mse11$actual - mse11$pred)^2)
MSE11  

predict(model12)
mse12 <- data.frame(pred = predict(model12,test), actual = test$y)
mse12
MSE12 <- mean((mse12$actual - mse12$pred)^2)
MSE12 

predict(model13)
mse13 <- data.frame(pred = predict(model13,test), actual =test$y)
mse13
MSE13 <- mean((mse13$actual - mse13$pred)^2)
MSE13 

predict(model14)
mse14 <- data.frame(pred = predict(model14,test), actual = test$y)
mse14
MSE14 <- mean((mse14$actual - mse14$pred)^2)
MSE14 

predict(model15)
mse15 <- data.frame(pred = predict(model15,test), actual = test$y)
mse15
MSE15 <- mean((mse15$actual - mse15$pred)^2)
MSE15 

predict(model16)
mse16 <- data.frame(pred = predict(model16,test), actual = test$y)
mse16
MSE16 <- mean((mse16$actual - mse16$pred)^2)
MSE16 

predict(model17)
mse17 <- data.frame(pred = predict(model17,test), actual = test$y)
mse17
MSE17 <- mean((mse17$actual - mse17$pred)^2)
MSE17 

predict(model18)
mse18 <- data.frame(pred = predict(model18,test), actual = test$y)
mse18
MSE18 <- mean((mse18$actual - mse18$pred)^2)
MSE18 


#transform + plot
tse <- c(MSE1, MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, MSE12, MSE13, MSE14, MSE15, MSE16, MSE17, MSE18)
print(tse)
log(tse)
degree <- c(1:18)
print(degree)
plot(degree, log(tse),'l', main="test error vs the polynomial dimension")



#-------------------------------------------------------------------------------






#2d





###2d-(b)

#construct data again then fit into different models
noise <- rnorm(30, 0, 0.07)
xi <- seq(0, 1, by = 1/29)
g <- (sin(2*pi*xi))^2 + noise

#k=1,degree=0
df1 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model1 <- lm(y ~ 1 , data=df1)
#define function
k1 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df1 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 , data=df1))
}
k1(30)
#replication
replicate(100, k1(30), simplify = "array")
smodel1 <- replicate(100, k1(30), simplify = "array")
#transformation
mse1 <- rep(0,100)
for (i in 1:100){
  mse1[i] <- mean(unlist(smodel1[2,i])^2)
}
mse1

#k=2,degree=1
df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model2 <- lm(y ~ 1 + x , data=df2)
#define function
k2 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x , data=df2))
}
k2(30)
#replication
replicate(100, k2(30), simplify = "array")
smodel2 <- replicate(100, k2(30), simplify = "array")
#transformation
mse2 <- rep(0,100)
for (i in 1:100){
  mse2[i] <- mean(unlist(smodel2[2,i])^2)
}
mse2

#k=3,degree=2
df3 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model3 <- lm(y ~ 1 + x + I(x^2) , data=df3)
#define function
k3 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xti <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df3 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xti))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) , data=df3))
}
k3(30)
#replication
replicate(100, k3(30), simplify = "array")
smodel3 <- replicate(100, k3(30), simplify = "array")
#transformation
mse3 <- rep(0,100)
for (i in 1:100){
  mse3[i] <- mean(unlist(smodel3[2,i])^2)
}
mse3

#k=4, degree=3
df4 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model4 <- lm(y ~ 1 + x + I(x^2) + I(x^3) , data=df4)
#define function
k4 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df4 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) , data=df4))
}
k4(30)
#replication
replicate(100, k4(30), simplify = "array")
smodel4 <- replicate(100, k4(30), simplify = "array")
#transformation
mse4 <- rep(0,100)
for (i in 1:100){
  mse4[i] <- mean(unlist(smodel4[2,i])^2)
}
mse4

#k=5, degree=4
df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model5 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) , data=df5)
#define function
k5 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) , data=df5))
}
k5(30)
#replication
replicate(100, k5(30), simplify = "array")
smodel5 <- replicate(100, k5(30), simplify = "array")
#transformation
mse5 <- rep(0,100)
for (i in 1:100){
  mse5[i] <- mean(unlist(smodel5[2,i])^2)
}
mse5

#k=6, degree=5
df6 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model6 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data=df6)
#define function
k6 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df6 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) , data=df6))
}
k6(30)
#replication
replicate(100, k6(30), simplify = "array")
smodel6 <- replicate(100, k6(30), simplify = "array")
#transformation
mse6 <- rep(0,100)
for (i in 1:100){
  mse6[i] <- mean(unlist(smodel6[2,i])^2)
}
mse6

#k=7, degree=6
df7 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model7 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data=df7)
#define function
k7 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xti <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df7 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) , data=df7))
}
k7(30)
#replication
replicate(100, k7(30), simplify = "array")
smodel7 <- replicate(100, k7(30), simplify = "array")
#transformation
mse7 <- rep(0,100)
for (i in 1:100){
  mse7[i] <- mean(unlist(smodel7[2,i])^2)
}
mse7

#k=8, degree=7
df8 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model8 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7), data=df8)
#define function
k8 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df8 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) , data=df8))
}
k8(30)
#replication
replicate(100, k8(30), simplify = "array")
smodel8 <- replicate(100, k8(30), simplify = "array")
#transformation
mse8 <- rep(0,100)
for (i in 1:100){
  mse8[i] <- mean(unlist(smodel8[2,i])^2)
}
mse8

#k=9, degree=8
df9 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model9 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data=df9)
#define function
k9 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df9 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) , data=df9))
}
k9(30)
#replication
replicate(100, k9(30), simplify = "array")
smodel9 <- replicate(100, k9(30), simplify = "array")
#transformation
mse9 <- rep(0,100)
for (i in 1:100){
  mse9[i] <- mean(unlist(smodel9[2,i])^2)
}
mse9

#k=10, degree=9
df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model10 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data=df10)
#define function
k10 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data=df10))
}
k10(30)
#replication
replicate(100, k10(30), simplify = "array")
smodel10 <- replicate(100, k10(30), simplify = "array")
#transformation
mse10 <- rep(0,100)
for (i in 1:100){
  mse10[i] <- mean(unlist(smodel10[2,i])^2)
}
mse10

#k=11, degree=10
df11 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model11 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=df11)
#define function
k11 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df11 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=df11))
}
k11(30)
#replication
replicate(100, k11(30), simplify = "array")
smodel11 <- replicate(100, k11(30), simplify = "array")
#transformation
mse11 <- rep(0,100)
for (i in 1:100){
  mse11[i] <- mean(unlist(smodel11[2,i])^2)
}
mse11

#k=12, degree=11
df12 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model12 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11), data=df12)
#define function
k12 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df12 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11), data=df12))
}
k12(30)
#replication
replicate(100, k12(30), simplify = "array")
smodel12 <- replicate(100, k12(30), simplify = "array")
#transformation
mse12 <- rep(0,100)
for (i in 1:100){
  mse12[i] <- mean(unlist(smodel12[2,i])^2)
}
mse12

#k=13, degree=12
df13 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model13 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12), data=df13)
#define function
k13 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df13 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12), data=df13))
}
k13(30)
#replication
replicate(100, k13(30), simplify = "array")
smodel13 <- replicate(100, k13(30), simplify = "array")
#transformation
mse13 <- rep(0,100)
for (i in 1:100){
  mse13[i] <- mean(unlist(smodel13[2,i])^2)
}
mse13

#k=14, degree=13
df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model14 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13), data=df14)
#define function
k14 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13), data=df14))
}
k14(30)
#replication
replicate(100, k14(30), simplify = "array")
smodel14 <- replicate(100, k14(30), simplify = "array")
#transformation
mse14 <- rep(0,100)
for (i in 1:100){
  mse14[i] <- mean(unlist(smodel14[2,i])^2)
}
mse14

#k=15, degree=14
df15 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model15 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14), data=df15)
#define function
k15 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df15 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14), data=df15))
}
k15(30)
#replication
replicate(100, k15(30), simplify = "array")
smodel15 <- replicate(100, k15(30), simplify = "array")
#transformation
mse15 <- rep(0,100)
for (i in 1:100){
  mse15[i] <- mean(unlist(smodel15[2,i])^2)
}
mse15

#k=16, degree=15
df16 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model16 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15), data=df16)
#define function
k16 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df16 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15), data=df16))
}
k16(30)
#replication
replicate(100, k16(30), simplify = "array")
smodel16 <- replicate(100, k16(30), simplify = "array")
#transformation
mse16 <- rep(0,100)
for (i in 1:100){
  mse16[i] <- mean(unlist(smodel16[2,i])^2)
}
mse16

#k=17, degree=16
df17 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model17 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16), data=df17)
#define function
k17 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df17 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16), data=df17))
}
k17(30)
#replication
replicate(100, k17(30), simplify = "array")
smodel17 <- replicate(100, k17(30), simplify = "array")
#transformation
mse17 <- rep(0,100)
for (i in 1:100){
  mse17[i] <- mean(unlist(smodel17[2,i])^2)
}
mse17

#k=18, degree=17
df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model18 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17), data=df18)
#define function
k18 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17), data=df18))
}
k18(30)
#replication
replicate(100, k18(30), simplify = "array")
smodel18 <- replicate(100, k18(30), simplify = "array")
#transformation
mse18 <- rep(0,100)
for (i in 1:100){
  mse18[i] <- mean(unlist(smodel18[2,i])^2)
}
mse18

#average
avg30mse1 <- sum(mse1)/100 
avg30mse2 <- sum(mse2)/100 
avg30mse3 <- sum(mse3)/100 
avg30mse4 <- sum(mse4)/100
avg30mse5 <- sum(mse5)/100 
avg30mse6 <- sum(mse6)/100
avg30mse7 <- sum(mse7)/100
avg30mse8 <- sum(mse8)/100
avg30mse9 <- sum(mse9)/100
avg30mse10 <- sum(mse10)/100
avg30mse11 <- sum(mse11)/100
avg30mse12 <- sum(mse12)/100
avg30mse13 <- sum(mse13)/100
avg30mse14 <- sum(mse14)/100
avg30mse15 <- sum(mse15)/100
avg30mse16 <- sum(mse16)/100
avg30mse17 <- sum(mse17)/100
avg30mse18 <- sum(mse18)/100

#log
avgte <- c(avg30mse1, avg30mse2, avg30mse3, avg30mse4, avg30mse5, avg30mse6, avg30mse7, avg30mse8, avg30mse9, avg30mse10, avg30mse11, avg30mse12, avg30mse13, avg30mse14, avg30mse15, avg30mse16, avg30mse17, avg30mse18)
print(avgte)
log(avgte)
degree <- c(1:18)
print(degree)
plot(degree, log(avgte),'l')

#-------------------------------------------------------------------------------






#2d-(c)






#data
set.seed(23)
noise <- rnorm(30, 0, 0.07)
xti <- runif(30, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
train <- data.frame(y = g,
                    x = xti)

noise <- rnorm(1000, 0, 0.07)
xti <- runif(1000, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
test <- data.frame(y = g,
                   x = xti)


#k=1
model1 <- lm(y ~ 1 , data=train)

#Replication
k1 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model1, test))
}
k1(1000)
#replication
replicate(100, k1(1000), simplify = "array")
smodel1 <- replicate(100, k1(1000), simplify = "array")
#transformation
mse1 <- rep(0,100)
for (i in 1:100){
  mse1[i] <- mean(unlist(smodel1[2,i])^2)
}
mse1

#k=2
model2 <- lm(y ~ 1 + x , data=train)
#define function
k2 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model2, test))
}
k2(1000)

replicate(100, k2(1000), simplify = "array")
smodel2 <- replicate(100, k2(1000), simplify = "array")

mse2 <- rep(0,100)
for (i in 1:100){
  mse2[i] <- mean(unlist(smodel2[2,i])^2)
}
mse2


#k=3,degree=2
model3 <- lm(y ~ 1 + x + I(x^2) , data=train)

k3 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model3, test))
}
k3(1000)
replicate(100, k3(1000), simplify = "array")
smodel3 <- replicate(100, k3(1000), simplify = "array")
#transformation
mse3 <- rep(0,100)
for (i in 1:100){
  mse3[i] <- mean(unlist(smodel3[2,i])^2)
}
mse3

#k=4, degree=3
model4 <- lm(y ~ 1 + x + I(x^2) + I(x^3) , data=train)

k4 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model4, test))
}
k4(1000)

replicate(100, k4(1000), simplify = "array")
smodel4 <- replicate(100, k4(1000), simplify = "array")

mse4 <- rep(0,100)
for (i in 1:100){
  mse4[i] <- mean(unlist(smodel4[2,i])^2)
}
mse4

#k=5, degree=4
model5 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) , data=train)

k5 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model5, test))
}
k5(1000)

replicate(100, k5(1000), simplify = "array")
smodel5 <- replicate(100, k5(1000), simplify = "array")

mse5 <- rep(0,100)
for (i in 1:100){
  mse5[i] <- mean(unlist(smodel5[2,i])^2)
}
mse5

#k=6, degree=5
model6 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data=train)

k6 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model6, test))
}
k6(1000)

replicate(100, k6(1000), simplify = "array")
smodel6 <- replicate(100, k6(1000), simplify = "array")

mse6 <- rep(0,100)
for (i in 1:100){
  mse6[i] <- mean(unlist(smodel6[2,i])^2)
}
mse6

#k=7, degree=6
model7 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data=train)

k7 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model7, test))
}
k7(1000)

replicate(100, k7(1000), simplify = "array")
smodel7 <- replicate(100, k7(1000), simplify = "array")

mse7 <- rep(0,100)
for (i in 1:100){
  mse7[i] <- mean(unlist(smodel7[2,i])^2)
}
mse7

#k=8
model8 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7), data=train)

k8 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model8, test))
}
k8(1000)

replicate(100, k8(1000), simplify = "array")
smodel8 <- replicate(100, k8(1000), simplify = "array")

mse8 <- rep(0,100)
for (i in 1:100){
  mse8[i] <- mean(unlist(smodel8[2,i])^2)
}
mse8

#k=9, 
model9 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), data=train)

k9 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model9, test))
}

k9(1000)

replicate(100, k9(1000), simplify = "array")
smodel9 <- replicate(100, k9(1000), simplify = "array")

mse9 <- rep(0,100)
for (i in 1:100){
  mse9[i] <- mean(unlist(smodel9[2,i])^2)
}
mse9

#k=10
model10 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data=train)

k10 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model10, test))
}
k10(1000)

replicate(100, k10(1000), simplify = "array")
smodel10 <- replicate(100, k10(1000), simplify = "array")

mse10 <- rep(0,100)
for (i in 1:100){
  mse10[i] <- mean(unlist(smodel10[2,i])^2)
}
mse10

#k=11
model11 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=train)

k11 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model11, test))
}
k11(1000)

replicate(100, k11(1000), simplify = "array")
smodel11 <- replicate(100, k11(1000), simplify = "array")

mse11 <- rep(0,100)
for (i in 1:100){
  mse11[i] <- mean(unlist(smodel11[2,i])^2)
}
mse11

#k=12
model12 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11), data=train)

k12 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model12, test))
}
k12(1000)

replicate(100, k12(1000), simplify = "array")
smodel12 <- replicate(100, k12(1000), simplify = "array")

mse12 <- rep(0,100)
for (i in 1:100){
  mse12[i] <- mean(unlist(smodel12[2,i])^2)
}
mse12

#k=13
model13 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12), data=train)

k13 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model13, test))
}
k13(1000)

replicate(100, k13(1000), simplify = "array")
smodel13 <- replicate(100, k13(1000), simplify = "array")

mse13 <- rep(0,100)
for (i in 1:100){
  mse13[i] <- mean(unlist(smodel13[2,i])^2)
}
mse13

#k=14
model14 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13), data=train)

k14 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model14, test))
}
k14(1000)

replicate(100, k14(1000), simplify = "array")
smodel14 <- replicate(100, k14(1000), simplify = "array")

mse14 <- rep(0,100)
for (i in 1:100){
  mse14[i] <- mean(unlist(smodel14[2,i])^2)
}
mse14

#k=15
model15 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14), data=train)

k15 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model15, test))
}
k15(1000)

replicate(100, k15(1000), simplify = "array")
smodel15 <- replicate(100, k15(1000), simplify = "array")

mse15 <- rep(0,100)
for (i in 1:100){
  mse15[i] <- mean(unlist(smodel15[2,i])^2)
}
mse15

#k=16 
model16 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15), data=train)

k16 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model16, test))
}
k16(1000)

replicate(100, k16(1000), simplify = "array")
smodel16 <- replicate(100, k16(1000), simplify = "array")

mse16 <- rep(0,100)
for (i in 1:100){
  mse16[i] <- mean(unlist(smodel16[2,i])^2)
}
mse16

#k=17
model17 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16), data=train)

k17 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model17, test))
}
k17(1000)

replicate(100, k17(1000), simplify = "array")
smodel17 <- replicate(100, k17(1000), simplify = "array")

mse17 <- rep(0,100)
for (i in 1:100){
  mse17[i] <- mean(unlist(smodel17[2,i])^2)
}
mse17

#k=18 
model18 <- lm(y ~ 1 + x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10) + I(x^11) + I(x^12) + I(x^13) + I(x^14) + I(x^15) + I(x^16) + I(x^17), data=train)

k18 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model18, test))
}
k18(1000)

replicate(100, k18(1000), simplify = "array")
smodel18 <- replicate(100, k18(1000), simplify = "array")

mse18 <- rep(0,100)
for (i in 1:100){
  mse18[i] <- mean(unlist(smodel18[2,i])^2)
}
mse18

#average
avgmse1 <- sum(mse1)/100 
avgmse2 <- sum(mse2)/100 
avgmse3 <- sum(mse3)/100 
avgmse4 <- sum(mse4)/100
avgmse5 <- sum(mse5)/100 
avgmse6 <- sum(mse6)/100
avgmse7 <- sum(mse7)/100
avgmse8 <- sum(mse8)/100
avgmse9 <- sum(mse9)/100
avgmse10 <- sum(mse10)/100
avgmse11 <- sum(mse11)/100
avgmse12 <- sum(mse12)/100
avgmse13 <- sum(mse13)/100
avgmse14 <- sum(mse14)/100
avgmse15 <- sum(mse15)/100
avgmse16 <- sum(mse16)/100
avgmse17 <- sum(mse17)/100
avgmse18 <- sum(mse18)/100

#log
avgtse <- c(avgmse1, avgmse2, avgmse3, avgmse4, avgmse5, avgmse6, avgmse7, avgmse8, avgmse9, avgmse10, avgmse11, avgmse12, avgmse13, avgmse14, avgmse15, avgmse16, avgmse17, avgmse18)
print(avgtse)
log(avgtse)
degree <- c(1:18)
print(degree)
plot(degree, log(avgtse),'l')


#-------------------------------------------------------------------------------













###QUESION 3




#3(b)

#data
noise <- rnorm(30, 0, 0.07)
xi <- seq(0, 1, by = 1/29)
g <- (sin(2*pi*xi))^2 + noise


#k=1,degree=0
df1 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model1 <- lm(y ~ sin(pi*x) , data=df1)
mean(model1$residuals^2) 


#k=2,degree=1
df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model2 <- lm(y ~ sin(pi*x)+ sin(2*pi*x) , data=df2)
mean(model2$residuals^2) 

#k=3,degree=2
df3 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model3 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+ sin(3*pi*x), data=df3)
mean(model3$residuals^2)

#k=4, degree=3
df4 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model4 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x) , data=df4)
mean(model4$residuals^2)

#k=5, degree=4
df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model5 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+ sin(5*pi*x) , data=df5)
mean(model5$residuals^2)

#k=6, degree=5
df6 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model6 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+ sin(6*pi*x), data=df6)
mean(model6$residuals^2)

#k=7, degree=6
df7 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model7 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+ sin(7*pi*x), data=df7)
mean(model7$residuals^2)

#k=8, degree=7
df8 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model8 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+ sin(8*pi*x), data=df8)
mean(model8$residuals^2)

#k=9, degree=8
df9 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model9 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+ sin(9*pi*x), data=df9)
mean(model9$residuals^2)

#k=10, degree=9
df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model10 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x), data=df10)
mean(model10$residuals^2)

#k=11, degree=10
df11 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model11 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+ sin(11*pi*x), data=df11)
mean(model1$residuals^2)

#k=12, degree=11
df12 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model12 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+ sin(12*pi*x), data=df12)
mean(model2$residuals^2)

#k=13, degree=12
df13 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model13 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+ sin(13*pi*x), data=df13)
mean(model3$residuals^2)

#k=14, degree=13
df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model14 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+ sin(14*pi*x), data=df14)
mean(model4$residuals^2)

#k=15, degree=14
df15 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model15 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+ sin(15*pi*x), data=df15)
mean(model15$residuals^2)

#k=16, degree=15
df16 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model16 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x), data=df16)
mean(model16$residuals^2)

#k=17, degree=16
df17 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model17 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x), data=df17)
mean(model17$residuals^2)

#k=18, degree=17
df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model18 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x)+sin(18*pi*x), data=df18)
mean(model18$residuals^2)

#transform + plot
te_sin <- c(mean(model1$residuals^2), mean(model2$residuals^2), mean(model3$residuals^2), mean(model4$residuals^2), mean(model5$residuals^2), mean(model6$residuals^2), mean(model7$residuals^2), mean(model8$residuals^2), mean(model9$residuals^2), mean(model10$residuals^2), mean(model11$residuals^2), mean(model12$residuals^2), mean(model13$residuals^2), mean(model14$residuals^2), mean(model15$residuals^2), mean(model16$residuals^2), mean(model17$residuals^2), mean(model18$residuals^2))
print(te_sin)
log(te_sin)
degree <- c(1:18)
print(degree)
plot(degree, log(te_sin),'l')


#-------------------------------------------------------------------------------




#3(c)testing error



#data
set.seed(23)
noise <- rnorm(30, 0, 0.07)
xti <- runif(30, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
train <- data.frame(y = g,
                    x = xti)

noise <- rnorm(1000, 0, 0.07)
xti <- runif(1000, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
test <- data.frame(y = g,
                   x = xti)

#fitting with sin basis
#k=1,degree=0

model1 <- lm(y ~ 1 , data=train)

#k=2,degree=1
model2 <- lm(y ~ 1 + sin(pi*x) , data=train)

#k=3,degree=2
model3 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x) , data=train)

#k=4, degree=3
model4 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x) , data=train)

#k=5, degree=4
model5 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x) , data=train)

#k=6, degree=5
model6 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x), data=train)

#k=7, degree=6
model7 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x), data=train)

#k=8, degree=7
model8 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x), data=train)

#k=9, degree=8
model9 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x), data=train)

#k=10, degree=9
model10 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x), data=train)


#k=11, degree=10
model11 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x), data=train)


#k=12, degree=11
model12 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x), data=train)


#k=13, degree=12
model13 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x), data=train)


#k=14, degree=13
model14 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x), data=train)


#k=15, degree=14
model15 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x), data=train)


#k=16, degree=15
model16 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x), data=train)


#k=17, degree=16
model17 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x), data=train)


#k=18, degree=17
model18 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x), data=train)

#MSE test
predict(model1)
mse1 <- data.frame(pred = predict(model1, test), actual = test$y)
mse1
MSE1 <- mean((mse1$actual - mse1$pred)^2)
MSE1 

predict(model2)
mse2 <- data.frame(pred = predict(model2, test), actual = test$y)
mse2
MSE2 <- mean((mse2$actual - mse2$pred)^2)
MSE2 

predict(model3)
mse3 <- data.frame(pred = predict(model3,test), actual = test$y)
mse3
MSE3 <- mean((mse3$actual - mse3$pred)^2)
MSE3

predict(model4)
mse4 <- data.frame(pred = predict(model4,test), actual = test$y)
mse4
MSE4 <- mean((mse4$actual - mse4$pred)^2)
MSE4 

predict(model5)
mse5 <- data.frame(pred = predict(model5,test), actual = test$y)
mse5
MSE5 <- mean((mse5$actual - mse5$pred)^2)
MSE5 

predict(model6)
mse6 <- data.frame(pred = predict(model6,test), actual = test$y)
mse6
MSE6 <- mean((mse6$actual - mse6$pred)^2)
MSE6 

predict(model7)
mse7 <- data.frame(pred = predict(model7,test), actual = test$y)
mse7
MSE7 <- mean((mse7$actual - mse7$pred)^2)
MSE7

predict(model8)
mse8 <- data.frame(pred = predict(model8,test), actual = test$y)
mse8
MSE8 <- mean((mse8$actual - mse8$pred)^2)
MSE8 

predict(model9)
mse9 <- data.frame(pred = predict(model9,test), actual = test$y)
mse9
MSE9 <- mean((mse9$actual - mse9$pred)^2)
MSE9

predict(model10)
mse10 <- data.frame(pred = predict(model10,test), actual = test$y)
mse10
MSE10 <- mean((mse10$actual - mse10$pred)^2)
MSE10 

predict(model11)
mse11 <- data.frame(pred = predict(model11,test), actual = test$y)
mse11
MSE11 <- mean((mse11$actual - mse11$pred)^2)
MSE11  

predict(model12)
mse12 <- data.frame(pred = predict(model12,test), actual = test$y)
mse12
MSE12 <- mean((mse12$actual - mse12$pred)^2)
MSE12 

predict(model13)
mse13 <- data.frame(pred = predict(model13,test), actual =test$y)
mse13
MSE13 <- mean((mse13$actual - mse13$pred)^2)
MSE13 

predict(model14)
mse14 <- data.frame(pred = predict(model14,test), actual = test$y)
mse14
MSE14 <- mean((mse14$actual - mse14$pred)^2)
MSE14 

predict(model15)
mse15 <- data.frame(pred = predict(model15,test), actual = test$y)
mse15
MSE15 <- mean((mse15$actual - mse15$pred)^2)
MSE15 

predict(model16)
mse16 <- data.frame(pred = predict(model16,test), actual = test$y)
mse16
MSE16 <- mean((mse16$actual - mse16$pred)^2)
MSE16 

predict(model17)
mse17 <- data.frame(pred = predict(model17,test), actual = test$y)
mse17
MSE17 <- mean((mse17$actual - mse17$pred)^2)
MSE17 

predict(model18)
mse18 <- data.frame(pred = predict(model18,test), actual = test$y)
mse18
MSE18 <- mean((mse18$actual - mse18$pred)^2)
MSE18 


#transform + plot
tse_sin <- c(MSE1, MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10, MSE11, MSE12, MSE13, MSE14, MSE15, MSE16, MSE17, MSE18)
print(tse_sin)
log(tse_sin)
degree <- c(1:18)
print(degree)
plot(degree, log(tse_sin),'l', main="test error vs the polynomial dimension")


#-------------------------------------------------------------------------------






#3d-b(with 30 points)
#Fit the model first, then replicate for 100 times


#data
noise <- rnorm(30, 0, 0.07)
xi <- seq(0, 1, by = 1/29)
g <- (sin(2*pi*xi))^2 + noise


#k=1,degree=0
df1 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model1 <- lm(y ~ sin(pi*x) , data=df1)
#define function
k1 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df1 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x) , data=df1))
}
k1(30)
#replication
replicate(100, k1(30), simplify = "array")
smodel1 <- replicate(100, k1(30), simplify = "array")
#transformation
mse1 <- rep(0,100)
for (i in 1:100){
  mse1[i] <- mean(unlist(smodel1[2,i])^2)
}
mse1

#k=2,degree=1
df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model2 <- lm(y ~ sin(pi*x)+ sin(2*pi*x) , data=df2)
#define function
k2 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df2 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+ sin(2*pi*x) , data=df2))
}
k2(30)
#replication
replicate(100, k2(30), simplify = "array")
smodel2 <- replicate(100, k2(30), simplify = "array")
#transformation
mse2 <- rep(0,100)
for (i in 1:100){
  mse2[i] <- mean(unlist(smodel2[2,i])^2)
}
mse2

#k=3,degree=2
df3 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model3 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+ sin(3*pi*x), data=df3)
#define function
k3 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xti <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df3 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xti))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+ sin(3*pi*x) , data=df3))
}
k3(30)
#replication
replicate(100, k3(30), simplify = "array")
smodel3 <- replicate(100, k3(30), simplify = "array")
#transformation
mse3 <- rep(0,100)
for (i in 1:100){
  mse3[i] <- mean(unlist(smodel3[2,i])^2)
}
mse3

#k=4, degree=3
df4 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model4 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x), data=df4)
#define function
k4 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df4 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x), data=df4))
}
k4(30)
#replication
replicate(100, k4(30), simplify = "array")
smodel4 <- replicate(100, k4(30), simplify = "array")
#transformation
mse4 <- rep(0,100)
for (i in 1:100){
  mse4[i] <- mean(unlist(smodel4[2,i])^2)
}
mse4

#k=5, degree=4
df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model5 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+ sin(5*pi*x) , data=df5)
#define function
k5 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df5 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+ sin(5*pi*x) , data=df5))
}
k5(30)
#replication
replicate(100, k5(30), simplify = "array")
smodel5 <- replicate(100, k5(30), simplify = "array")
#transformation
mse5 <- rep(0,100)
for (i in 1:100){
  mse5[i] <- mean(unlist(smodel5[2,i])^2)
}
mse5

#k=6, degree=5
df6 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model6 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+ sin(6*pi*x), data=df6)
#define function
k6 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df6 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+ sin(6*pi*x), data=df6))
}
k6(30)
#replication
replicate(100, k6(30), simplify = "array")
smodel6 <- replicate(100, k6(30), simplify = "array")
#transformation
mse6 <- rep(0,100)
for (i in 1:100){
  mse6[i] <- mean(unlist(smodel6[2,i])^2)
}
mse6

#k=7, degree=6
df7 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model7 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+ sin(7*pi*x), data=df7)
#define function
k7 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xti <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df7 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+ sin(7*pi*x), data=df7))
}
k7(30)
#replication
replicate(100, k7(30), simplify = "array")
smodel7 <- replicate(100, k7(30), simplify = "array")
#transformation
mse7 <- rep(0,100)
for (i in 1:100){
  mse7[i] <- mean(unlist(smodel7[2,i])^2)
}
mse7

#k=8, degree=7
df8 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model8 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+ sin(8*pi*x), data=df8)
#define function
k8 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df8 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+ sin(8*pi*x), data=df8))
}
k8(30)
#replication
replicate(100, k8(30), simplify = "array")
smodel8 <- replicate(100, k8(30), simplify = "array")
#transformation
mse8 <- rep(0,100)
for (i in 1:100){
  mse8[i] <- mean(unlist(smodel8[2,i])^2)
}
mse8

#k=9, degree=8
df9 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model9 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+ sin(9*pi*x), data=df9)
#define function
k9 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df9 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+ sin(9*pi*x), data=df9))
}
k9(30)
#replication
replicate(100, k9(30), simplify = "array")
smodel9 <- replicate(100, k9(30), simplify = "array")
#transformation
mse9 <- rep(0,100)
for (i in 1:100){
  mse9[i] <- mean(unlist(smodel9[2,i])^2)
}
mse9

#k=10, degree=9
df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model10 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x), data=df10)
#define function
k10 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df10 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x), data=df10))
}
k10(30)
#replication
replicate(100, k10(30), simplify = "array")
smodel10 <- replicate(100, k10(30), simplify = "array")
#transformation
mse10 <- rep(0,100)
for (i in 1:100){
  mse10[i] <- mean(unlist(smodel10[2,i])^2)
}
mse10

#k=11, degree=10
df11 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model11 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+ sin(11*pi*x), data=df11)
#define function
k11 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df11 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+ sin(11*pi*x), data=df11))
}
k11(30)
#replication
replicate(100, k11(30), simplify = "array")
smodel11 <- replicate(100, k11(30), simplify = "array")
#transformation
mse11 <- rep(0,100)
for (i in 1:100){
  mse11[i] <- mean(unlist(smodel11[2,i])^2)
}
mse11

#k=12, degree=11
df12 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model12 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+ sin(12*pi*x), data=df12)
#define function
k12 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df12 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+ sin(12*pi*x), data=df12))
}
k12(30)
#replication
replicate(100, k12(30), simplify = "array")
smodel12 <- replicate(100, k12(30), simplify = "array")
#transformation
mse12 <- rep(0,100)
for (i in 1:100){
  mse12[i] <- mean(unlist(smodel12[2,i])^2)
}
mse12

#k=13, degree=12
df13 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model13 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+ sin(13*pi*x), data=df13)
#define function
k13 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df13 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+ sin(13*pi*x), data=df13))
}
k13(30)
#replication
replicate(100, k13(30), simplify = "array")
smodel13 <- replicate(100, k13(30), simplify = "array")
#transformation
mse13 <- rep(0,100)
for (i in 1:100){
  mse13[i] <- mean(unlist(smodel13[2,i])^2)
}
mse13

#k=14, degree=13
df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model14 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+ sin(14*pi*x), data=df14)
#define function
k14 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df14 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+ sin(14*pi*x), data=df14))
}
k14(30)
#replication
replicate(100, k14(30), simplify = "array")
smodel14 <- replicate(100, k14(30), simplify = "array")
#transformation
mse14 <- rep(0,100)
for (i in 1:100){
  mse14[i] <- mean(unlist(smodel14[2,i])^2)
}
mse14

#k=15, degree=14
df15 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model15 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+ sin(15*pi*x), data=df15)
#define function
k15 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df15 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+ sin(15*pi*x), data=df15))
}
k15(30)
#replication
replicate(100, k15(30), simplify = "array")
smodel15 <- replicate(100, k15(30), simplify = "array")
#transformation
mse15 <- rep(0,100)
for (i in 1:100){
  mse15[i] <- mean(unlist(smodel15[2,i])^2)
}
mse15

#k=16, degree=15
df16 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model16 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x), data=df16)
#define function
k16 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df16 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x), data=df16))
}
k16(30)
#replication
replicate(100, k16(30), simplify = "array")
smodel16 <- replicate(100, k16(30), simplify = "array")
#transformation
mse16 <- rep(0,100)
for (i in 1:100){
  mse16[i] <- mean(unlist(smodel16[2,i])^2)
}
mse16

#k=17, degree=16
df17 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model17 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x), data=df17)
#define function
k17 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df17 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x), data=df17))
}
k17(30)
#replication
replicate(100, k17(30), simplify = "array")
smodel17 <- replicate(100, k17(30), simplify = "array")
#transformation
mse17 <- rep(0,100)
for (i in 1:100){
  mse17[i] <- mean(unlist(smodel17[2,i])^2)
}
mse17

#k=18, degree=17
df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
model18 <- lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x)+sin(18*pi*x), data=df18)
#define function
k18 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  xi <- seq(0, 1, by = 1/29)
  g <- (sin(2*pi*xi))^2 + noise
  df18 <- data.frame(x = seq(0, 1, by = 1/29), y = (sin(2*pi*xi))^2 + noise)
  return(lm(y ~ sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x)+sin(18*pi*x), data=df18))
}
k18(30)
#replication
replicate(100, k18(30), simplify = "array")
smodel18 <- replicate(100, k18(30), simplify = "array")
#transformation
mse18 <- rep(0,100)
for (i in 1:100){
  mse18[i] <- mean(unlist(smodel18[2,i])^2)
}
mse18

#average
avg30mse1 <- sum(mse1)/100 
avg30mse2 <- sum(mse2)/100 
avg30mse3 <- sum(mse3)/100 
avg30mse4 <- sum(mse4)/100
avg30mse5 <- sum(mse5)/100 
avg30mse6 <- sum(mse6)/100
avg30mse7 <- sum(mse7)/100
avg30mse8 <- sum(mse8)/100
avg30mse9 <- sum(mse9)/100
avg30mse10 <- sum(mse10)/100
avg30mse11 <- sum(mse11)/100
avg30mse12 <- sum(mse12)/100
avg30mse13 <- sum(mse13)/100
avg30mse14 <- sum(mse14)/100
avg30mse15 <- sum(mse15)/100
avg30mse16 <- sum(mse16)/100
avg30mse17 <- sum(mse17)/100
avg30mse18 <- sum(mse18)/100

#log
avgte_sin <- c(avg30mse1, avg30mse2, avg30mse3, avg30mse4, avg30mse5, avg30mse6, avg30mse7, avg30mse8, avg30mse9, avg30mse10, avg30mse11, avg30mse12, avg30mse13, avg30mse14, avg30mse15, avg30mse16, avg30mse17, avg30mse18)
print(avgte_sin)
log(avgte_sin)
degree <- c(1:18)
print(degree)
plot(degree, log(avgte_sin),'l')


#-------------------------------------------------------------------------------




#3d-c (with 1000 points)







#data
set.seed(23)
noise <- rnorm(30, 0, 0.07)
xti <- runif(30, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
train <- data.frame(y = g,
                    x = xti)

noise <- rnorm(1000, 0, 0.07)
xti <- runif(1000, 0, 1)
g <- (sin(2*pi*xti))^2 + noise
test <- data.frame(y = g,
                   x = xti)


#k=1
model1 <- lm(y ~ 1 , data=train)

#Replication
k1 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model1, test))
}
k1(1000)
#replication
replicate(100, k1(1000), simplify = "array")
smodel1 <- replicate(100, k1(1000), simplify = "array")
#transformation
mse1 <- rep(0,100)
for (i in 1:100){
  mse1[i] <- mean(unlist(smodel1[2,i])^2)
}
mse1

#k=2
model2 <- lm(y ~ 1 + sin(pi*x) , data=train)
#define function
k2 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model2, test))
}
k2(1000)

replicate(100, k2(1000), simplify = "array")
smodel2 <- replicate(100, k2(1000), simplify = "array")

mse2 <- rep(0,100)
for (i in 1:100){
  mse2[i] <- mean(unlist(smodel2[2,i])^2)
}
mse2


#k=3,degree=2
model3 <- lm(y ~ 1 +sin(pi*x)+sin(2*pi*x) , data=train)

k3 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model3, test))
}
k3(1000)
replicate(100, k3(1000), simplify = "array")
smodel3 <- replicate(100, k3(1000), simplify = "array")
#transformation
mse3 <- rep(0,100)
for (i in 1:100){
  mse3[i] <- mean(unlist(smodel3[2,i])^2)
}
mse3

#k=4, degree=3
model4 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x) , data=train)

k4 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model4, test))
}
k4(1000)

replicate(100, k4(1000), simplify = "array")
smodel4 <- replicate(100, k4(1000), simplify = "array")

mse4 <- rep(0,100)
for (i in 1:100){
  mse4[i] <- mean(unlist(smodel4[2,i])^2)
}
mse4

#k=5, degree=4
model5 <- lm(y ~ 1 +sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x) , data=train)

k5 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model5, test))
}
k5(1000)

replicate(100, k5(1000), simplify = "array")
smodel5 <- replicate(100, k5(1000), simplify = "array")

mse5 <- rep(0,100)
for (i in 1:100){
  mse5[i] <- mean(unlist(smodel5[2,i])^2)
}
mse5

#k=6, degree=5
model6 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x), data=train)

k6 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model6, test))
}
k6(1000)

replicate(100, k6(1000), simplify = "array")
smodel6 <- replicate(100, k6(1000), simplify = "array")

mse6 <- rep(0,100)
for (i in 1:100){
  mse6[i] <- mean(unlist(smodel6[2,i])^2)
}
mse6

#k=7, degree=6
model7 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x), data=train)

k7 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model7, test))
}
k7(1000)

replicate(100, k7(1000), simplify = "array")
smodel7 <- replicate(100, k7(1000), simplify = "array")

mse7 <- rep(0,100)
for (i in 1:100){
  mse7[i] <- mean(unlist(smodel7[2,i])^2)
}
mse7

#k=8
model8 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x), data=train)

k8 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model8, test))
}
k8(1000)

replicate(100, k8(1000), simplify = "array")
smodel8 <- replicate(100, k8(1000), simplify = "array")

mse8 <- rep(0,100)
for (i in 1:100){
  mse8[i] <- mean(unlist(smodel8[2,i])^2)
}
mse8

#k=9, 
model9 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x), data=train)

k9 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model9, test))
}

k9(1000)

replicate(100, k9(1000), simplify = "array")
smodel9 <- replicate(100, k9(1000), simplify = "array")

mse9 <- rep(0,100)
for (i in 1:100){
  mse9[i] <- mean(unlist(smodel9[2,i])^2)
}
mse9

#k=10
model10 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x), data=train)

k10 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model10, test))
}
k10(1000)

replicate(100, k10(1000), simplify = "array")
smodel10 <- replicate(100, k10(1000), simplify = "array")

mse10 <- rep(0,100)
for (i in 1:100){
  mse10[i] <- mean(unlist(smodel10[2,i])^2)
}
mse10

#k=11
model11 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x), data=train)

k11 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model11, test))
}
k11(1000)

replicate(100, k11(1000), simplify = "array")
smodel11 <- replicate(100, k11(1000), simplify = "array")

mse11 <- rep(0,100)
for (i in 1:100){
  mse11[i] <- mean(unlist(smodel11[2,i])^2)
}
mse11

#k=12
model12 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x), data=train)

k12 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model12, test))
}
k12(1000)

replicate(100, k12(1000), simplify = "array")
smodel12 <- replicate(100, k12(1000), simplify = "array")

mse12 <- rep(0,100)
for (i in 1:100){
  mse12[i] <- mean(unlist(smodel12[2,i])^2)
}
mse12

#k=13
model13 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x), data=train)

k13 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model13, test))
}
k13(1000)

replicate(100, k13(1000), simplify = "array")
smodel13 <- replicate(100, k13(1000), simplify = "array")

mse13 <- rep(0,100)
for (i in 1:100){
  mse13[i] <- mean(unlist(smodel13[2,i])^2)
}
mse13

#k=14
model14 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x), data=train)

k14 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model14, test))
}
k14(1000)

replicate(100, k14(1000), simplify = "array")
smodel14 <- replicate(100, k14(1000), simplify = "array")

mse14 <- rep(0,100)
for (i in 1:100){
  mse14[i] <- mean(unlist(smodel14[2,i])^2)
}
mse14

#k=15
model15 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x), data=train)

k15 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model15, test))
}
k15(1000)

replicate(100, k15(1000), simplify = "array")
smodel15 <- replicate(100, k15(1000), simplify = "array")

mse15 <- rep(0,100)
for (i in 1:100){
  mse15[i] <- mean(unlist(smodel15[2,i])^2)
}
mse15

#k=16 
model16 <- lm(y ~ 1 + sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x), data=train)

k16 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model16, test))
}
k16(1000)

replicate(100, k16(1000), simplify = "array")
smodel16 <- replicate(100, k16(1000), simplify = "array")

mse16 <- rep(0,100)
for (i in 1:100){
  mse16[i] <- mean(unlist(smodel16[2,i])^2)
}
mse16

#k=17
model17 <- lm(y ~ 1+sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x), data=train)

k17 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model17, test))
}
k17(1000)

replicate(100, k17(1000), simplify = "array")
smodel17 <- replicate(100, k17(1000), simplify = "array")

mse17 <- rep(0,100)
for (i in 1:100){
  mse17[i] <- mean(unlist(smodel17[2,i])^2)
}
mse17

#k=18 
model18 <- lm(y ~ 1+sin(pi*x)+sin(2*pi*x)+sin(3*pi*x)+ sin(4*pi*x)+sin(5*pi*x)+sin(6*pi*x)+sin(7*pi*x)+sin(8*pi*x)+sin(9*pi*x)+ sin(10*pi*x)+sin(11*pi*x)+sin(12*pi*x)+sin(13*pi*x)+sin(14*pi*x)+sin(15*pi*x)+ sin(16*pi*x)+sin(17*pi*x), data=train)

k18 <- function(n){
  noise <- rnorm(n, 0, 0.07)
  return(predict(model18, test))
}
k18(1000)

replicate(100, k18(1000), simplify = "array")
smodel18 <- replicate(100, k18(1000), simplify = "array")

mse18 <- rep(0,100)
for (i in 1:100){
  mse18[i] <- mean(unlist(smodel18[2,i])^2)
}
mse18

#average
avgmse1 <- sum(mse1)/100 
avgmse2 <- sum(mse2)/100 
avgmse3 <- sum(mse3)/100 
avgmse4 <- sum(mse4)/100
avgmse5 <- sum(mse5)/100 
avgmse6 <- sum(mse6)/100
avgmse7 <- sum(mse7)/100
avgmse8 <- sum(mse8)/100
avgmse9 <- sum(mse9)/100
avgmse10 <- sum(mse10)/100
avgmse11 <- sum(mse11)/100
avgmse12 <- sum(mse12)/100
avgmse13 <- sum(mse13)/100
avgmse14 <- sum(mse14)/100
avgmse15 <- sum(mse15)/100
avgmse16 <- sum(mse16)/100
avgmse17 <- sum(mse17)/100
avgmse18 <- sum(mse18)/100

#log
avgtse_sin <- c(avgmse1, avgmse2, avgmse3, avgmse4, avgmse5, avgmse6, avgmse7, avgmse8, avgmse9, avgmse10, avgmse11, avgmse12, avgmse13, avgmse14, avgmse15, avgmse16, avgmse17, avgmse18)
print(avgtse_sin)
log(avgtse_sin)
degree <- c(1:18)
print(degree)
plot(degree, log(avgtse_sin),'l')






#-------------------------------------------------------------------------------




####QUESTION 4





#-----------------------------------------------------------------------------





#####QUESTION 4







#4a
#a

test.data <- read.csv("Boston-filtered.csv",header=TRUE)

#shuffle data
set.seed(23)
trainmse <- matrix(0, 20, 1)
testmse <- matrix(0, 20, 1)

for(i in 1:20){
  
  data = test.data[sample(1:nrow(test.data)), ]
  split <- round(nrow(data) * (2/3))
  train_data <- data[1:split, ]
  
  train_data$ones <- rep(1, nrow(train_data))
  
  test_data <- data[(split + 1):nrow(data),]
  
  test_data$ones <- rep(1, nrow(test_data ))
  
  
  model <- lm(MEDV ~  ones + 0, data = train_data )
  
  trainmse[i,1] <- mean((predict(model,train_data) - train_data$MEDV )^2)
  
  
  p <- predict(model, test_data)
  
  testmse[i,1] <- mean((p - test_data$MEDV )^2)
  
  
}
colnames(trainmse) <- "ones"
colnames(testmse) <- "ones_test"
trainmse
testmse

#means and sds over 20 runs for each predictor

mean_train_mse <- apply(trainmse, 2, mean)
mean_train_mse

sd_train_mse <- apply(trainmse, 2, sd)
sd_train_mse

mean_test_mse <- apply(testmse, 2, mean)
mean_test_mse

sd_test_mse <- apply(testmse, 2, sd)
sd_test_mse




#----------------------------------------------------------------------------------




#4b





test.data <- read.csv("Boston-filtered.csv",header=TRUE)

#shuffle data
set.seed(23)
constants <- numeric(20)

for(i in 1:20){
  
  data = test.data[sample(1:nrow(test.data)), ]
  split <- round(nrow(data) * (2/3))
  train_data <- data[1:split, ]
  
  train_data$ones <- rep(1, nrow(train_data))
  
  test_data <- data[(split + 1):nrow(data),]
  
  test_data$ones <- rep(1, nrow(test_data ))
  
  model <- lm(MEDV ~  ones + 0, data = train_data )
  
  constants[i] <- predict(model,train_data) 
  
}
#the constants shown in the 20 randomly splits

constants


mean(constants)

#-------------------------------------------------------------------------------





##4c

test.data <- read.csv("Boston-filtered.csv",header=TRUE)

#shuffle data
set.seed(23)
trainmse <- matrix(0, 20, 12)
testmse <- matrix(0, 20, 12)

for(i in 1:20){
  
  CRIMdata = test.data[sample(1:nrow(test.data)), ]
  split <- round(nrow(CRIMdata) * (2/3))
  trainCRIM_data <- CRIMdata[1:split, ]
  testCRIM_data <- CRIMdata[(split + 1):nrow(CRIMdata),]
  
  for(j in 1:12) {
    
    model <- lm(MEDV ~ ., trainCRIM_data[,c(j,13)])
    
    
    trainmse[i,j] <- mean((predict(model,trainCRIM_data) - trainCRIM_data$MEDV )^2)
    
    
    p <- predict(model, testCRIM_data)
    
    testmse[i,j] <- mean((p - testCRIM_data$MEDV )^2)
    
    
  }
  
}
colnames(trainmse) <- colnames(test.data)[1:12]
colnames(testmse) <- colnames(test.data)[1:12]
trainmse
testmse

#means and sds over 20 runs for each predictor

mean_train_mse <- apply(trainmse, 2, mean)
mean_train_mse

sd_train_mse <- apply(trainmse, 2, sd)
sd_train_mse

mean_test_mse <- apply(testmse, 2, mean)
mean_test_mse

sd_test_mse <- apply(testmse, 2, sd)
sd_test_mse





#-------------------------------------------------------------------------------




##4d





test.data <- read.csv("Boston-filtered.csv",header=TRUE)

#shuffle data
set.seed(23)
trainmse <- testmse <- matrix(0, 20, 1)

for(i in 1:20){
  
  data = test.data[sample(1:nrow(test.data)), ]
  split <- round(nrow(data) * (2/3))
  train_data <- data[1:split, ]
  
  
  test_data <- data[(split + 1):nrow(data),]
  
  test_data$ones <- rep(1, nrow(test_data ))
  
  
  model <- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX	+ PTRATIO + LSTAT, data = train_data )
  
  trainmse[i,1] <- mean((predict(model,train_data) - train_data$MEDV )^2)
  
  
  p <- predict(model, test_data)
  
  testmse[i,1] <- mean((p - test_data$MEDV )^2)
  
  
}

trainmse
testmse

#means and sds over 20 runs for each predictor

mean_train_mse <- apply(trainmse, 2, mean)
mean_train_mse

sd_train_mse <- apply(trainmse, 2, sd)
sd_train_mse

mean_test_mse <- apply(testmse, 2, mean)
mean_test_mse

sd_test_mse <- apply(testmse, 2, sd)
sd_test_mse





##5a
#-------------------------------------------------------------------------------
#define data
data <- read.csv("Boston-filtered.csv")

set.seed(1)

n <- nrow(data)

id <- sample(1:n, 2/3 * n)

train <- data[id, ]
test <- data[-id, ]


x.train <- train[, -ncol(train)]
y.train <- train[, ncol(train)]

x.test <- test[, -ncol(test)]
y.test <- test[, ncol(test)]

kfold <- 5
folds <- rep_len(1:kfold, nrow(x.train))
folds <- sample(folds)


K <- function(x, y, sig) {
  
  exp(- sum((x - y)^2) / (2*sig^2))
  
}


gammas <- 2^seq(-40, -26, 1)

sigs <- 2^seq(7, 13, 0.5)


cv.mse <- array(NA, dim = c(length(sigs), length(gammas), kfold))

for(k in 1:kfold) {
  
  
  for (a in 1:length(sigs)) {
    
    sig <-  sigs[a]
    
    x.valid.train <- x.train[folds !=k, ]
    y.valid.train <- y.train[folds !=k ]
    x.valid.valid <- x.train[folds == k, ]
    y.valid.valid <- y.train[folds ==k ]
    Kmat <- matrix(0,nrow(  x.valid.train),nrow(  x.valid.train) )
    
    for(i in 1:nrow(x.valid.train)) {
      
      difs <- x.valid.train -  matrix(rep(as.numeric(rep(x.valid.train[i, ]))
                                          ,nrow(x.valid.train)),nrow(x.valid.train),byrow=T)
      
      
      Kmat[i, ] <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*sig^2)))
      
    }
    
    for(b in 1:length(gammas)) {
      
      
      gamma <- gammas[b]
      l <- nrow(x.valid.train)
      
      alphastar <-  solve(Kmat + gamma * l * diag(l)) %*% y.valid.train
      
      
      #predictions on the validation set
      ytests <- c()
      
      #test on valid. set
      for(nn in 1:nrow(x.valid.valid)) {
        
        difs <- x.valid.train -  matrix(rep(as.numeric(rep(x.valid.valid[nn, ]))
                                            ,nrow(x.valid.train)),nrow(x.valid.train),byrow=T)
        
        kk <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*sig^2)))
        ytests[nn] <- sum(alphastar[,1] * kk)
      }
      
      #store cross validation (gamma_a, sig_b, kth fold) as validation
      cv.mse[a,b, k] <-  mean((ytests - y.valid.valid)^2)
      
    }
    
    
  }
  
  
}

#kfolds averaged MSE(gamma and sig)
cv_avg <- apply(cv.mse, 1:2, mean)

cv_avg 



#find lowest MSE
pos <- which(cv_avg == min(cv_avg ), arr.ind = TRUE)

#best parameter
bestsig <- sigs[pos[1,1]]
bestsig
bestgamma <- gammas[pos[1,2]]
bestgamma


#re-train on training data


Kmat <- matrix(0,nrow(  x.train),nrow(  x.train) )

for(i in 1:nrow(x.train)) {
  
  difs <- x.train -  matrix(rep(as.numeric(rep(x.train[i, ]))
                                ,nrow(x.train)),nrow(x.train),byrow=T)
  
  Kmat[i, ] <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*bestsig^2)))
  
}

l <- nrow(x.train)

alphastar <-  solve(Kmat + bestgamma * l * diag(l)) %*% y.train


#predictions on the train set

ytests <- c()

#for each test in train set
for(nn in 1:nrow(x.train)) {
  
  difs <- x.train -  matrix(rep(as.numeric(rep(x.train[nn, ]))
                                ,nrow(x.train)),nrow(x.train),byrow=T)
  
  kk <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*bestsig^2)))
  ytests[nn] <- sum(alphastar[,1] * kk)
}


trainingerror <- mean((ytests - y.train)^2)
trainingerror
#4.225576
#total error
sqrt(trainingerror*337)
#37.73618

#predictions on the test set

ytests <- c()

#for each test in test set
for(nn in 1:nrow(x.test)) {
  
  difs <- x.train -  matrix(rep(as.numeric(rep(x.test[nn, ]))
                                ,nrow(x.train)),nrow(x.train),byrow=T)
  
  kk <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*bestsig^2)))
  ytests[nn] <- sum(alphastar[,1] * kk)
}


testingerror <- mean((ytests - y.test)^2)
testingerror #14.25957
sqrt(testingerror*169)
#49.0904

##5b
#-------------------------------------------------------------------------------
contour(
  log(sigs,2) ,
  log(lambdas,2),
  nlevels = 50,
  cv_avg,
  xlab = expression(sigma),
  ylab = expression(lambda),
)







##5d)
#-------------------------------------------------------------------------------
#only questions in Q5 writes here, questions for Q4 stores below their original question



data <- read.csv("Boston-filtered.csv")

set.seed(1)


gamma <- 2^seq(-40, -26, 1) 

sigs <- 2^seq(7, 13, 0.5) 


cv_total <- matrix(0, length(sigs), length(gamma))


n <- nrow(data)

repeattimes <- 20

bigtrainingerrors <-  numeric(repeattimes)
bigtestingerrors <-  numeric(repeattimes)



for(bigloop in 1:repeattimes) {
  
  
  id <- sample(1:n, 2/3 * n)
  
  train <- data[id, ]
  test <- data[-id, ]
  
  
  x.train <- train[, -ncol(train)]
  y.train <- train[, ncol(train)]
  
  x.test <- test[, -ncol(test)]
  y.test <- test[, ncol(test)]
  
  kfold <- 5
  folds <- rep_len(1:kfold, nrow(x.train))
  folds <- sample(folds)
  
  
  K <- function(x, y, sig) {
    
    exp(- sum((x - y)^2) / (2*sig^2))
    
  }
  
  
  cv.mse <- array(NA, dim = c(length(sigs), length(gamma), kfold))
  
  for(k in 1:kfold) {
    
    
    for (a in 1:length(sigs)) {
      
      sig <-  sigs[a]
      
      x.valid.train <- x.train[folds !=k, ]
      y.valid.train <- y.train[folds !=k ]
      x.valid.valid <- x.train[folds == k, ]
      y.valid.valid <- y.train[folds ==k ]
      Kmat <- matrix(0,nrow(  x.valid.train),nrow(  x.valid.train) )
      
      for(i in 1:nrow(x.valid.train)) {
        
        difs <- x.valid.train -  matrix(rep(as.numeric(rep(x.valid.train[i, ]))
                                            ,nrow(x.valid.train)),nrow(x.valid.train),byrow=T)
        
        
        Kmat[i, ] <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*sig^2)))
        
      }
      
      for(b in 1:length(gamma)) {
        
        
        gamma <- gamma[b]
        l <- nrow(x.valid.train)
        
        alphastar <-  solve(Kmat + gamma * l * diag(l)) %*% y.valid.train
        
        
        #predictions on validation set
        ytests <- c()
        
        #test on valid. set
        for(nn in 1:nrow(x.valid.valid)) {
          
          difs <- x.valid.train -  matrix(rep(as.numeric(rep(x.valid.valid[nn, ]))
                                              ,nrow(x.valid.train)),nrow(x.valid.train),byrow=T)
          
          kk <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*sig^2)))
          ytests[nn] <- sum(alphastar[,1] * kk)
        }
        
        #store cross validation
        cv.mse[a,b, k] <-  mean((ytests - y.valid.valid)^2)
        
      }
      
      
    }
    
    
  }
  
  #kfolds averaged MSE (sig,gamma)
  cv_avg <- apply(cv.mse, 1:2, mean)
  
  row.names(cv_avg) <- sigs
  colnames(cv_avg) <- gamma
  
  cv_avg
  cv_total <- cv_total  +  cv_avg
  
  
  #find lowest MSE
  pos <- which(cv_avg == min(cv_avg ), arr.ind = TRUE)
  
  #best parameter
  bestsig <- sigs[pos[1,1]]
  bestsig
  bestgamma <- gamma[pos[1,2]]
  bestgamma
  
  
  #re-training(training)
  Kmat <- matrix(0,nrow(  x.train),nrow(  x.train) )
  
  for(i in 1:nrow(x.train)) {
    
    difs <- x.train -  matrix(rep(as.numeric(rep(x.train[i, ]))
                                  ,nrow(x.train)),nrow(x.train),byrow=T)
    
    Kmat[i, ] <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*bestsig^2)))
    
  }
  
  l <- nrow(x.train)
  
  alphastar <-  solve(Kmat + bestgamma * l * diag(l)) %*% y.train
  
  
  #predicting on the train set
  
  ytests <- c()
  
  #testing 
  for(nn in 1:nrow(x.train)) {
    
    
    difs <- x.train -  matrix(rep(as.numeric(rep(x.train[nn, ]))
                                  ,nrow(x.train)),nrow(x.train),byrow=T)
    
    kk <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*bestsig^2)))
    ytests[nn] <- sum(alphastar[,1] * kk)
  }
  
  
  trainingerror <- mean((ytests - y.train)^2)
  trainingerror
  
  
  #predictions on the test set
  
  ytests <- c()
  
  #test on test set
  for(nn in 1:nrow(x.test)) {
    
    difs <- x.train -  matrix(rep(as.numeric(rep(x.test[nn, ]))
                                  ,nrow(x.train)),nrow(x.train),byrow=T)
    
    kk <-  apply(difs, 1, function(x) exp(-sum(x^2)/(2*bestsig^2)))
    ytests[nn] <- sum(alphastar[,1] * kk)
  }
  
  
  testingerror <- mean((ytests - y.test)^2)
  testingerror
  
  
  bigtrainingerrors[bigloop] <- trainingerror
  bigtestingerrors[bigloop] <- testingerror
  
}

#20 repeated results
df <- data.frame(bigtrainingerrors,
                 bigtestingerrors
)
df
#standard errors(train, test)
sd(df[,1])
sd(df[,2])























