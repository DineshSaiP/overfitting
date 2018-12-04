#Loading the data of boston housing data
getwd()
setwd('C:\\Users\\bharath\\Desktop\\datasets')
hou<-read.csv('boston_housing.csv')
cor(hou)
dim(hou)
#Removing the rows with the NULL values a
house=na.omit(hou)
dim(house)
cor(house)
#MEDV and RM are having a correlation of .74
plot(house$RM,house$MEDV)

#Problem-1
#7th degree polynomial with test error 
l=c()#empty list for creating a list of error values

#selecting a sample with different sizes and fitting in model having order 7
for(i in c(20,30,50,70,100,200,350)){
  rand = sample(1:nrow(house),i)
  train = house[rand, ]
  test = house[403:452,]
  #fitting the model with test and train data
  m1 <- lm(MEDV ~ RM+ I(RM^2) + I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train)
  #PLOTTING THE MODEL OVER THE DATA
  #plot(train$RM,train$MEDV, pch=19, cex=0.5)
  #lines(sort(train$RM), fitted(m1)[order(train$RM)], col='red', type='l')
  sum(m1$residuals^2)
  pred = predict(m1, newdata=test)
  l=append(l,sum((pred-test$MEDV)^2))
}
plot(c(20,30,50,70,100,200,350),l,type="l",main="Samples size and test error for order 7",xlab="Sample Size",ylab="Test Errors",col='blue')

#9th degree polynomial with test error 
l=c()#empty list for creating a list of error values

#selecting a sample with different sizes and fitting in model having order 7
for(i in c(20,30,50,70,100,200,350)){
  rand = sample(1:nrow(house),i)
  train = house[rand, ]
  test = house[403:452,]
  #fitting the model with test and train data
  m1 <- lm(MEDV ~ RM+ I(RM^2) + I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train)
  #PLOTTING THE MODEL OVER THE DATA
  #plot(train$RM,train$MEDV, pch=19, cex=0.5)
  #lines(sort(train$RM), fitted(m1)[order(train$RM)], col='red', type='l')
  sum(m1$residuals^2)
  pred = predict(m1, newdata=test)
  l=append(l,sum((pred-test$MEDV)^2))
}
plot(c(20,30,50,70,100,200,350),l,type="l",main="Samples size and test error for order 9",xlab="Sample Size",ylab="Test Errors",col='blue')



#2nd Question

test1_error<-c()
test2_error<-c()
test3_error<-c()
test4_error<-c()

train1_error<-c()
train2_error<-c()
train3_error<-c()
train4_error<-c()

order_num=c(1,2,7,8,9,10)
#DATA:
train1 = house[1:20,]
train2 = house[21:40,]
train3 = house[41:60,]
train4 = house[61:80,]

test = house[401:450,]

#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 1
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train1)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train1)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train1)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train1)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train1)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train1)

#For order 1
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with order 1",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml1)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with order 2",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml2)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with order 7",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml7)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with order 8",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml8)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with order 9",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml9)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with order 10",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml10)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#Plotting all the order lines in a single graph
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=20) with different orders",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(m11)[order(train1$RM)], col='red', type='l') 
lines(sort(train1$RM), fitted(m12)[order(train1$RM)], col='green3', type='l') 
lines(sort(train1$RM), fitted(m17)[order(train1$RM)], col='orange', type='l') 
lines(sort(train1$RM), fitted(m18)[order(train1$RM)], col='mediumorchid4', type='l') 
lines(sort(train1$RM), fitted(m19)[order(train1$RM)], col='deeppink', type='l') 
lines(sort(train1$RM), fitted(m110)[order(train1$RM)], col='blue', type='l') 
legend("topright", legend = c("degree_1", "degree_2","degree_7","degree_8","degree_9","degree_10" ), col = c("red","green3","orange","mediumorchid4","deeppink","blue"),lty=1:2,cex = 0.8)

#ggplot for Test Train and complexity
library('ggplot2')
sample1 <- data.frame(order_num, train1_error, test1_error)
graph <- ggplot(sample1, aes(x = order_num, y = test1_error, col="Test Error"))
graph <- graph + geom_line(size  = 1.2)
graph <- graph + geom_line(aes(y = train1_error, col="Train Error"),size = 1.2)
graph <- graph + xlab("Order Number") + ylab("Errors") + 
  ggtitle("Model Complexity vs Train and Test Errors for Sample1 of Size 20") + 
  theme(axis.title.x = element_text(color = 'Brown', size = 13),
        axis.title.y = element_text(color = 'Brown', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))
graph

#Plot between Order number and Test error
plot(order_num,test1_error,type='l', pch=19, cex=0.5,main="Sample1(size=20) with different orders",xlab="Order numbers",ylab="Test Errors")



#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 2
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train2)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train2)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train2)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train2)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train2)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train2)

#For order 1
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#Plot between Order number and Test error
plot(order_num,test2_error,type='l', pch=19, cex=0.5,main="Sample2(size=20) with different orders",xlab="Order numbers",ylab="Test Errors")



#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 3
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train3)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train3)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train3)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train3)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train3)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train3)

#For order 1
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#Plot between Order number and Test error
plot(order_num,test3_error,type='l', pch=19, cex=0.5,main="Sample3(size=20) with different orders",xlab="Order numbers",ylab="Test Errors")



#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 4
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train4)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train4)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train4)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train4)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train4)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train4)

#For order 1
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#Plot between Order number and Test error
plot(order_num,test4_error,type='l', pch=19, cex=0.5,main="Sample4(size=20) with different orders",xlab="Order numbers",ylab="Test Errors")

#Problem-3
#------------------------------------------------------------------------------------------------------------


test1_error<-c()
test2_error<-c()
test3_error<-c()
test4_error<-c()

train1_error<-c()
train2_error<-c()
train3_error<-c()
train4_error<-c()

order_num=c(1,2,7,8,9,10)
#DATA:
train1 = house[1:100,]
train2 = house[101:200,]
train3 = house[201:300,]
train4 = house[301:400,]

test = house[401:450,]

#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 1
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train1)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train1)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train1)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train1)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train1)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train1)

#For order 1
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with order 1",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml1)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with order 2",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml2)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with order 7",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml7)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with order 8",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml8)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with order 9",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml9)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#PLOTTING THE MODEL OVER THE DATA
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with order 10",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(ml10)[order(train1$RM)], col='blue', type='l', pch=19) 


#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train1_error=append(train1_error,sum(ml1$residuals^2))
test1_error=append(test1_error,sum((pred-test$MEDV)^2))


#Plotting all the order lines in a single graph
plot(train1$RM,train1$MEDV, pch=19, cex=0.5,main="Sample1(size=100) with different orders",xlab="Predictor(No_of_rooms)",ylab="Target(Price)")
lines(sort(train1$RM), fitted(m11)[order(train1$RM)], col='red', type='l') 
lines(sort(train1$RM), fitted(m12)[order(train1$RM)], col='green3', type='l') 
lines(sort(train1$RM), fitted(m17)[order(train1$RM)], col='orange', type='l') 
lines(sort(train1$RM), fitted(m18)[order(train1$RM)], col='mediumorchid4', type='l') 
lines(sort(train1$RM), fitted(m19)[order(train1$RM)], col='deeppink', type='l') 
lines(sort(train1$RM), fitted(m110)[order(train1$RM)], col='blue', type='l') 
legend("topright", legend = c("degree_1", "degree_2","degree_7","degree_8","degree_9","degree_10" ), col = c("red","green3","orange","mediumorchid4","deeppink","blue"),lty=1:2,cex = 0.8)

#ggplot for Test Train and complexity
library('ggplot2')
sample1 <- data.frame(order_num, train1_error, test1_error)
graph <- ggplot(sample1, aes(x = order_num, y = test1_error, col="Test Error"))
graph <- graph + geom_line(size  = 1.2)
graph <- graph + geom_line(aes(y = train1_error, col="Train Error"),size = 1.2)
graph <- graph + xlab("Order Number") + ylab("Errors") + 
  ggtitle("Model Complexity vs Train and Test Errors for Sample1 of Size 100") + 
  theme(axis.title.x = element_text(color = 'Brown', size = 13),
        axis.title.y = element_text(color = 'Brown', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))
graph

#Plot between Order number and Test error
plot(order_num,test1_error,type='l', pch=19, cex=0.5,main="Sample1(size=100) with different orders",xlab="Order numbers",ylab="Test Errors")



#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 2
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train2)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train2)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train2)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train2)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train2)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train2)

#For order 1
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train2_error=append(train2_error,sum(ml1$residuals^2))
test2_error=append(test2_error,sum((pred-test$MEDV)^2))


#Plot between Order number and Test error
plot(order_num,test2_error,type='l', pch=19, cex=0.5,main="Sample2((size=100)) with different orders",xlab="Order numbers",ylab="Test Errors")



#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 3
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train3)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train3)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train3)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train3)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train3)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train3)

#For order 1
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train3_error=append(train3_error,sum(ml1$residuals^2))
test3_error=append(test3_error,sum((pred-test$MEDV)^2))


#Plot between Order number and Test error
plot(order_num,test3_error,type='l', pch=19, cex=0.5,main="Sample3(size=100) with different orders",xlab="Order numbers",ylab="Test Errors")



#=============================================================================================
# FITTING DIFFERENT ORDER POLYNOMIAL REGRESSION FOR SAMPLE 4
#=============================================================================================

ml1 <- lm(MEDV ~ RM, train4)
ml2 <- lm(MEDV ~ RM+ I(RM^2), train4)
ml7 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7), train4)
ml8 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8), train4)
ml9 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9), train4)
ml10 <- lm(MEDV ~ RM+ I(RM^2)+ I(RM^3)+ I(RM^4)+ I(RM^5)+ I(RM^6)+ I(RM^7)+ I(RM^8)+ I(RM^9)+I(RM^10), train4)

#For order 1
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml1$residuals^2)
pred = predict(ml1, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))

#For order 2
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml2$residuals^2)
pred = predict(ml2, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))

#For order 7
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml7$residuals^2)
pred = predict(ml7, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#For order 8
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml8$residuals^2)
pred = predict(ml8, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#For order 9
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml9$residuals^2)
pred = predict(ml9, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#For order 10
#---------------------------------------------------------------------------------------------
#TRAIN AND TEST ACCURACY
sum(ml10$residuals^2)
pred = predict(ml10, newdata=test)
sum((pred-test$MEDV)^2)
train4_error=append(train4_error,sum(ml1$residuals^2))
test4_error=append(test4_error,sum((pred-test$MEDV)^2))


#Plot between Order number and Test error
plot(order_num,test4_error,type='l', pch=19, cex=0.5,main="Sample4(size=100) with different orders",xlab="Order numbers",ylab="Test Errors")

