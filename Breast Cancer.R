rm(list=ls())
setwd("/Users/liaoyantai/Documents/study/545 data mining/HW4 Yantai Liao 50093183")
library(gbm)
library(randomForest)
library(MMST)
data(wdbc)

####

my_data<-wdbc[,-1]
set.seed(12345)
training<-sample(1:nrow(my_data),0.8*nrow(my_data))
wdbc.training<-my_data[training,]
wdbc.test<-my_data[-training,]
y.true.test<-as.numeric(wdbc.test$class)-1

# Random Forest

rf.fit<-randomForest(class~.,data= wdbc.training,n.tree=10000)
y.hat<-predict(rf.fit,newdata=wdbc.test,type="response")
y.hat<-as.numeric(y.hat)-1
misclass.rf<-sum(abs(y.true.test-y.hat))/length(y.hat)
misclass.rf # 0.03508772

# Bagging

bag.fit<-randomForest(class~.,data= wdbc.training,n.tree=10000,mtry=30)
y.hat<-predict(bag.fit,newdata=wdbc.test,type="response")
y.hat<-as.numeric(y.hat)-1
misclass.bag<-sum(abs(y.true.test-y.hat))/length(y.hat)
misclass.bag # 0.07017544

# Boosting

boost.train<-wdbc.training
boost.train$class<-as.numeric(boost.train$class)-1
boost.test<-wdbc.test
boost.test$class<-as.numeric(boost.test$class)-1

shrink<-c(.1, .4, .6, .8)
max.iter<-1000
store.error<-c()
for(i in 1:length(shrink)){
  boost.fit<-gbm(class~.,data=boost.train,n.trees=max.iter,shrinkage=shrink[i],interaction.depth=3,distribution="adaboost")
  temp<-c()
  for(j in 1:max.iter){
    y.hat<-predict(boost.fit,newdata=boost.test,n.trees=j,type="response")
    misclass.boost<-sum(abs(y.hat-y.true.test))/length(y.hat)
    temp<-c(temp,misclass.boost)
  }
  store.error<-cbind(store.error,temp)
}

store.error

quartz()
plot(store.error[,1],main="Error profiles",ylab="error",xlab="boosting interations")
lines(store.error[,2],col="red")
lines(store.error[,3],col="blue")
lines(store.error[,4],col="green")
legend("topright",c("shrinkage:0.1","shrinkage:0.4","shrinkage:0.6","shrinkage:0.8"),lty=c(1,2,3,4),col=c("black","red","blue","green"))

which.min(store.error[,3]) # 730
store.error[730,3] # 0.02323512 shrinkage:0.6 

# Logistic regression

glm.train<-wdbc.training
glm.train$class<-as.numeric(boost.train$class)
glm.test<-wdbc.test
glm.test$class<-as.numeric(boost.test$class)
glm.fit<-glm(class~.,data=glm.train,family="binomial")
glm.pred.test<-predict(glm.fit,newdata=glm.test,type="response")
y.hat.test<-round(glm.pred.test)
glm.test.error<-sum(abs(y.true.test-y.hat.test))/length(y.true.test)
glm.test.error # 0.0877193

# KNN

library(class)
store.error<-c()
for(i in 1:20){
  knn.pred.test<-knn(wdbc.training[,-1],wdbc.test[,-1],wdbc.training[,1],k=i)
  y.hat.test<-as.numeric(knn.pred.test)-1
  knn.test.error<-sum(abs(y.true.test-y.hat.test))/length(y.true.test)
  store.error<-c(store.error,knn.test.error)
}
store.error

n<-which.min(store.error)
store.error[n] #0.02631579
n #6

quartz()
plot(store.error,main="KNN error ",ylab="error",xlab="K",type="o",lty=2,col = "red")
legend("topright",c("error"),lty=1,col="red")



