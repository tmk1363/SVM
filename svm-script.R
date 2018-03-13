data<-read.csv("formability.csv",FALSE, ",")
str(data)
data$V1<-as.factor(data$V1)
str(data)
library(ggplot2)
qplot(V2,V3,data=data,color=V1)
library(e1071)
mymodel<-svm(V1~.,data=data)
summary(mymodel)
plot(mymodel,data=data, V3~V2)
#prediction using the model we built
pred<-predict(mymodel,data)
tab<-table(Predicted=pred,Actual=data$V1)
tab
sum(diag(tab)/sum(tab))
#linear Kernel
mymodel<-svm(V1~.,data=data, kernel="linear")
summary(mymodel)
plot(mymodel,data=data, V3~V2)
#prediction using the model we built
pred<-predict(mymodel,data)
tab<-table(Predicted=pred,Actual=data$V1)
tab
sum(diag(tab)/sum(tab))
#polynomial kernel
mymodel<-svm(V1~.,data=data, kernel="polynomial")
summary(mymodel)
plot(mymodel,data=data, V3~V2)
#prediction using the model we built
pred<-predict(mymodel,data)
tab<-table(Predicted=pred,Actual=data$V1)
tab
sum(diag(tab)/sum(tab))
#sigmoid kernel
mymodel<-svm(V1~.,data=data, kernel="sigmoid")
summary(mymodel)
plot(mymodel,data=data, V3~V2)
#prediction using the model we built
pred<-predict(mymodel,data)
tab<-table(Predicted=pred,Actual=data$V1)
tab
sum(diag(tab)/sum(tab))
#radial basis function kernel
mymodel<-svm(V1~.,data=data)
summary(mymodel)
plot(mymodel,data=data, V3~V2)
#prediction using the model we built
pred<-predict(mymodel,data)
tab<-table(Predicted=pred,Actual=data$V1)
tab
sum(diag(tab)/sum(tab))
#fine tune the model
set.seed(123)
tmodel<-tune(svm,V1~.,data=data,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel)
summary(tmodel)
#choosing the best model for classification
mymodel<-tmodel$best.model
summary(mymodel)
plot(mymodel,data=data,V3~V2)
#confusion matrix
pred<-predict(mymodel,data)
tab<-table(Predicted=pred,Actual=data$V1)
tab
accuracy<-sum(diag(tab)/sum(tab))
accuracy
#ENd

