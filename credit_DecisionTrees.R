credit<-read.csv("C:/Users/Nikhil/Downloads/machine learning/credit.csv")

credit$default<-factor(credit$default,labels=c("no","yes"))

table(credit$checking_balance)


set.seed(1203)
credit_random<-credit[order(runif(1000)),]

credit_train<-credit_random[1:900,]
credit_test<-credit_random[901:1000,]



prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

library(C50)

credit_model<-C5.0(credit_train[,-21],credit_train$default)

credit_model
summary(credit_model)

credit_pred<-predict(credit_model,credit_test)

library(gmodels)

CrossTable(credit_test$default,credit_pred,prop.chisq=FALSE,propc.c=FALSE,prop.r=FALSE,
           dnn=c("actual default","predict default"))

##adaptive boosting

credit_modelboost<-C5.0(credit_train[,-21],credit_train$default,trials=10)


##using cost matrix to supress one type of error against other type
## rows indicate predicted class,column indicate actual class..we have
##weighed false negative error 4 times as unwanted as false positive.
cost_matrix<-matrix(c(0,1,4,0),nrow=2)

credit_model_cost<-C5.0(credit_train[,-21],credit_train$default,costs=cost_matrix)
credit_cost_predict<-predict(credit_model_cost,credit_test)

CrossTable(credit_test$default,credit_cost_predict,prop.chisq=FALSE,propc.c=FALSE,prop.r=FALSE,
           dnn=c("actual default","predict default"))


