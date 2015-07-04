
## Reading data
data<-read.csv("C:/Users/Nikhil/Downloads/machine learning/wisc_bc_data.csv")

## setting labels for factor diagnosis
data$diagnosis<-factor(data$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))

round(prop.table(table(data$diagnosis))*100,digits=1)

summary(data[c("radius_mean","area_mean","smoothness_mean")])

## function to normalise features
normalise<-function(x)
{
  (x-min(x))/(max(x)-min(x))
  
}

data<-data[-1]

##normalising
normaldata<-as.data.frame(lapply(data[2:31],normalise))

str(normaldata)

summary(normaldata$area_mean)

library(caret)


# training dataset using caret package
##diagdata<-cbind(data[,1],normaldata)
##x<-createDataPartition(y=diagdata[,1],p=.82)
##x<-x[[1]]
##tdata<-normaldata[x,]

# training dataset manually:-
traindata<-normaldata[1:469,]
testdata<-normaldata[470:569,]

trainlabels<-data[1:469,1]
testlabels<-data[470:569,1]

library(class)

predlabels<-knn(traindata,testdata,trainlabels,21)

library(gmodels)

CrossTable(testlabels,predlabels,prop.chisq="False")



##z core tranformation

zdata<-as.data.frame(scale(data[-1]))

traindata<-zdata[1:469,]
testdata<-zdata[470:569,]


predzlabels<-knn(traindata,testdata,trainlabels,21)

CrossTable(testlabels,predzlabels,prop.chisq="False")
