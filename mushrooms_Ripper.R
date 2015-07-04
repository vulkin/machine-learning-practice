raw<-read.csv("C:/Users/Nikhil/Downloads/machine learning/mushrooms.csv",stringsAsFactors=TRUE)

raw<-raw[,-17]

table(raw$type)

library(RWeka)

##OneR algo

mushrooms_1r<-OneR(type~.,raw)
summary(mushrooms_1r)

##Ripper Algo
mushrooms_ripper<-JRip(type~.,raw)

mushrooms_ripper