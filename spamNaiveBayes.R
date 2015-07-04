rawdata<-read.csv("C:/Users/Nikhil/Downloads/machine learning/sms_spam.csv",stringsAsFactors=FALSE)

library(tm)

sms_corpus<-Corpus(VectorSource(rawdata$text))

as.character(sms_corpus[[1]])

##to lower case
corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
##removing numbers 
corpus_clean<-tm_map(corpus_clean,content_transformer(removeNumbers))
##remving word like or and to
corpus_clean<-tm_map(corpus_clean,content_transformer(removeWords),stopwords())
##remove puntcuation
corpus_clean<-tm_map(corpus_clean,content_transformer(removePunctuation))

##removing white spaces
corpus_clean<-tm_map(corpus_clean,content_transformer(stripWhitespace))

as.character(corpus_clean[[1]])

## creating spase matrix of words
sms_dtm<-DocumentTermMatrix(corpus_clean)


##splittinf training and tets data

raw_train<-rawdata[1:4179,]
raw_test<-rawdata[4180:5574,]

dtm_train<-sms_dtm[1:4179,]
dtm_test<-sms_dtm[4180:5574,]

corpus_train<-corpus_clean[1:4179]
corpus_test<-corpus_clean[4180:5574]

##checking even distribution of data
prop.table(table(raw_test$type))
prop.table(table(raw_train$type))

library(wordcloud)
##making wordlcoud of training data 
wordcloud(corpus_train,min.freq=40,random.order=FALSE,scale=c(3,.4))


spam1<-rawdata[rawdata$type=="spam",]
ham1<-rawdata[rawdata$type=="ham",]

wordcloud(spam1$text,max.words=40,scale=c(3,.4))

wordcloud(ham1$text,max.words=40,scale=c(3,.4))


##dictionary of words with freq>5
sms_train<-DocumentTermMatrix(corpus_train,list(dictionary=findFreqTerms(dtm_train,5)))


sms_test<-DocumentTermMatrix(corpus_test,list(dictionary=findFreqTerms(dtm_train,5)))

##converting numeric features to factors(absence or present of partcular word)
convertToFactor<-function(x)
{
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("No","Yes"))
  
  
}

sms_train<-apply(sms_train,2,convertToFactor)
sms_test<-apply(sms_test,2,convertToFactor)

library(e1071)


model<-naiveBayes(sms_train,raw_train$type,1)

predictions<-predict(model,sms_test)

library(gmodels)

CrossTable(predictions,raw_test$type,prop.chisq=FALSE,prop.t=FALSE,dnn=c("predicted","actual"))
