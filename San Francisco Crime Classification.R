library(randomForest)
library(Hmisc) # cut2
library(plyr)
library(dplyr) # mutate
library(xlsx)
library(caret)
#library(readr)

set.seed(154)
#options(stringsAsFactors = T)

setwd("C:\\Users\\Steve\\Documents\\Programming\\Kaggle\\San Francisco Crime Classification\\")
train<-read.csv("train.csv")
test <- read.csv("test.csv")

numTrain <- 3000
numTrain <- nrow(train)
numTrees <- 100

# Discretize X and Y (longtude and latitude) based on quantiles.
# g is the number of breaks
train<-mutate(train,cutX=cut2(X,g=10,digits=10))
train<-mutate(train,cutY=cut2(Y,g=10,digits=10))
train<-train[,-c(8,9)] # remove continous variables
test<-mutate(test,cutX=cut2(X,cuts=as.numeric(substring(levels(train$cutX),15,26)),digits=10))
test<-mutate(test,cutY=cut2(Y,cuts=as.numeric(substring(levels(train$cutY),14,24)),digits=10))
test<-test[,-c(6,7)] # remove continous varaibles

#a<-cut2(test$X,cuts=as.numeric(substring(levels(train$cutX),15,26)),digits=10)
#levels(a)
#levels(train$cutX)

#help(as.numeric)
#as.numeric(substring(levels(train$cutX),15,26))
#help(cut2)
### trying equally spaced cuts, not based on quantile
#train<-mutate(train,cutX=cut2(X,cuts=seq(min(X),max(X),(max(X)-min(X))/10),digits=10))
#train<-mutate(train,cutY=cut2(Y,cuts=seq(min(X),max(X),(max(X)-min(X))/10),digits=10))
#train<-train[,-c(8,9)]
# not better

## Inlcude month, year, and hour factor variables
train<-mutate(train,month=as.factor(substring(Dates,6,7)))
train<-mutate(train,year=as.factor(substring(Dates,1,4)))
train<-mutate(train,hour=as.factor(substring(Dates,12,13)))
test<-mutate(test,month=as.factor(substring(Dates,6,7)))
test<-mutate(test,year=as.factor(substring(Dates,1,4)))
test<-mutate(test,hour=as.factor(substring(Dates,12,13)))


# remove factors not in both training and test sets (and address and dates)
#train <- train[,-c(2,3,6,7)]
#test <- test[,-c(1,5)]
train <- train[,-c(1,3,6,7)] # predicting variable 2 (Category)
test <- test[,-c(1,2,5)]

# splitting the 
inTraining <- sample(1:nrow(train), numTrain)
training <- train[inTraining,]
training$Category<-as.factor(as.character(training$Category)) # remove empty levels
testing <- train[-inTraining,]
labels <- training[,1]
true.values<-testing[,1]


#str(training)
#str(train)

ptm <- proc.time()
fit1<-train(Category~.,data=training[1:5],method="rf",trControl=trainControl(repeats=5))
proc.time() - ptm
pred1<-predict(fit1,testing)
confusionMatrix(pred1,testing$Category)$overall[1]

help(train)
head(train)
#for (i in 1:39){
#    print(sum(training$Category==levels(training$Category)[i]))
#}


head(test)
head(train)
head(training)
head(testing)


### Random Forest
ptm <- proc.time()
fit1 <- randomForest(x=training[,2:8],y=labels, xtest=testing[,2:8], ntree=numTrees)
fit2 <- randomForest(x=training[,2:8],y=labels, ntree=numTrees)
proc.time() - ptm
predictions <- data.frame(ImageId=1:nrow(testing), Label=levels(labels)[fit1$test$predicted])
accuracy<-sum(as.character(true.values)==as.character(predictions[,2]))/length(true.values)
accuracy # 0.2731

help(randomForest)

str(fit1$test)
pred1<-predict(fit2,testing)
confusionMatrix(pred1,true.values)$overall[1]


### 
ptm <- proc.time()
fit1<-train(Category~.,data=training,method="avNNet",linout = TRUE, trace = FALSE)
proc.time() - ptm
pred1<-predict(fit1,testing)
confusionMatrix(pred1,testing$Category)$overall[1]
levels(pred1)
levels(testing$Category)


warnings()

rf <- randomForest(train[,2:8], labels, xtest=test[,2:8], ntree=numTrees)
#predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])



#sum(rowSums(df[,2:40])==1)
#head(df)
help(randomForest)

#df<-data.frame(true.values,predictions)
#head(df,n=100)
#str(predictions)

df<-data.frame(Id=0:(nrow(test)-1))
for (i in 1:39){
    df[,i+1]=1*(levels(labels)[fit1$test$predicted]==levels(labels)[i])
}
names(df)=c("Id",levels(labels))


write.csv(df, "submission.csv",row.names=FALSE) 


