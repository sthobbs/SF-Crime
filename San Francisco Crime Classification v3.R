# Code for the Kaggle competition found here:
# https://www.kaggle.com/c/sf-crime

# Load Packages
library(randomForest)
library(Hmisc) # cut2
library(plyr)
library(dplyr) # mutate
library(xlsx)
library(caret)

# Variables
numTrain <- 5000
numTrain <- nrow(train)
numTrees <- 100

# Set seed
set.seed(154)

# Read in Data
setwd("C:\\Users\\Steve\\Documents\\Programming\\Kaggle\\San Francisco Crime Classification\\")
train<-read.csv("train.csv")
test <- read.csv("test.csv")

## Clean the data

# Discretize X and Y (longitude and latitude) based on quantiles.
# g is the number of breaks
train<-mutate(train,cutX=cut2(X,g=10,digits=10))
train<-mutate(train,cutY=cut2(Y,g=10,digits=10))
train<-train[,-c(8,9)] # remove continous variables
test<-mutate(test,cutX=cut2(X,cuts=as.numeric(substring(levels(train$cutX),15,26)),digits=10))
test<-mutate(test,cutY=cut2(Y,cuts=as.numeric(substring(levels(train$cutY),14,24)),digits=10))
test<-test[,-c(6,7)] # remove continous varaibles


## Inlcude month, year, and hour factor variables
train<-mutate(train,month=as.factor(substring(Dates,6,7)))
train<-mutate(train,year=as.factor(substring(Dates,1,4)))
train<-mutate(train,hour=as.factor(substring(Dates,12,13)))
test<-mutate(test,month=as.factor(substring(Dates,6,7)))
test<-mutate(test,year=as.factor(substring(Dates,1,4)))
test<-mutate(test,hour=as.factor(substring(Dates,12,13)))


# Remove factors not in both training and test sets (and address and dates)
train <- train[,-c(1,3,6,7)]
test <- test[,-c(1,2,5)]

# Splitting the train set into 'training' and 'testing' subsets
inTraining <- sample(1:nrow(train), numTrain)
training <- train[inTraining,]
training$Category<-as.factor(as.character(training$Category)) # remove empty levels
testing <- train[-inTraining,]
training_cat <- training$Category
testing_cat<-testing$Category



### Random Forest
ptm <- proc.time()
fit2 <- randomForest(x=training[,2:8],y=training_cat, ntree=numTrees)
proc.time() - ptm
predictions<-predict(fit2,testing) # predicting testing set
suppressWarnings(confusionMatrix(predictions,testing_cat)$overall[1])

predictions<-predict(fit2,test) # predict test set



###===================
### Neural Networks, possible other fit
ptm <- proc.time()
fit3<-train(Category~.,data=training,method="avNNet",linout = TRUE, trace = FALSE)
proc.time() - ptm
pred3<-predict(fit3,testing)
suppressWarnings(confusionMatrix(pred3,testing$Category)$overall[1])
#levels(pred3)
#levels(testing$Category)
# 1000 => 819   0.2141169
# 2000 => 1666  0.2184022
# 3000 => 2448  0.226689

## possible other fit
ptm <- proc.time()
fit4<-train(Category~.,data=training,method="treebag",linout = TRUE, trace = FALSE)
proc.time() - ptm
pred4<-predict(fit4,testing)
suppressWarnings(confusionMatrix(pred4,testing$Category)$overall[1])
###===================



# Write file for submission
df<-data.frame(Id=0:(nrow(test)-1))
for (i in 1:39){
    df[,i+1]=1*(levels(testing_cat)[predictions]==levels(testing_cat)[i])
}
names(df)=c("Id",levels(training_cat))
write.csv(df, "submission.csv",row.names=FALSE) 

