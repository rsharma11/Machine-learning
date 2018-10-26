####Loading libraries
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(corrplot)
library(e1071)
require(ggplot2)
library(tableplot)
library('RANN')
library(gbm)

####Loading data file
data <- read.csv("data.csv")

####Exploratory analysis
str(data)
data <- data %>% select(-id)
data %>% as.tibble()

par(yaxs="i",las=1)
ggplot(data, aes(x = data$texture_mean)) + geom_histogram() + facet_grid() ### Histogram of all variables


####Pre-processing
#Finding NA values
any(is.na(data)) ## Found none

#Centering and scaling numerical columns
preProcValues <- preProcess(data, method = c("BoxCox", "center", "scale", "spatialSign"))
train_processed <- predict(preProcValues, data)

#Converting outcome variable to numeric
train_processed$diagnosis<-ifelse(train_processed$diagnosis=='M',0,1)
str(train_processed)

#Converting the dependent variable back to categorical
train_processed$diagnosis<-as.factor(train_processed$diagnosis)

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_processed$diagnosis, p=0.75, list=FALSE)
trainSet <- train_processed[ index,]
testSet <- train_processed[-index,]

#Checking the structure of trainSet & testSet
str(trainSet)
str(testSet)

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'diagnosis'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
Diagnosis_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)
Diagnosis_Pred_Profile

predictors<-c("perimeter_worst", "concave.points_worst", "area_worst", "radius_worst", "texture_worst")

####Model building
#Gradient boosting
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
model_gbm_all_features<-train(trainSet[,2:31],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)

plot(model_gbm)
plot(model_gbm_all_features)

varImp(object=model_gbm)
plot(varImp(object=model_gbm),main="GBM - Variable Importance")

varImp(object=model_gbm_all_features)
plot(varImp(object=model_gbm_all_features),main="GBM - Variable Importance")

####Predictions
#with selected features
predictions1<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions1)
confusionMatrix(predictions1,testSet[,outcomeName])

#with all features
predictions2<-predict.train(object=model_gbm_all_features,testSet[,predictors],type="raw")
table(predictions2)
confusionMatrix(predictions2,testSet[,outcomeName])

