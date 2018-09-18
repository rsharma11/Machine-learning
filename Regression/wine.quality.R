library(ggplot2)
library(lattice)
library(caret)
library(C50)
library(kernlab)
library(mlbench)
library(randomForest)
library(caretEnsemble)
library(MASS)
library(klaR)
library(nnet)
library(data.table)
wine <- read.csv("regression/advanced.csv", header = TRUE)

summary(wine)
ggplot(data =wine, aes(y= quality)) + geom_boxplot()

#Preprocessing 
sum(is.na(wine))

correlationMatrix <- cor(wine[,1:12])
corrplot(correlationMatrix, is.corr = FALSE, method = "circle")

#Data Splitting
set.seed(17)
# Stratified sampling
TrainingDataIndex <- createDataPartition(wine$quality, p=0.75, list = FALSE)
# Create Training Data 
trainingData <- wine[TrainingDataIndex,]
testData <- wine[-TrainingDataIndex,]

TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)

#Model Training

#XGBoost 
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50,75,100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3,0.4,0.5),
                         gamma=0,
                         subsample = 1)
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)
caret.cv <- train(quality ~ .,
                  data = trainingData,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = TrainingParameters)

#Prediction with Model
#XGBoost
XGB_Predictions <-predict(caret.cv, testData)
# Create confusion matrix
cmXGB <-confusionMatrix(XGB_Predictions, testData$quality)
print(cmXGB)