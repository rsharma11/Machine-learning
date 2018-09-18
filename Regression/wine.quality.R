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
wine <- read.csv("/Users/vimal/Machine-learning/Regression/regression/advanced.csv", header = TRUE)

summary(wine)
table(wine$quality)

ggplot(data =wine, aes(y= quality)) + geom_boxplot()

#Preprocessing 
sum(is.na(wine))

correlationMatrix <- cor(wine[,1:12])
corrplot(correlationMatrix, is.corr = FALSE, method = "circle")

#Data Splitting
set.seed(17)
# Stratified sampling
wine$quality <- as.factor(wine$quality)
inTrain <- createDataPartition(wine$quality, p = 2/3, list = F)
train.wine <- wine[inTrain,]
test.wine <- wine[-inTrain,]


#Model building

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
rf.grid <- expand.grid(mtry = 1:11)
rf.train <- train(quality ~ ., data = train.wine, method = "rf",
                  trControl = t.ctrl, tuneGrid = rf.grid, 
                  preProcess = c("center", "scale"))
plot(rf.train)

#Model building
rf.predict <- predict(rf.train, test.wine)
confusionMatrix(rf.predict, test.wine$quality)
