#===============================================
#install.packages(c("e1071","caret","doSNOW","xgboost","ipred"))
#Load Data
#===============================================
library(caret)
library(doSNOW)
library(data.table)
library(tibble)
library(dplyr)
library(corrplot)
#===============================================
#Load Data
#===============================================
red.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
red.raw <- read.csv(red.url, header = TRUE, sep = ";")
red <- red.raw
View(red)
str(red)
table(red$quality)

#====================================================
#Checking for missing data
#====================================================
anyNA(red)

#=======================================================
#Linear regression plot of quality wrt other variables
#=======================================================
par(mfrow = c(4,3))
for (i in c(1:11)) {
  plot(red[, i], jitter(red[, "quality"]), xlab = names(red)[i],
       ylab = "quality", col = "firebrick", cex = 0.8, cex.lab = 1.3)
  abline(lm(red[, "quality"] ~ red[ ,i]), lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))

#=======================================================
#Correlation plot between variables
#=======================================================
par(mfrow = c(1,1))
cor.red <- cor(red)
corrplot(cor.red, method="number", type = "upper")

#=======================================================
#Model Building
#=======================================================
set.seed(3033)
red$quality <- as.factor(red$quality)
inTrain <- createDataPartition(red$quality, p = 2/3, list = F)
train.red <- red[inTrain,]
test.red <- red[-inTrain,]
dim(train.red); dim(test.red)

#====================================================
#Split Data: 70:30%
#====================================================
set.seed(54321)
indexes <- createDataPartition(train$quality, 
                               times = 1, 
                               p = 0.7, 
                               list = FALSE)

wine.train <-train[indexes,]
wine.test <-train[-indexes,]

#==================================================================
#Examine the proportion of quality class lable across the datasets
#==================================================================
prop.table(table(train$quality))
prop.table(table(wine.train$quality))
prop.table(table(wine.test$quality))

#==================================================================
#Model Building
#==================================================================
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")

#==================================================================
#Grid search for hyperparameters for xgboost
#==================================================================
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50,75,100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3,0.4,0.5),
                         gamma=0,
                         subsample = 1)
View(tune.grid)

#==================================================================
#Train the data
#==================================================================
c1 <- makeCluster(8, type = "SOCK")
registerDoSNOW(c1)
caret.cv <- train(quality ~ .,
                  data = wine.train,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = train.control)

#==================================================================
#Testing model on test data
#==================================================================
preds <- predict(caret.cv, wine.test)

#==================================================================
#Estimating effictiveness of the  model
#==================================================================
confusionMatrix(preds, wine.test$quality)

