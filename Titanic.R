library(caret)
library(doSNOW)

#===============================================
#Load Data
#===============================================
train <- read.csv("all/train.csv")
View(train)

#===============================================
#Replace missing embarked values with mode
#===============================================
table(train$Embarked)
train$Embarked[train$Embarked==""] <- "S"

#===============================================
#Handeling missing age data
#===============================================
summary(train$Age)
train$MissingAge <- ifelse(is.na(train$Age),"Y","N")

#===============================================
#Add feature for family size
#===============================================
train$FamilySize <- 1+train$SibSp + train$Parch

#===============================================
#Set up factors 
#===============================================
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$MissingAge <- as.factor(train$MissingAge)

#====================================================
#Subsetting the data to keep the important features 
#====================================================
features <- c("Survived", "Pclass", "Sex", "Age", "SibSp", 
              "Parch", "Fare", "Embarked", "MissingAge",
              "FamilySize")
train <- train[,features]
str(train)

#====================================================
#Subsetting the data to keep the important features 
#====================================================
dummy.vars <- dummyVars(~., data = train[, -1])
train.dummy <- predict(dummy.vars, train[, -1])
View(train.dummy)

#====================================================
#Imputation 
#====================================================
pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.data <- predict(pre.process, train.dummy)
View(imputed.data)

train$Age <- imputed.data[,6]
View(train)

#====================================================
#Split Data: 70:30%
#====================================================
set.seed(54321)
indexes <- createDataPartition(train$Survived, 
                               times = 1, 
                               p = 0.7, 
                               list = FALSE)

titanic.train <-train[indexes,]
titanic.test <-train[-indexes,]

#==================================================================
#Examine the proportion of Survival class lable across the datasets
#==================================================================
prop.table(table(train$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))

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
caret.cv <- train(Survived ~ .,
                  data = titanic.train,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = train.control)

#==================================================================
#Testing model on test data
#==================================================================
preds <- predict(caret.cv, titanic.test)

#==================================================================
#Estimating effictiveness of the  model
#==================================================================
confusionMatrix(preds, titanic.test$Survived)
