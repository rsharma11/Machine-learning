library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)

####Loading the data
data <- read.table("/Users/vimal/Machine-learning/crop-data-challenge-2018-cland/TrainingDataSet_Maize.csv", sep = "\t")
test_data <- read.table("/Users/vimal/Machine-learning/crop-data-challenge-2018-cland/TestDataSet_Maize_blind.csv", sep = "\t", header = TRUE)

####Exploratory analysis
summary(data)
names(data)
library(skimr)

y <- data %>% select(yield_anomaly)
x <- data %>% select(-yield_anomaly)
summary(x)
summary(y)
skimmed <- skim_to_wide(data)


####Model buiding and prediction on train data
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
set.seed(3333)

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

c1 <- makeCluster(8, type = "SOCK")
registerDoSNOW(c1)

caret.cv <- train(yield_anomaly ~ year_harvest + NUMD + IRR + ETP_1 + ETP_2 + 
                     ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + 
                     PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + 
                     RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + 
                     SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + 
                     SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + 
                     Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + 
                     Tx_6 + Tx_7 + Tx_8 + Tx_9 , data = data, method = "xgbTree",
                     trControl=trctrl,tuneGrid = tune.grid)
                     

#==================================================================
#Testing model on train & test data
#==================================================================
preds1_train <- predict(caret.cv, x)
preds1_test <- predict(caret.cv, test_data)

#==================================================================
#Estimating effectiveness of the  model
#==================================================================
plot(preds1, data$yield_anomaly)


#### decision tree
c1 <- makeCluster(8, type = "SOCK")
registerDoSNOW(c1)
caret.cv <- train(yield_anomaly ~ year_harvest + NUMD + IRR + ETP_1 + ETP_2 + 
                    ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + 
                    PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + 
                    RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + 
                    SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + 
                    SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + 
                    Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + 
                    Tx_6 + Tx_7 + Tx_8 + Tx_9 , data = data, method = "rpart",
                    trControl=trctrl, tuneLength = 10, 
                    parms=list(split='information'))


#==================================================================
#Testing model on train & test data
#==================================================================
preds1_train <- predict(caret.cv, x)
preds1_test <- predict(caret.cv, test_data)

