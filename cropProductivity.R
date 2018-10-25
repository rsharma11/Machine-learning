library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(corrplot)
library(mlbench)
library(e1071)

####Loading the data
train_data <- read.table("/Users/vimal/Machine-learning/crop-data-challenge-2018-cland/TrainingDataSet_Maize.csv", sep = "\t")
test_data <- read.table("/Users/vimal/Machine-learning/crop-data-challenge-2018-cland/TestDataSet_Maize_blind.csv", sep = "\t", header = TRUE)

####Exploratory analysis
summary(train_data)
summary(test_data)

y <- data %>% select(yield_anomaly)
x <- data %>% select(-yield_anomaly)
summary(x)
summary(y)
skimmed <- skim_to_wide(data)

correlation <- cor(train_data)
corrplot(correlation, type = "lower")

highlyCorrelated <- findCorrelation(correlation, cutoff=0.5) ####index of highly correlated features

####Rank Features By Importance
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# estimate variable importance
importance <- varImp(model, scale=FALSE)

# plot importance
plot(importance)

# Feature Selection
# run the RFE algorithm
ctrl <- rfeControl(functions = lmFuncs, method = "cv", verbose = FALSE, returnResamp = "final")
results <- rfe(x[,1:57], y[,1], sizes=c(1:57), rfeControl=ctrl)
predictors(results)
results$fit
plot(results, metric = "Rsquared")

# The top 5 variables (out of 55):
#   ETP_3, SeqPR_5, ETP_8, ETP_4, ETP_7

# list the chosen features
predictors(results)

# plot the results
plot(results, type=c("g", "o"))

# train the model
ctrl<-trainControl(method = "cv", number = 10)
model1 <- train(yield_anomaly ~ ETP_3 + SeqPR_5 + ETP_8 + ETP_4 + ETP_7 , data = data, method="lm", preProcess="scale", trControl = ctrl, metric="Rsquared")

model2 <- train(yield_anomaly ~ year_harvest + NUMD + IRR + ETP_1 + ETP_2 + 
                  ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + 
                  PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + 
                  RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + 
                  SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + 
                  SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + 
                  Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + 
                  Tx_6 + Tx_7 + Tx_8 + Tx_9, data = data, method="glm",  trControl = ctrl)

coef.icept <- coef(model$finalModel)[1]
coef.slope <- coef(model$finalModel)[2]

summary(model)

# Model Diagnostics and Scoring:
residuals<-resid(model2)
predictedValues<-predict(model2)
plot(train_data$yield_anomaly,residuals)
plot(train_data$yield_anomaly,predictedValues)
abline(0.0,0.0)

anova(model, model1) 


# Prediction
predictedVal<-predict(model,test_data)