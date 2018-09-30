library(dplyr)
library(caret)
library(tibble)
library(caret)
set.seed(54321)
library(doSNOW)
#Biomarker 
Train <- read.csv("/Users/vimal/Downloads/Biomarker/Train.csv", header = T) %>%  as.tibble()
Val1 <- read.csv("/Users/vimal/Downloads/Biomarker/Val1.csv", header = T) %>%  as.tibble()
Val2 <- read.csv("/Users/vimal/Downloads/Biomarker/Val2.csv", header = T) %>%  as.tibble()


dim(Train)
dim(Val1)
dim(Val2)

par(mar=c(1, 1, 1.5, 1))

p1 <- ggplot(as.data.frame(table(Train$response)), aes(x=Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity")
p2 <- ggplot(as.data.frame(table(Val1$response)), aes(x=Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity")
p3 <- ggplot(as.data.frame(table(Val2$response)), aes(x=Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity")

cowplot::plot_grid(p1, p2, p3, labels = c("Train", "Validation1", "Validation2"), align ="h")


Train_data <- Train %>%  select(-c(id, response, X))
Train_pred <- Train %>%  select(-id, -X)



############ xgbTree
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

rf_fit <- train(response ~., data = Train_pred, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

rf_fit

Val1_pred <- Val1 %>%  select(-id, -response , -X) 
Val2_pred <- Val2 %>%  select(-id, -response , -X) 

val1_predict <- predict(rf_fit, Val1_pred)
val2_predict <- predict(rf_fit, Val2_pred)


confusionMatrix(val1_predict, Val1$response)
confusionMatrix(val2_predict, Val2$response)

Accuracy : 0.7955
Accuracy : 0.7745

############## gbm
set.seed(54321)

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
c1 <- makeCluster(8, type = "SOCK")
registerDoSNOW(c1)
caret.cv <- train(response ~ .,
                  data = Train_pred,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = train.control)

#==================================================================
#Testing model on test data
#==================================================================
preds1 <- predict(caret.cv, Val1_pred)
preds2 <- predict(caret.cv, Val2_pred)
#==================================================================
#Estimating effictiveness of the  model
#==================================================================
confusionMatrix(preds1, Val1$response)
confusionMatrix(preds2, Val2$response)

#Accuracy : 0.803    
#Accuracy : 0.7353


######## SVM

library(e1071)
library(MASS)
library(caTools)
library(MLmetrics)

Train_pred <- Train[,4:length(Train)]
Val1_pred <- Val1[,4:length(Val1)]
Val2_pred <- Val2[,4:length(Val2)]


# SVM_model <- svm(Train_pred, Train$response, probability = TRUE)
# pred_prob_Val1 <- predict(SVM_model, Val1_pred, decision.values = TRUE, probability = FALSE)
# heatmap(attr(pred_prob_Val1, "probabilities"), Colv = NA, col=cm.colors(100))

set.seed(123) 

TrainCtrl1 <- trainControl(method = "repeatedcv", number = 5,savePred=T, repeats=5,verbose = FALSE)
SVMgrid <- expand.grid(sigma = c(0.0577), C = c(2.21049))
modelSvmRRB <- train(Train_pred, Train$response, method = "svmLinear", trControl=TrainCtrl1, tuneLength = 10)

Predicted_Val1 <- predict(modelSvmRRB,Val1_pred)
Predicted_Val2 <- predict(modelSvmRRB,Val2_pred)
confusionMatrix(Predicted_Val1, Val1$response)
confusionMatrix(Predicted_Val2, Val2$response)

Accuracy(Predicted_Val1, Val1$response)
#0.7348485
Accuracy(Predicted_Val2, Val2$response)
#0.7156863





# Random forest ----------
library(randomForest)
library(mlbench)
library(caret)

# Create model 
seed <- 1234
metric <- "Accuracy"

set.seed(seed)
mtry <- sqrt(ncol(Train_pred))
trctrl <- trainControl(method="repeatedcv", number=10, repeats=3)

tunegrid <- expand.grid(.mtry=100)



Train_pred <- Train[,3:length(Train)]
Val1_pred <- Val1[,3:length(Val1)]
Val2_pred <- Val2[,3:length(Val2)]


rf_fit <- train(response ~., data = Train_pred, method = "rf",
                trControl=trctrl,
                tuneGrid = tunegrid,
                tuneLength = 50)




val1_predict <- predict(rf_fit, Val1_pred)
val2_predict <- predict(rf_fit, Val2_pred)


confusionMatrix(val1_predict, Val1$response)
confusionMatrix(val2_predict, Val2$response)

#Accuracy : 0.7803
#Accuracy : 0.7549







#Build the model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(694)) %>%
  layer_dense(units = 2, activation = 'softmax') 

model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = 'accuracy'
)

trainLabels <- array(Train$response)


colnames(Train_data) <- NULL
history <- model %>% fit(as.array((Train_data)), trainLabels, 
                         epochs = 10)

#-------------------