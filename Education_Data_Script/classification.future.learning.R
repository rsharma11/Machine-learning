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
education <- read.csv("/Users/vimal/Downloads/xAPI-Edu-Data.csv", header = TRUE)
#education <- fread("/Users/vimal/Downloads/xAPI-Edu-Data.csv", header = TRUE)


summary(education) 
ggplot(data =education, aes(x= Class, fill = Class)) + geom_bar()

#Preprocessing 
sum(is.na(education))


correlationMatrix <- cor(education[,10:13])
corrplot(correlationMatrix, is.corr = FALSE, method = "circle")

#Set up factors 
#===============================================
education$gender<- as.factor(education$gender)
education$NationalITy<- as.factor(education$NationalITy)
education$PlaceofBirth<- as.factor(education$PlaceofBirth)
education$StageID<- as.factor(education$StageID)
education$GradeID<- as.factor(education$GradeID)
education$SectionID<- as.factor(education$SectionID)
education$Topic<- as.factor(education$Topic)
education$Semester<- as.factor(education$Semester)
education$Relation<- as.factor(education$Relation)
education$ParentAnsweringSurvey<- as.factor(education$ParentAnsweringSurvey)
education$ParentschoolSatisfaction<- as.factor(education$ParentschoolSatisfaction)


#Data Splitting
set.seed(17)
# Stratified sampling
TrainingDataIndex <- createDataPartition(education$Class, p=0.75, list = FALSE)
# Create Training Data 
trainingData <- education[TrainingDataIndex,]
testData <- education[-TrainingDataIndex,]


TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)


#Model Training
# training model with SVM

SVModel <- train(Class ~ ., data = trainingData,
                 method = "svmPoly",
                 trControl= TrainingParameters,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1),
                 preProcess = c("pca","scale","center"),
                 na.action = na.omit
)

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
caret.cv <- train(Class ~ .,
                  data = trainingData,
                  method = "xgbTree",
                  tuneGrid = tune.grid,
                  trControl = TrainingParameters)



#Prediction with Model
#SVM
SVMPredictions <-predict(SVModel, testData)
# Create confusion matrix
cmSVM <-confusionMatrix(SVMPredictions, testData$Class)
print(cmSVM)

#Prediction with Model
#XGBoost
XGB_Predictions <-predict(caret.cv, testData)
# Create confusion matrix
cmXGB <-confusionMatrix(XGB_Predictions, testData$Class)
print(cmXGB)

