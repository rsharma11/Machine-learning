## Loading the Library
library(data.table)
library(tibble)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caret)
library(caTools)

## Read the Data
df <- read.csv2(file = "Machine Learning with R/student-mat.csv") %>% as.tibble()

## Summary of the Data
summary(df)

## Cleaning the Data
######## Checking the null value
any(is.na(df))    #No "na" data 
######## Checking the structure of the data (looking for factor data)
str(df)

## Visualizing the Data
######## Num only
num.cols <- sapply(df, is.numeric)
########  Filter
cor.data <- cor(df[,num.cols])
print (cor.data)
########  Ploting Correlation Data
## Using Corrplot
print(corrplot(cor.data, method = 'color'))
## Using Corrgram
corrgram(df)
corrgram(df,order=TRUE, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt)
## Plotting the Grades
ggplot(df, aes(x=G1)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
ggplot(df, aes(x=G2)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'red')
ggplot(df, aes(x=G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'green')

####### with caTools
####### Split Data into Train and Test Set
## Set A Seed
set.seed(101)
## Split the Sample
sample <- sample.split(df$G3, SplitRatio = 0.7)
# 70% of data 
train <- subset(df,sample == TRUE)
# 30% of data
test <- subset(df,sample == FALSE)

## Train and Model Building
model <- lm(G3 ~ ., data = train)

## Run Model

## Interpret the Model
print(summary(model))

## Plotting the residuals
res <- residuals(model)
class(res)
res <- as.data.frame(res)
ggplot(res,aes(res))+geom_histogram(fill='blue',alpha=0.5)