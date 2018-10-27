####Loading libraries
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(mlbench)
library(Hmisc)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(ggthemes)
library(scales)
library(MLmetrics)

data <- read.csv("/Users/vimal/Machine-learning/Regression/linear/weather/weatherHistory.csv")
data %>% as.tibble()

####Exploratory analysis
str(data)
summary(data)
colnames(data)
head(data)

par(yaxs="i",las=1)
ggplot(data, aes(x = data$Pressure..millibars.)) + geom_histogram() + facet_grid() ### Histogram of all variables
num_data <- 
correlation_weather <- cor(data[4:9])
corrplot(correlation_weather, type = "lower")

####Pre-processing
#Finding NA values
any(is.na(data)) ## Found none

#Centering and scaling numerical columns
preProcValues <- preProcess(data, method = c("BoxCox", "center", "scale", "spatialSign"))
train_processed <- predict(preProcValues, data)
