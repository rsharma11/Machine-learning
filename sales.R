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