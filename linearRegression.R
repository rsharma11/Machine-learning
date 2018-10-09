## Loading the library
library(data.table)
library(tibble)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)

## Read the data
df <- read.csv2(file = "Machine Learning with R/student-mat.csv") %>% as.tibble()

## Summary of the data
summary(df)

## Cleaning the data
######## Checking the null value
any(is.na(df))    #No "na" data 
######## Checking the structure of the data (looking for factor data)
str(df)

## Visualizing the data
######## Num only
num.cols <- sapply(df, is.numeric)
########  filter
cor.data <- cor(df[,num.cols])
print (cor.data)
########  Ploting correlation data
## Using corrplot
print(corrplot(cor.data, method = 'color'))
## Using corrgram
corrgram(df)
corrgram(df,order=TRUE, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt)
