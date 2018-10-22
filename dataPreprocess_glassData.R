library(mlbench)
library(ggplot2)
library(corrplot)
library(corrgram)
library(reshape2)
library(e1071)

data(Glass)
str(Glass)
attach(Glass)
glassData <- Glass

##### Distribution and relationship between predictors
names<-names(glassData)
classes<-sapply(glassData,class)

ggplot(melt(glassData),aes(x=value)) + geom_histogram() + facet_wrap(~variable) + xlim(0,20) 

corrMat <- cor(Glass[, -10])
corrplot(corr = corrMat, type = "lower")

##### Outliers detection
boxplot(glassData)

##### Skewness detection
apply(glassData[,-10], 2, skewness)

par(mfrow=c(1,9))
hist( Glass$Ri ) 
hist( Glass$Na ) 
hist( Glass$Mg ) # Looks multimodal
hist( Glass$Al )
hist( Glass$Si )
hist( Glass$K ) # Looks like a data error in that we have only two  samples with a very large K value 
hist( Glass$Ca )
hist( Glass$Ba ) # Looks like a skewed distribution
hist( Glass$Fe )
par(mfrow=c(3,3))
