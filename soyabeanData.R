library(mlbench)
library(caret)
data(Soybean)
str(Soybean)

apply( Soybean, 2, function(x){ sum(is.na(x)) } )
Soybean$has_nans_in_sample = apply( Soybean[,-1], 1, function(x){ sum(is.na(x)) > 0 } )
table( Soybean[, c(1,34) ] )

dataPostPreprocess <- preProcess( Soybean[,-1], method=c("knnImpute"), na.remove=FALSE )
dataAfterImpute <- predict(dataPostPreprocess, Soybean[,-1])
