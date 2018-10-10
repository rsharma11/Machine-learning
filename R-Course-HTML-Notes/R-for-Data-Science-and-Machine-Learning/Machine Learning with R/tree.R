## Decision tree
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)

str(kyphosis)
tree <- rpart(Kyphosis ~ .,method = 'class', data = kyphosis)
printcp(tree)
plot(tree, uniform = T, main = 'Kyphosis Tree')
text(tree, use.n = T, all = T)
prp(tree)

## Random Forest


