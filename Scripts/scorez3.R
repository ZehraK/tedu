
library(dummies)
library(tree)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(rattle)

labelm <- dummy.data.frame(labels)

#convert binary data into factor for easy interpretation

labelm <- lapply(labelm, factor)
labelm <- as.data.frame(labelm)


labelz1 <- subset(labelm, select=-c(62,63,64,65,66))
labelz2 <- subset(labelm, select=-c(61,63,64,65,66))
labelz3 <- subset(labelm, select=-c(61,62,64,65,66))
labelz4 <- subset(labelm, select=-c(61,62,63,65,66))
labelz5 <- subset(labelm, select=-c(61,62,63,64,66))
labelz6 <- subset(labelm, select=-c(61,62,63,64,65))


#data partition
set.seed(101)
labelz3$typeZ3 <- as.factor(labelz3$typeZ3)
sample = sample.split(labelz3, SplitRatio = .80)
train = subset(labelz3, sample == TRUE)
test  = subset(labelz3, sample == FALSE)

#model

fit <- rpart(typeZ3 ~ .,
             method="class", data=labelz3)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
rpart.plot(fit)
fancyRpartPlot(fit, palettes = c("Greys", "Oranges"), main="Z3")

#prediction & model evaluation
pred <- predict(fit, test, type = 'class')
#type 1, type 2 error, cross validation
table(pred, test$typeZ3) 

mean(pred == test$typeZ3)

varImp(fit, scale=TRUE)

asRules(fit)

