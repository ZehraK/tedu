#labels for cross-valid and model
#labelling data for the confusion matrix/external cluster validation

labels <- veri

Z1 <- veri[c(9,24,31,39,41,43,45,50,53,60)]
Z2 <- veri[c(3,12,15,21,23,28,32,35,49,57)]
Z3 <- veri[c(1,8,10,16,17,22,27,34,40,47)]
Z4 <- veri[c(5,7,33,36,38,52,54,55,58,59)]
Z5 <- veri[c(2,4,11,14,18,19,26,30,37,44)]
Z6 <- veri[c(6,13,20,25,29,42,46,48,51,56)]


#Compute the dimension score for each obs

labels$Z1 <- rowSums(Z1)
labels$Z2 <- rowSums(Z2)
labels$Z3 <- rowSums(Z3)
labels$Z4 <- rowSums(Z4)
labels$Z5 <- rowSums(Z5)
labels$Z6 <- rowSums(Z6)

z_scores <- labels[c(61,62,63,64,65,66)]

cluster_labels <- colnames(z_scores)[max.col(z_scores,ties.method="random")]
labels$type <- cluster_labels

labels <- subset(labels, select= -c(61,62,63,64,65,66))
labels$type <- as.factor(labels$type)
###

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
labelz6$typeZ6 <- as.factor(labelz6$typeZ6)
sample = sample.split(labelz6, SplitRatio = .80)
train = subset(labelz6, sample == TRUE)
test  = subset(labelz6, sample == FALSE)

#model

fit <- rpart(typeZ6 ~ .,
             method="class", data=train, control = rpart.control(maxdepth = 20, minsplit = 5 ) )
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
rpart.plot(fit)
fancyRpartPlot(fit, palettes = c("Greys", "Oranges"), main="Z6")

#prediction & model evaluation
pred <- predict(fit, test, type = 'class')
#type 1, type 2 error, cross validation
table(pred, test$typeZ6) 

mean(pred == test$typeZ6)

varImp(fit, scale=TRUE)

asRules(fit)

Z6 <- veri[c(6,13,20,25,29,42,46,48,51,56)]
