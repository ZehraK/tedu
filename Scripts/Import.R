#libraries
library(Amelia)

#import the dataset
veri <- read.csv(file = "dataset.csv", head = TRUE, sep=";")
veri <- as.data.frame(veri)

#descriptive statistics
summary(veri)

#rename the first column
##names(veri)[names(veri) == '?fade'] <- 'observation number'
veri <- subset(veri, select = -1)

#missing values
sapply(veri, function(x) sum(is.na(x)))
#all the missings are common or not?
missmap(veri)

#remove rows with NA
veri <- na.omit(veri)

ncol(veri)
nrow(veri)
