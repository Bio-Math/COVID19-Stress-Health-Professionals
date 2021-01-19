#################################################
## Exploration of AdaBoost Algortithm with dataC
#################################################

#install.packages(c("adabag","caret"))
library(adabag)
library(caret)
# Reading data
dataC <- read.csv(file = "Data/class.csv", header = TRUE, sep = ",")

# Creating model
cvmodel <- boosting.cv(Class~., data = dataC, boos = TRUE, mfinal = 50, par = TRUE, control = c(minsplit = 5))
modelTest <- boosting(Class~., data = dataC, boos = TRUE, mfinal = 50, par = TRUE, control = c(minsplit = 5))
importanceplot(modelTest)

library(rpart)
data(iris)
model <- boosting(Species~., data = iris, boos = TRUE, mfinal = 10)

importanceplot(model)

str(iris)

str(dataC)

table(dataC$Class)
#################################################
## Exploration of XGBoost Algortithm with dataC
#################################################

