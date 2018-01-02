##### Quiz Week 4 #####
#######################

# Problem 1.
# For this quiz we will be using several R packages. R package versions change over time, 
# the right answers have been checked using the following versions of the packages. 

library(AppliedPredictiveModeling)
library(Hmisc)
library(caret)
library(ggplot2)
library(gridExtra)
library(pgmm)
library(rpart)
library(ElemStatLearn)
library(rattle)
library(gbm)
library(lubridate)

# Load the vowel.train and vowel.test data sets:
data(vowel.train)
data(vowel.test)

str(vowel.train)
str(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. 
# Fit (1) a random forest predictor relating the factor variable y to the remaining variables 
# and (2) a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package. 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
modelFitRF <- train(y~.,data=vowel.train, method="rf")
modelFit1$finalModel
predictionRF <- predict(modelFit1,vowel.test)
confusionMatrix(predictionRF,vowel.test$y)$overall #0.5930736 
sqrt(sum((as.numeric(predictionRF)-as.numeric(vowel.test$y))^2))

modelFitGBM <- train(y~.,data=vowel.train, method="gbm", verbose=F)
modelFitGBM$finalModel
predictionGBM <- predict(modelFitGBM, vowel.test)
confusionMatrix(predictionGBM, vowel.test$y)$overall #0.5281385 
sqrt(sum((as.numeric(predictionGBM)-as.numeric(vowel.test$y))^2))

# What are the accuracies for the two approaches on the test data set? 
# What is the accuracy among the test set samples where the two methods agree? 
predictions <- data.frame(predictionRF, predictionGBM, y=vowel.test$y,
                          agree=predictionRF == predictionGBM)
head(predictions)
sum(predictions$agree==T)/length(predictions$agree) #0.6645022

# Solution: Option 1 (?).

# Problem 2.
# Load the Alzheimer's data using the following commands

set.seed(3433)
data(AlzheimerDisease)

adData <- data.frame(diagnosis,predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[ inTrain,]
testing <- adData[-inTrain,]

dim(training); dim(testing)

# Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm")
# and linear discriminant analysis ("lda") model. Stack the predictions together using random forests ("rf"). 
# What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions? 
set.seed(62433)
modelFitRF <- train(diagnosis~., method="rf", data=training)
modelFitGBM <- train(diagnosis~., method="gbm", data=training, verbose=F)
modelFitLDA <- train(diagnosis~., method="lda", data=training)

predRF <- predict(modelFitRF,testing)
confusionMatrix(predRF,testing$diagnosis)$overall[1]#0.7804878 

predGBM <- predict(modelFitGBM,testing)
confusionMatrix(predGBM,testing$diagnosis)$overall[1]#0.804878 

predLDA <- predict(modelFitLDA, testing)
confusionMatrix(predLDA,testing$diagnosis)$overall[1]#0.7682927 

predDF <- data.frame(pred1=predRF, pred2=predGBM, pred3=predLDA, diagnosis=testing$diagnosis)
combModelFit <- train(diagnosis~., method="rf", data=predDF)
combPred <- predict(combModelFit,predDF)
confusionMatrix(combPred,testing$diagnosis)$overall[1]#0.804878 

# Solution: Option 3.
# Stacked Accuracy: 0.80 is better than random forests and lda 
# and the same as boosting.

# Problem 3
# Load the concrete data with the commands:
library(elasticnet)
set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
str(training)

# Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
# Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet). 
set.seed(233)
modelFit <- train(CompressiveStrength~., data=training, method="lasso")
modelFit$finalModel
plot.enet(modelFit$finalModel,xvar="penalty", use.color=T)

# Solution: Option 3. Cement

# Problem 4:
# Load the data on the number of visitors to the instructors blog from here: 
# https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv
library(lubridate)
library(forecast)

# install.packages("forecast", lib = "C:/Program Files/R/R-3.4.3/library")

data <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"))
training <- data[year(data$date) < 2012,]
testing <- data[(year(data$date)) > 2011,]
tstrain <- ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training time series. 
# Then forecast this model for the remaining time points. 
# For how many of the testing points is the true value within the 95% prediction interval bounds? 

modelFit <- bats(tstrain)
modelFit

prediction <- forecast(modelFit, h=dim(testing)[1], level = 95)
predictionCF <- sum(testing$visitsTumblr > prediction$lower & testing$visitsTumblr < prediction$upper)
predictionCF

predictionCF/dim(testing)[1] # 0.9617021

# Solution: Option 2.

# Problem 5.
# Load the concrete data with the commands:

library(e1071)
set.seed(352)
data(concrete)
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]

# Set the seed to 325 and fit a support vector machine using the e1071 package to predict 
# Compressive Strength using the default settings. Predict on the testing set. What is the RMSE? 
set.seed(325)
modelFit <- svm(CompressiveStrength~., data=training)
prediction <- predict(modelFit,testing)
accuracy(prediction,testing$CompressiveStrength)
# 
