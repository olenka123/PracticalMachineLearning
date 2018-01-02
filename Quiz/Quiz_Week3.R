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

# Load the data:
data(segmentationOriginal)

# Subset the data to a training set and testing set based on the Case variable in the data set: 

inTrain <- segmentationOriginal$Case == "Train"

training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

dim(training)
dim(testing)

# Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings:
set.seed(125)
modelFit <- train(Class~., method="rpart", data=training)
print(modelFit$finalModel)

#  In the final model what would be the final model prediction for cases with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

# Solution: Option 4(a. PS, b. WS, c.PS, d. Not possible to predict)

# fancyRpartPlot(modelFit$finalModel)
plot(modelFit$finalModel, uniform=T, main="Classification Tree")
text(modelFit$finalModel, use.n=T, all=T, cex=0.7)

# Alternative prediction
train <-rpart(Class~.,data=training)

test<-segmentationOriginal[0,]
test[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")]<-c(23000, 10, 2)
test[2,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)
test[3,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(57000, 8, 100)
test[4,c("FiberWidthCh1", "VarIntenCh4", "PerimStatusCh1")]<-c(8, 100, 2)
predict(train, newdata=test)

# Problem 2.
# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? 
# If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 
# Is K large or small in leave one out cross validation? 

# Solution: Option 4
# The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

# Problem 3.
# Load the olive oil data using the commands:
library(pgmm)
data(olive)
str(olive)
olive<-olive[,-1]
summary(olive)

# These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
# Fit a classification tree where Area is the outcome variable. 
# Then predict the value of area for the following data frame using the tree command with all defaults.

modelFit<-train(Area~., method="rpart", data=olive)
print(modelFit$finalModel)

newdata <- as.data.frame(t(colMeans(olive)))
predict(modelFit,newdata)

# Solution: Option 1. 
# 2.873. It is strange because Area should be a qualitative 
# variable - but tree is reporting the average value of Area as 
# a numeric variable in the leaf predicted for newdata

# Problem 4.
# Load the South Africa Heart Disease Data and create training and test sets with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train <- sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA <- SAheart[train,]
testSA = SAheart[-train,]

head(trainSA)

# Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") 
# with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, 
# cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors.
set.seed(1234)
modelFit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, data=trainSA, method="glm", family="binomial")
print(modelFit$finalModel)

# Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

# What is the misclassification rate on the training set? 
predictions<-predict(modelFit,trainSA)
missClass(trainSA$chd,predictions) #0.2727273

# What is the misclassification rate on the test set? 
predictions<-predict(modelFit,testSA)
missClass(testSA$chd,predictions) #0.3116883

# Solution: Option 2.

# Problem 5
# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. 
# Fit a random forest predictor relating the factor variable y to the remaining variables. 
# Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
# The caret package uses by default the Gini importance. 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
modelFit<-randomForest(y~., data=vowel.train)
print(modelFit)

#Calculate the variable importance using the varImp function in the caret package. 
# What is the order of variable importance?
order(varImp(modelFit),decreasing=T)
varImp(modelFit)

# Solution: Option 4.

