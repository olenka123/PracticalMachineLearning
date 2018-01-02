##### Quiz Week 2 #####
#######################

# Problem 1.
# Load the Alzheimer's disease data using the commands:

data(AlzheimerDisease)
adData <- data.frame(diagnosis,predictors)

# Which of the following commands will create non-overlapping training
# and test sets with about 50% of the observations assigned to each? 

# Solution: Option 2.
trainIndex <- createDataPartition(diagnosis,p=0.5,list=FALSE)
training <- adData[trainIndex,]
testing <- adData[-trainIndex,]
dim(training)
dim(testing)

# Problem 2.
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
library(Hmisc)
library(caret)
library(ggplot2)
library(gridExtra)

data(concrete)
summary(mixtures)
set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training <- mixtures[ inTrain,]
testing <- mixtures[-inTrain,]
dim(training)
dim(testing)

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function in the Hmisc package 
# useful for turning continuous covariates into factors). What do you notice in these plots?

# Solution: Option 2
plot(training$CompressiveStrength)
names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x=training[,names], y=training$CompressiveStrength, plot="pairs")
# There appears to be no relation between the outcome variable and predictors

index <- seq_along(1:nrow(training))
qplot(x=index,y=CompressiveStrength, data=training, geom=c("point"))
# There seem to be four categories/clusters affecting the trend

CompressiveStrength_cut <- cut2(training$CompressiveStrength, g=4)
table(CompressiveStrength_cut)
qp1 <- qplot(CompressiveStrength_cut,index, data=training, fill=CompressiveStrength_cut, geom=c("boxplot"))
qp2 <- qplot(CompressiveStrength_cut,index, data=training, fill=CompressiveStrength_cut, geom=c("boxplot", "jitter"))
print(qp1)
print(qp2)
grid.arrange(qp1,qp2,ncol=2)

tab <- table(CompressiveStrength_cut,index)
print(tab)

# There is a non-random pattern in the plot of the outcome versus index that does not appear 
# to be perfectly explained by any predictor suggesting a variable may be missing.


# Problem 3.
# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. 
# Why would that be a poor choice for this variable?

# Solution: Option 4.

hist(training$Superplasticizer, breaks=50)
Superlasticizer_log <- log(training$Superplasticizer+1)
hist(Superlasticizer_log, breakes=50)
qplot(Superlasticizer_log, geom=c("histogram"))

# There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1) 
# they would still all be identical so the distribution would not be symmetric.

# Problem 4.
# Load the Alzheimer's disease data using the commands:
# library(caret)
# library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData <- data.frame(diagnosis,predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[ inTrain,]
testing <- adData[-inTrain,]

#Solution: Option 3

# Find all the predictor variables in the training set that begin with IL. 
vars_IL <- grep("^IL", names(training))
print(vars_IL)
names(training[,vars_IL])
# Perform principal components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 90% of the variance. 
# How many are there?
prComp <- preProcess(training[,vars_IL], method="pca", thresh = 0.9)
prComp
prComp$numComp #9

# Problem 5
# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis.
set.seed(3433)
data(AlzheimerDisease)
vars_IL <-grep("^IL", names(predictors))
names(predictors[,vars_IL])
adData <- data.frame(diagnosis,predictors[,vars_IL])

inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[ inTrain,]
testing <- adData[-inTrain,]
dim(training)
dim(testing)

# Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% 
# of the variance in the predictors. Use method="glm" in the train function.
# What is the accuracy of each method in the test set? Which is more accurate?
# Solution: Option 3.
modelFit1 <- train(diagnosis~., method ="glm", data=training)
predictions <- predict(modelFit1, newdata=testing)
confusionMatrix(predictions, testing$diagnosis)$overall[1]

modelFit2 <- train(diagnosis~., method ="glm", preProcess="pca", data= training,
                   trControl=trainControl(preProcOptions=list(thresh = 0.8)))
predictions <- predict(modelFit2, newdata=testing)
confusionMatrix(predictions, testing$diagnosis)$overall[1]
                  
