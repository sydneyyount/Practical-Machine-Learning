## Reading in Training/Test data sets 
trainingData <- read.csv('pml-training.csv')
testingData <- read.csv('pml-testing.csv')

## Loading in caret, randomForest, ggplot2 packages
install.packages("caret")
install.packages("ggplot2")
library(caret)
library(randomForest)

## Removing columns with NA values
trainData <-trainingData[,colSums(is.na(trainingData)) == 0]
testData <- testingData[,colSums(is.na(testingData))==0]

## Removing columns with non useful data

trainData <- trainData[,-c(1:7)]
testData <- testData[,-c(1:7)]

## Further cleaning training data to remove zero variance predictors
ind <- nearZeroVar(trainData)
trainData <- trainData[,-ind]

## Partitioning variables
inTrain <- createDataPartition(y=trainData$classe,p=0.7,list=FALSE)
trainSet <- trainData[inTrain,]
validateSet <- trainData[-inTrain,]

## Setting a seed for reproducibility
set.seed(223)

## fitting Random Forest model to data set
RFmod <- train(classe~.,data=trainSet,method="rf")

## Predicting with Random Forest Model
RFPred <- predict(RFmod,validateSet)

## Creating confusion matrix to summarize prediciton results of RF model
cm <- confusionMatrix(RFPred,factor(validateSet$classe))

## Printing confusion matrix
cm

## plotting model
plot(RFmod)

