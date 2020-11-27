# Install mltools 

#install.packages("mltools")

#load library
# library(caret)
# library(PerformanceAnalytics)
# library(pROC)
# library(mltools)


# dataset variables
DATA_FILE <- "PreprocessedTelco.csv"
KEY_COLUMN <- "ChurnYes"

# Learning model variables
MODEL_METHOD <- "rf"
TRAINING_PARAM <- ChurnYes ~.
TUNEL_LENGTH <- 5

# Cross validation variables
FOLD_NUMBER <- 2
REPEATS <- 1
P_VALUE <- .75
VALIDATION_METHOD <- "repeatedcv"
CONFUSION_MATRIX_POSITIVE_CLASS <- "1"


#Read dataset
readDataset <- function(datafile, keyColumn){
  dataset<-read.csv(datafile, encoding="UTF-8", stringsAsFactors = FALSE)
  dataset[,keyColumn] = as.factor(dataset[,keyColumn])
  return(dataset)
}


# Prepare training data
separateTrainingSet <- function(dataset, pValue, keyColumn){
  trainingSet <- createDataPartition(dataset[,keyColumn],p= pValue, list = FALSE)
  return(trainingSet)
}

# Return training data
getTrainingData <- function(dataset, trainSet){
  return (dataset[trainSet,])
}

# Return testing data
getTestingData <- function(dataset, trainSet){
  return (dataset[-trainSet,])
}

#10 fold cross validation
createCrossValidation <- function(validationMethod, numbers, repeats){
  return (trainControl(method = validationMethod ,number = numbers, repeats = repeats))
}

generateRandomForestModel <- function(trainingParam, data, modelMethod, tunelLength, kControl){
  return(train(trainingParam, data=data, method=modelMethod, tuneLength=tunelLength, trControl=kControl))
}


createClassProbabilityPrediction <- function(model, dataset){
  #Predict class probabilities against the original dataset
  prediction <- predict(model, dataset, "prob")
  return(prediction)
}

# generate confusion matrix
creatConfusionMatrix <- function(model, testingData, keyColumn, positiveCLass){
  prediction<- predict(model,newdata=testingData)
  confusionMatrix<- caret::confusionMatrix(data=prediction, reference = testingData[,keyColumn], positive = positiveCLass)
  return(confusionMatrix)
}

# Evaluate model

# ROC

createRoc <- function(model, testingData, predictor){
  
  pred <- predict(model, type = "prob", newdata = testingData)
  rocObj <- roc(testingData[,predictor],pred[,2],plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, main=paste("Random Forest"))
  bestThreshold <- round(coords(rocObj, "best", "threshold",transpose = TRUE)[1],digits = 4)
  
  return(bestThreshold)
}

# MCC

createMcc <- function(model, testingData, confusionMat) {
  
  pred <- predict(model, newdata = testingData)
  TP<-confusionMat$table[1]
  FN<-confusionMat$table[2]
  FP<-confusionMat$table[3]
  TN<-confusionMat$table[4]
  
  mccResult <- mcc(preds = NULL, actuals = NULL, TP=TP, FP=FP, TN=TN, FN=FN)

  return(mccResult)
}

buildRandomForest <- function(){
  
  dataset <- readDataset(DATA_FILE, KEY_COLUMN)
  trainSet <- separateTrainingSet(dataset, P_VALUE, KEY_COLUMN)
  trainingData <- getTrainingData(dataset, trainSet)
  testingData <- getTestingData(dataset, trainSet)
  kControl <- createCrossValidation(VALIDATION_METHOD, FOLD_NUMBER, REPEATS)
  
  model <- generateRandomForestModel(TRAINING_PARAM, trainingData, MODEL_METHOD, TUNEL_LENGTH, kControl)
  classProbabilityResult <- createClassProbabilityPrediction(model, dataset)
  confusionMatrix <- creatConfusionMatrix(model, trainingData, KEY_COLUMN, CONFUSION_MATRIX_POSITIVE_CLASS)

  
  bestThreshold <- createRoc(model, testingData, KEY_COLUMN)
  mcc <- createMcc(model, testingData,confusionMatrix)
  
  
  sink(file = paste("randomF.txt"), append = T)
  
  # Print model result
  cat("Model Result")
  cat("\n")
  model$result
  
  cat("\n")
  cat("\n")
  
  #Print class probability
  cat("Class Probability Results")
  cat("\n")
  head(classProbabilityResult)
  cat("\n")
  cat("\n")
  
  
  #Print confusion matrix
  cat("Confusion Matrix")
  cat("\n")
  confusionMatrix
  cat("\n")
  cat("\n")
  
  cat("Threshold")
  bestThreshold
  cat("\n")
  cat("\n")
  
  cat("MCC")
  mcc
  
  
  
  sink(file = NULL)
}

buildRandomForest()




