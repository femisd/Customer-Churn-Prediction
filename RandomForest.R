# dataset variables
DATA_FILE <- "PreprocessedTelco.csv"
KEY_COLUMN <- "ChurnYes"

# Learning model variables
MODEL_METHOD <- "rf"
TRAINING_PARAM <- Class ~.
TUNEL_LENGTH <- 5

# Cross validation variables
FOLD_NUMBER <- 2
REPEATS <- 1
P_VALUE <- .75
VALIDATION_METHOD <- "repeatedcv"
CONFUSION_MATRIX_POSITIVE_CLASS <- "1"

MAX <-1.793254
MIN <- -1.547173

MEAN <- 64.79821
SD <- 30.08597

#Read dataset
readDataset <- function(datafile, keyColumn){
  dataset<-read.csv(datafile, encoding="UTF-8", stringsAsFactors = FALSE)
  dataset[,keyColumn] = as.factor(dataset[,keyColumn])
  return( dataset<-subset(dataset, select=-c(X)))
}
#un-normalise
unNormalise<-function(MAX, MIN, x){
   x<- (x)*(MAX-MIN) + MIN

  return (x)
}
# ********************************************************************************
#unS
unStandardise<-function(SD,MEAN,x){
  x<- ((x)*SD)+MEAN
  
  return (x)
}

# Prepare training data
separateTrainingSet <- function(dataset, pValue, keyColumn){
  trainingSet <- createDataPartition(dataset[,keyColumn],p= pValue, list = FALSE)
  return(trainingSet)
}

# Return training data
getTrainingData <- function(dataset, trainSet){
  trainSet <- dataset[trainSet,]
  set.seed(100)
  downtrainData <- downSample(x = trainSet[, -ncol(trainSet)],
                              y = factor(trainSet[,KEY_COLUMN]))
  return (downtrainData)
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
  return(prediction[,2])
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

# AUC

generateAuc <- function (model, testingData, predictor){
  pred <- predict(model, type = "prob", newdata = testingData)
  auc<-auc(testingData[, predictor],pred[,2])
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

# ************************************************
# determineTheChurnRecords() :
#
# determine the people who will churn from the testing data
# 
#
# INPUT:   dataset - testingData - dataset of the customers 
#          model - model - model used to make predictions
#          threshold - bestthreshold - threshold used to determine if a customer churns
#
# OUTPUT : the valuable customers from the dataset
# ************************************************
determineTheChurnRecords <- function(model,testingData,bestThreshold){
  
  set.seed(100)
  #probability of the records churning
  pred <- predict(model, type = "prob", newdata = testingData)
  #bind the proabblities to the testing data
  testingWithChurn <- cbind(testingData,pred[,2])
  #remove ChurnYes as want to use proability and not actual
  #remove the index
  testingWithChurn <- subset(testingWithChurn, select=-c(ChurnYes))
  #rename column with proability to Churn 
  names(testingWithChurn)[names(testingWithChurn) == "pred[, 2]"] <- "Churn"
  
  #find of the records which ones will churn
  whichRecordsChurnRows <- which(testingWithChurn$Churn >= bestThreshold)
  whichRecordsChurn <- testingWithChurn[whichRecordsChurnRows,]

  return(whichRecordsChurn)
}

# ************************************************
# determineValuableCustomers() :
#
# determines low quality customers firstly by seeing which fall below average tenure
# then of those customers seeing which fall below average monthly charge then of
# of those customers which are on a month to month contract, then of those customers see 
# which fall below the average monthly charge again.
# 
# those low quality customers are removed and the rest are deemed as valuable 
#
# INPUT:   dataset - churnCustomers - dataset of the customers who churn
# 
#
# OUTPUT : the valuable customers from the dataset
# ************************************************
determineValuableCustomers <- function(churnCustomers){
  
  #tenure
  averageTenure <- mean(churnCustomers$tenure)
  customersLessThanAverageTenure <- churnCustomers[which(churnCustomers$tenure <= averageTenure),]
  
  #monthly charge
  averageMonthlyCharge <- mean(customersLessThanAverageTenure$MonthlyCharges)
  customersLessThanMonthlyCharge <- customersLessThanAverageTenure[which(customersLessThanAverageTenure$MonthlyCharges <= averageMonthlyCharge),]
    
  lowQualityCustomers <- customersLessThanMonthlyCharge[which(customersLessThanMonthlyCharge$ContractMonth.to.month == 1),]
  
  #monthly charge of low quality customers
  meanlowQualityCustomersMonthlyCharge <- mean(lowQualityCustomers$MonthlyCharges)
  finalCustomersThatWillBeLetGo <- lowQualityCustomers[which(lowQualityCustomers$MonthlyCharges <= meanlowQualityCustomersMonthlyCharge),]
  
  valuableCustomers <- churnCustomers[-which(lowQualityCustomers$MonthlyCharges <= meanlowQualityCustomersMonthlyCharge),]
  
  print(paste("customers being let go",nrow(finalCustomersThatWillBeLetGo)))
  print(paste("customers being kept",nrow(valuableCustomers)))
  
  return(valuableCustomers) 
}

# ************************************************
# discount() :
#
# applys a 5% discount
#
#
# INPUT:   number - x - number to discount 
#
# OUTPUT : how much discount should be applied
# ************************************************
discount <- function(x) {
  return (x * 0.05)
}

buildRandomForest <- function(){
  
  dataset <- readDataset(DATA_FILE, KEY_COLUMN)
  trainSet <- separateTrainingSet(dataset, P_VALUE, KEY_COLUMN)
  trainingData <- getTrainingData(dataset, trainSet)
  testingData <- getTestingData(dataset, trainSet)
  kControl <- createCrossValidation(VALIDATION_METHOD, FOLD_NUMBER, REPEATS)
  
  model <- generateRandomForestModel(TRAINING_PARAM, trainingData, MODEL_METHOD, TUNEL_LENGTH, kControl)
  classProbabilityResult <- createClassProbabilityPrediction(model, dataset)
  confusionMatrix <- creatConfusionMatrix(model, testingData, KEY_COLUMN, CONFUSION_MATRIX_POSITIVE_CLASS)

  
  bestThreshold <- createRoc(model, testingData, KEY_COLUMN)
  auc <- generateAuc(model, testingData, KEY_COLUMN)
  mcc <- createMcc(model, testingData,confusionMatrix)
  
  importantVariables <- caret::varImp(model)
  
  sink(file = paste("randomF.txt"), append = T)
  
  cat("Random Forest")
  cat("\n")
  cat("\n")
  cat("\n")
  
  # Print model result
  cat("-------Model Result-------")
  cat("\n")
  print(model$result)
  
  cat("\n")
  cat("\n")
  
  #Print class probability
  cat("----Class Probability Results----")
  cat("\n")
  print(head(classProbabilityResult))
  cat("\n")
  cat("\n")
  
  
  #Print confusion matrix
  cat("-------Confusion Matrix-------")
  cat("\n")
  
  print(confusionMatrix)
  cat("\n")
  cat("\n")
  
  cat("-------Evaluation-------")
  cat("\n")
  
  cat("Threshold: ")
  cat(bestThreshold)
  cat("\n")
  cat("\n")
  
  cat("AUC: ")
  cat(auc)
  cat("\n")
  cat("\n")
  
  
  cat("MCC Result: ")
  cat(mcc)
  cat("\n")
  cat("\n")
  
  cat("-----Variable Importance------")
  cat("\n")
  print(importantVariables)
  
  sink(file = NULL)
  
  #Finding which customers are valuable and worth keeping
  churnRecords <- determineTheChurnRecords(model,testingData,bestThreshold)
  valuableChurn <- determineValuableCustomers(churnRecords)
  
  valuableChurn$discount <- lapply(valuableChurn$MonthlyCharges, function(x) sapply(x, discount))
  costToCompany <- as.data.frame(valuableChurn$discount)
  
  costToCompany <- unNormalise(MAX = MAX, MIN=MIN, x = costToCompany)
  costToCompany <- unStandardise(SD=SD, MEAN=MEAN ,x = costToCompany)
  
  print(paste("Cost to the company to keep ",nrow(valuableChurn),"customers - £",rowSums(costToCompany)*12))
  
}

buildRandomForest()




