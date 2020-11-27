# MYLIBRARIES<-c("caret","MASS","car","pROC","pscl","ModelMetrics")

PVALUE <- 0.05
PREDICTOR <- "ChurnYes"
MULTICOLLINEARITY_VAL <- 10
NUMBER_OF_IMPORTANT_VARIABLES <- 15

# ************************************************
# removevariablesWithCollinearity() :
#
# takes a model and see which parameters have high Multicollinearity
# and recursively remove them
#
# INPUT:   Model - model - model to be analysed
#          Data - data - data for the model
#
# OUTPUT : model with collinearity less than 10
# ************************************************
removevariablesWithCollinearity <- function(model,data){
  #checking for variables that have high multicollinearity between them
  variablesMulticollinearity <- car::vif(model$finalModel)
  variablesHighMulticollinearity <- variablesMulticollinearity[which(variablesMulticollinearity>MULTICOLLINEARITY_VAL)]
  
  if(length(variablesHighMulticollinearity)!=0){
    highestMulticollinearity <- names(sort(variablesHighMulticollinearity)[length(variablesHighMulticollinearity)])
    allvariablesModel <- names(summary(model)$coef[,1])[-1]
    newVariables <- allvariablesModel[!(allvariablesModel %in% highestMulticollinearity)]
    
    f <- as.formula(
      paste("Class",
            paste(newVariables, collapse = "+"),
            sep = " ~ "))
    
    modelnew <- trainWithStratifiedCrossValidation(f,data)
    
    return(removevariablesWithCollinearity(modelnew,data))
  }else{
    return(model)
  }
}

# ************************************************
# removeVariablesWithHighPvalue() :
#
# takes a model and see which parameters have high pvalue
# and remove them
#
# INPUT:   Model - model - model to be analysed
#          Data - data - data for the model
#
# OUTPUT : model with all variables less than 0.05 p value
# ************************************************
removeVariablesWithHighPvalue <- function(model,data){
  
  variablesHighPvalue <- names(summary(model)$coef[,4][-1][which(summary(model)$coef[,4][-1] > PVALUE)])
  allvariablesModel <- names(summary(model)$coef[,1])[-1]
  newVariables <- allvariablesModel[!(allvariablesModel %in% variablesHighPvalue)]
  
  f <- as.formula(
    paste("Class",
          paste(newVariables, collapse = "+"),
          sep = " ~ "))
  
  modelnew <- trainWithStratifiedCrossValidation(f,data)
  
  return(modelnew)
}

# ************************************************
# confusionMatrixCutoff() :
#
# takes threshold and predictions and test data and creates confusion matrix
#
# INPUT:   threshold - cutoff - threshold to be used
#          predictions - pred - predictions from the model
#          test data - testData - to get actual predictions
#
# OUTPUT : sensitivity, specificity and accuracy
# ************************************************
confusionMatrixCutoff <- function(cutoff,pred,testData){
  #only need the second column as 2nd colum gives probablity of churn(1)
  cutoffChurn <- factor((ifelse(pred[,2] >= cutoff, "Yes", "No")))
  actualChurn <- factor(ifelse(testData[,PREDICTOR]==1,"Yes","No"))
  
  confMatrixInitial <- caret::confusionMatrix(cutoffChurn, actualChurn, positive = "Yes")
  
  print(confMatrixInitial)
  
  accuracy <- confMatrixInitial$overall[1]
  sensitivity <- confMatrixInitial$byClass[1]
  specificity <- confMatrixInitial$byClass[2]
  
  confMatrix <- t(as.matrix(c(sensitivity,specificity,accuracy)))
  colnames(confMatrix) <- c("sensitivity","specificity", "accuracy")
  return(confMatrix)
}

# ************************************************
# evaluateModel() :
#
# takes a model creates a ROC curve, finds the best threshold and then create a confusion matrix
#
# INPUT:   Model - model - model to be evaluated
#          Testing data - testingData - data to evaluate the model with
#          Model Name - modelName - name of model for ROC curve plot
#
# OUTPUT : prints best threshold and confusion matrix and plots ROC curve
# ************************************************
evaluateModel <- function(model,testingData,modelName){
  #evaluation
  #get predictions on test set
  pred <- predict(model, type = "prob", newdata = testingData)
  
  #improve roc curve display
  par(pty = "s")
  
  #create roc curve to find best threshhold
  rocObj <- roc(testingData[,PREDICTOR],pred[,2],plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, main=paste("Logistic regression - ",modelName))
  
  #Outputting the evaluation metrics to txt file
  print(paste(modelName,"-","Outputting evaluation metrics to txt file"))
  sink(file = paste("lr",modelName,"_evaluation_metrics.txt"), append = T)
  cat("LOGISTIC REGRESSION")
  cat("\n")
  cat("\n")
  #optimal threshold
  cat("Best threshold")
  cat("\n")
  bestThreshold <- round(coords(rocObj, "best", "threshold",transpose = TRUE)[1],digits = 4)
  cat(bestThreshold)
  cat("\n")
  
  #Matthews correlation coefficient
  cat("\n")
  cat("mcc")
  cat("\n")
  matthewsCorrelationCoef <- ModelMetrics::mcc(predicted = pred[,2], actual = testingData[,PREDICTOR], cutoff = bestThreshold)
  cat(matthewsCorrelationCoef)
  cat("\n")
  
  cat("\n")
  confMatrixOptimal <- confusionMatrixCutoff(cutoff = bestThreshold,pred = pred, testData = testingData)
  
  sink(file = NULL)
  print("basic logistic regression metrics")
  print(confMatrixOptimal)
}

# ************************************************
# trainWithStratifiedCrossValidation() :
#
# Trains model with stratified cross validation
#
# INPUT:   Model - model - model to be evaluated
#          Training data - trainingData - data to train the model with
#
# OUTPUT : trained model
# ************************************************
trainWithStratifiedCrossValidation <- function(modelFormula,trainingData){
  crossValSettings <- trainControl(method = "repeatedcv", repeats = 3, savePredictions = TRUE)
  trainedModel <- train(modelFormula, data=trainingData, family="binomial", method="glm", trControl = crossValSettings)
  return(trainedModel)
}

# ************************************************
# createFormulaWithImportantFields() :
#
# Takes a model and gets its important variables, then returns
# a new formula for logistic regression with the important variables only
#
# INPUT:   Model - model - asses the important variables of the models formula
#
# OUTPUT : fromula for logistic regression with important variables only
# ************************************************
createFormulaWithImportantFields <- function(model){
  important <- caret::varImp(model)$importance
  important <- important[order(important$Overall),,drop=FALSE]
  importantVariables <- rownames(important)[(nrow(important)-NUMBER_OF_IMPORTANT_VARIABLES):nrow(important)]
  
  f <- as.formula(
    paste("Class",
          paste(importantVariables, collapse = "+"),
          sep = " ~ "))
  
  return(f)
}

# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Keeps all objects as local to this function
# ************************************************
LogisticRegression <- function(){
  #load dataset
  telcoPreProcessed <- read.csv(file = './PreprocessedTelco.csv')
  #remove index
  telco <- telcoPreProcessed[,-1]
  
  
  #train and test splits
  set.seed(100)
  # 70% training data
  # createDataPartition does a stratified random split of the data.
  index <- createDataPartition(telco[,PREDICTOR], p=0.7, list = F)
  
  trainData <- telco[index, ]
  testData <- telco[-index, ]
  
  #Downsampling due to large imbalance in training data
  set.seed(100)
  downtrainData <- downSample(x = trainData[, -ncol(trainData)],
                              y = factor(trainData[,PREDICTOR]))
  
  #1st iteration - Build the first model using all variables
  formulaModel1 <- as.formula(paste("Class","~","."))
  model1 <- trainWithStratifiedCrossValidation(formulaModel1,downtrainData)
  #evaluateModel(model1,testData,"Model_1")
  
  #2nd iteration - Build model using only important variables
  formulaModel2 <- createFormulaWithImportantFields(model1)
  model2 <- trainWithStratifiedCrossValidation(formulaModel2,downtrainData)
  #evaluateModel(model2,testData,"Model_2")
  
  #3rd iteration - Remove any mulitcollinearity 
  model3 <- removevariablesWithCollinearity(model2,downtrainData)
  #evaluateModel(model3,testData,"Model_3")
  
  #4th iteration - remove any variables with high p value
  #use model 2 as better than the other models
  model4 <- removeVariablesWithHighPvalue(model2,downtrainData)
  evaluateModel(model4,testData,"Model_4")
  
}


# Loads the libraries
# library(pacman)
# pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#run main
LogisticRegression()


