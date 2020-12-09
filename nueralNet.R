
#  clears all objects in "global environment"
rm(list=ls())
# 
# MYLIBRARIES<-c("outliers",
#                "corrplot",
#                "MASS",
#                "formattable",
#                "stats",
#                "caret",
#                "PerformanceAnalytics",
#                "stringr",
#                "partykit",
#                "C50",
#                "randomForest",
#                "keras",
#                "h2o",
#                "pROC",
#                "ModelMetrics",
#                "rattle",
#                "mltools", 
#                "assertr")

INPUT_FILE="PreprocessedTelco.csv";
MAX_ITER=150
DEPENDEDNT_VARIABLE='ChurnYes';


# Cross validation variables
FOLD_NUMBER <- 10
REPEATS <- 3
P_VALUE <- .75
VALIDATION_METHOD <- "repeatedcv"

# ************************************************
# readFileDataFrame() :
#
#reads in a csv file and converts it into a dataframe
#
# INPUT: Name of the input file   
#
# OUTPUT : dataframe  
# ************************************************
readFileDataFrame<-function(dataset){
   
   telcoDataset<-read.csv(dataset,encoding="UTF-8",stringsAsFactors = FALSE);
   
   return(telcoDataset)
}

# ************************************************
# factorDependentVariable() :
#
# factor the churnYes Column  
#
# INPUT:  telcoDataset - original dataframe
#         DEPENDEDNT_VARIABLE-churnYes
#
#
# OUTPUT : factored ChurnYes
# ************************************************
factorDependentVariable<- function(telcoDataset,DEPENDEDNT_VARIABLE){
   
   maxmindf<- as.data.frame(telcoDataset);
   
   maxmindf[, DEPENDEDNT_VARIABLE] = as.factor(maxmindf[, DEPENDEDNT_VARIABLE])
   
   return(maxmindf)
}

# ************************************************
# createCrossValidation() :
#
# caret model control
#
# INPUT:  validationMethod - validation method used
#         numbers-fold number
#         repeats- number of repeats
# OUTPUT : stratified random split of the data  
# ************************************************
createCrossValidation <- function(validationMethod, numbers, repeats){
   return (trainControl(method = validationMethod ,number = numbers, repeats = repeats))
}


# ************************************************
# partitionDataSet() :
#
#createDataPartition does a stratified random split of the data.
#
# INPUT:  dataframe  
#
# OUTPUT : stratified random split of the data  
# ************************************************
partitionDataSet<-function(maxmindf){
   
   totTrain <- createDataPartition(maxmindf[,DEPENDEDNT_VARIABLE],p= .75,list = FALSE)
   
   return(totTrain)
}

# ************************************************
# createTrainData() :
#
# INPUT:  maxmindf - original dataframe
#         totTrain - partitioned dataframe 
#
# OUTPUT : training data dataframe  
# ************************************************
createTrainData<-function(maxmindf,totTrain){
   
   trainingData<-maxmindf[totTrain,]
   
   return(trainingData)
}

# ************************************************
# createTestingData() :
#
# INPUT:  maxmindf - original dataframe
#         totTrain - partitioned dataframe 
#
# OUTPUT : testing data dataframe  
# ************************************************
createTestingData<-function(maxmindf,totTrain){
   
   testing <-maxmindf[-totTrain,]
   
   return(testing)
}

# ************************************************
# partitionDataSet() :
#
#Train the nnet model on our dataset.
# 
# INPUT:  dependentVar - our dependent variable
#         data - processed dataframe
#         algo - algorithm used to train the model
#         maxiter - max number of iterations
#         tuneGrid - 
#         control- Control the computational nuances of the train function
#
# OUTPUT : model
# ************************************************

trainModel<-function(dependentVar,data,algo,maxiter,tuneGrid,control){
   
   telcoTrain <- caret::train(
      dependentVar,
      data = data,
      method = algo,
      linear.output = TRUE,
      maxit=maxiter,
      tuneGrid=tuneGrid , # cannot pass parameter hidden directly!!
      trControl = control
   )
   
   return(telcoTrain)
}

# ************************************************
# createPrediciton() :
#
# INPUT:  trainedModel - trained Model
#         testingData - testing data datagrame 
#
# OUTPUT : a vector of predictions
# ************************************************
createPrediciton<-function(trainedModel,testingData){
   predictTelco<- predict(trainedModel,newdata=testingData)
   
   return(predictTelco)
}

# ************************************************
# createPredicitonProb() :
#
# INPUT:  trainedModel - trained Model
#         testingData - testing data datagrame 
#
# OUTPUT : a vector of predictions
# ************************************************
createPredicitonProb<-function(trainedModel,testingData){
   predictTelco<-predict(trainedModel,type="prob",newdata=testingData)   
   return(predictTelco)
}

# ************************************************
# createConfusionMatrix() :
#
# INPUT:  prediction - prediction variable
#         testingData - testing data datagrame 
#         keyColumn - dependent variable chrunYes
#
# OUTPUT : a confusion matrix
# ************************************************
createConfusionMatrix<-function(prediction,testingData,keyColumn){
   
   confusionMatrix<-caret::confusionMatrix(data=prediction,testingData[,keyColumn],positive='1')
   
   return(confusionMatrix)
   
}

# ************************************************
# createRocCurve() :
#
#create a roc curve
#
# INPUT:  DEPENDEDNT_VARIABLE - churnYes
#         testingData - testing data datagrame 
#         prediction - prediction variable
#         modelName - trainedModel   
#
# OUTPUT : a confusion matrix
# ************************************************
createRocCurve<-function(testingData,DEPENDEDNT_VARIABLE,prediction,modelName){
   
   rocVal<-roc(testingData[, DEPENDEDNT_VARIABLE],prediction[,2],plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, main=paste(" NNET - ",modelName))
   
   return(rocVal)
   
}

# ************************************************
# getBestThreshold() :
#
#calculate the best threshold from the roc curve
#
# INPUT:  rocVal - roc curve.  
#
# OUTPUT : best threshold
# ************************************************
getBestThreshold<-function(rocVal){
   
   bestThreshold <- round(coords(rocVal, "best", "threshold",transpose = TRUE)[1],digits = 4)
   
   return(bestThreshold)
   
}

# ************************************************
# createMatthew() :
#
#calculate the matthew correlation coefficient 
#
# INPUT:  confusionMat- our confusion matrix
#
# OUTPUT : mcc value
# ************************************************
createMatthew<-function(confusionMat){
   
   TP<-confusionMat$table[1]
   FN<-confusionMat$table[2]
   FP<-confusionMat$table[3]
   TN<-confusionMat$table[4]
   
   matthewsCorrelationCoef <- mcc(preds = NULL, actuals = NULL,TP=TP, FP=FP, TN=TN, FN=FN)
   
   return(matthewsCorrelationCoef)
   
}

# ************************************************
# createAucVal() :
#
#calculate the auc
#
# INPUT:  testingData - True positives
#         DEPENDEDNT_VARIABLE - False positives
#         probPredictTelco - prob variable
#         
# OUTPUT : auc value
# ************************************************
createAucVal<-function(testingData,DEPENDEDNT_VARIABLE,probPredictTelco){
   
   aucVal<- auc(testingData[, DEPENDEDNT_VARIABLE],probPredictTelco[,2])
   
   return(aucVal)
   
}

# ************************************************
# downSampleData() :
#
#downsample the data
#
# INPUT:  trainingData - training dataframe
#         DEPENDEDNT_VARIABLE - chrunYes
#  
#         
# OUTPUT : auc value
# ************************************************
downSampleData<-function(trainingData,DEPENDEDNT_VARIABLE){
   
   downtrainData <- downSample(x = trainingData[, -ncol(trainingData)],
                               y = factor(trainingData[,DEPENDEDNT_VARIABLE]))
   return(downtrainData)
   
}


# ************************************************
# main() :
#
# Entry point to execute your data analytics
#
# INPUT:  None
#
# OUTPUT :None
# ************************************************
nueralNet<-function(){
  
   set.seed(100)
  
   telcoDataset<-readFileDataFrame(INPUT_FILE);
  
   telcoDataset<-subset(telcoDataset, select=-c(X))
  
   maxmindf<- factorDependentVariable(telcoDataset,DEPENDEDNT_VARIABLE)

   totTrain <- partitionDataSet(maxmindf)
  
   trainingData<-createTrainData(maxmindf,totTrain)
 
   testingData <-createTestingData(maxmindf,totTrain)
  
   downtrainData <- downSampleData(trainingData,DEPENDEDNT_VARIABLE)
  
   trainingData <- data.frame(downtrainData)
  
   kControl <- createCrossValidation(VALIDATION_METHOD, FOLD_NUMBER, REPEATS)
    
   telcoTrain <- trainModel(Class~.,trainingData,"nnet",MAX_ITER,expand.grid(.size=c(3,5,7,10),.decay=c(0,0.001,0.1,1)), kControl)
   
   predicitonMod<- createPrediciton(telcoTrain,testingData)
   
   probPredictTelco<- createPredicitonProb(telcoTrain,testingData)
   
   confusionMat <-createConfusionMatrix(predicitonMod,testingData,DEPENDEDNT_VARIABLE)
   
   rocVal<- createRocCurve(testingData,DEPENDEDNT_VARIABLE,probPredictTelco,telcoTrain)
   
   auc<-createAucVal(testingData,DEPENDEDNT_VARIABLE,probPredictTelco)
   
   bestThreshold <- getBestThreshold(rocVal)
   
   matthewsCorrelation <-  createMatthew(confusionMat)
   
    sink(file = paste("NN.txt"), append = T)
    
    cat("NeuralNet")
    cat("\n")
    cat("\n")
    
    cat("\n")
    cat("Model Output:")
    print(telcoTrain$results)
    cat("\n")
    
    cat("\n")
    cat("threshold:")
    cat(bestThreshold)
    cat("\n")
    
    cat("\n")
    cat("auc:")
    cat(auc)
    cat("\n")
    
    #Matthews correlation coefficient
    cat("\n")
    cat("mcc:")
    cat(matthewsCorrelation)
    cat("\n")
    
    cat("\n")
    cat("Confusion Matrix:")
    print(confusionMat)
    
    sink(file = NULL)
  
} #endof main()

# ************************************************
# This is where R starts execution

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("START Supervised Machine Learning")
#install.packages('caret', dependencies = TRUE)

#library(pacman)
# library(nnet)
# pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)
#source("nerualNetFunctions.R")

set.seed(123)

# ************************************************
nueralNet()

print("end")
