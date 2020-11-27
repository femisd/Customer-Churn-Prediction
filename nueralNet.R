
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
MAX_ITER=250;
DEPENDEDNT_VARIABLE='ChurnYes';
NUM_FOLDS=10;
NUM_REPEATS=3;


readFileDataFrame<-function(dataset){
   
   telcoDataset<-read.csv(dataset,encoding="UTF-8",stringsAsFactors = FALSE);
   
   return(telcoDataset)
}

normalizedData<-function(x){
   return((x-min(x)) / (max(x)-min(x)))
}

normaliseDataSet<- function(telcoDataset,DEPENDEDNT_VARIABLE){
   
   maxmindf<- as.data.frame(lapply(telcoDataset, normalizedData));
   
   maxmindf[, DEPENDEDNT_VARIABLE] = as.factor(maxmindf[, DEPENDEDNT_VARIABLE])
   
   return(maxmindf)
}

# ************************************************
# partitionDataSet() :
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
# partitionDataSet() :
#
# INPUT:  dataframe  
#
# OUTPUT : stratified random split of the data  
# ************************************************
createTrainData<-function(maxmindf,totTrain){
   
   trainingData<-maxmindf[totTrain,]
   
   return(trainingData)
}

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

createPrediciton<-function(trainedModel,testingData){
   predictTelco<- predict(trainedModel,newdata=testingData)
   
   return(predictTelco)
}

createConfusionMatrix<-function(prediction,testingData,keyColumn){
   
   confusionMatrix<-caret::confusionMatrix(data=prediction,testingData[,keyColumn],positive='1')
   
   return(confusionMatrix)
   
}

createRocCurve<-function(testingData,DEPENDEDNT_VARIABLE,prediction,modelName){
   
   rocVal<-roc(testingData[, DEPENDEDNT_VARIABLE],prediction[,2],plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, main=paste(" NNET - ",modelName))
   
   return(rocVal)
   
}


getBestThreshold<-function(rocVal){
   
   bestThreshold <- round(coords(rocVal, "best", "threshold",transpose = TRUE)[1],digits = 4)
   
   return(bestThreshold)
   
}


matthew<-function(TP,FP,TN,FN){
   
   matthewsCorrelationCoef <- mcc(preds = NULL, actuals = NULL,TP=TP, FP=FP, TN=TN, FN=FN)
   
   return(matthewsCorrelationCoef)
   
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
  
  
  telcoDataset<-readFileDataFrame(INPUT_FILE);
  
  maxmindf<- normaliseDataSet(telcoDataset,DEPENDEDNT_VARIABLE)
  
  #numeric -> factor

  totTrain <- partitionDataSet(maxmindf)
  
  trainingData<-maxmindf[totTrain,]
  testingData <-maxmindf[-totTrain,]
  
  trainingData <- data.frame(trainingData)
  
   #ten fold val
   kControl <- trainControl(method = "repeatedcv",number =1,repeats=3)
    
   print(Sys.time())
   
   telcoTrain <- trainModel(ChurnYes~.,trainingData,"nnet",MAX_ITER,expand.grid(.size=c(1,3,5,7,10,12,20),.decay=c(0,0.001,0.1,1,4,10)), kControl)
   
   predicitonMod<- createPrediciton(telcoTrain,testingData)
   
   probPredictTelco<- predict(telcoTrain,type="prob",newdata=testingData)
   
   confusionMat <-createConfusionMatrix(predicitonMod,testingData,DEPENDEDNT_VARIABLE)
   
   rocVal<- createRocCurve(testingData,DEPENDEDNT_VARIABLE,probPredictTelco,telcoTrain)
   
   bestThreshold <- getBestThreshold(rocVal)
      
  # matthewsCorrelation <-  mcc(predicted = testPredictTelco[,1], actual = testing$ChurnYes, cutoff = bestThreshold)
   
   #printDataToFile(rocVal,matthewsCorrelation,confusionMat,bestThreshold)
   
   # print(bestThreshold)
   
 #  confuTab <-confusionMat$table[1]
   TP<-confusionMat$table[1]
   FN<-confusionMat$table[2]
   FP<-confusionMat$table[3]
   TN<-confusionMat$table[4]
    
   matthewsCorrelation <-  matthew(TP,FP,TN,FN)

 
   print(matthewsCorrelation)
   
   
    print(Sys.time())

    print(telcoTrain)

    print(confusionMat)
 
    plot(rocVal)
   
  #using nnet
  # system.time(
  #   telcoTrain <- caret::train(
  #     ChurnYes~.,
  #     data = trainingData,
  #     method = "nnet",
  #     linear.output = TRUE,
  #     maxit=150,
  #     tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1)) , # cannot pass parameter hidden directly!!
  #     trControl = kControl
  #   )
  # )
  

   
   
  #nueralNet

  
  #confusion Matrix section
   
   # predictTelco<- predict(telcoTrain,newdata=testing)
   # 
   # confMat<-confusionMatrix(data=predictTelco,testing$ChurnYes)
   
   #print(confMat)
   
   # p_class<-ifelse(predictTelco>0.50,"M","R")
   # table(p_class)
   # print(p_class)
   
   #Size-number of units in the hidden layer. Can be zero if there are skip-layer units.
   
   
   #using neuralnet
   # system.time(
   #   telcoTrain1 <- caret::train(
   #     ChurnYes~.,
   #     data = trainingData,
   #     threshold = 0.1,
   #     stepmax = 1e+05,
   #     method = "neuralnet",
   #     linear.output = TRUE,
   #     tuneGrid = data.frame(layer1 = 2:3, layer2 = 0, layer3 = 0),
   #     trControl = kControl
   #   )
   # )
   # 
   #  print(telcoTrain1)
   # 
   #  plot(telcoTrain1)
  
    sink(file = paste("NN.txt"), append = T)
    
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
