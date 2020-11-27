# ************************************************
# ************************************************
# PRACTICAL BUSINESS ANALYTICS
# COM3018/COMM053
#
# Rahul Patel
#
# 16 NOVEMEMBER 2020
#
# ************************************************
# R Script For Decision Tree Model

# clears all objects in "global environment"
rm(list=ls())

# * ***********************************************
# Global Environment Variables (available to all functions)


DATASET <- "newTelcoTest.csv"  # Name of the input dataset csv file
PREDICTOR <- "Churn"  # Field name of the output class to predict

# Define and then load the libraries used in this project
# Library from CRAN Version
# pacman                    
# caret                 
# mlbench                 
# rpart                    
# rpart.plot               
# ModelMetrics                
# mltools 
# pROC
# rattle
# Assertr
# 
# myLibraries<-c("caret", 
#                "mlbench", 
#                "rpart", 
#                "rpart.plot", 
#                "ModelMetrics", 
#                "mltools", 
#                "pROC", 
#                "rattle", 
#                "assertr")

# User defined functions are next
# ************************************************
# pruneTree() :
#
# Prunes the CART Decision Tree using the optimal CP (Complexity Parameter) value
#
# INPUT : 
#            Object       - cDT               - CART Decision Tree
#
# OUTPUT : 
#            Object       - cDT               - pruned CART Decision Tree
#
# Keeps all objects as local to this function
# ************************************************
pruneTree<-function(cDT){
  
  # Displays CP (complexity parameter) table for DT
  printcp(cDT)
  
  # Visual representation of CP
  plotcp(cDT)

bestcp <-cDT$cptable[which.min(cDT$cptable[,"xerror"]),"CP"]

print(bestcp)
cDT<-prune(cDT, cp=bestcp)

return(cDT)
}
# ************************************************
# dtEvaluation() :
#
# Input the test dataset and obtain class predictions from the Decision Tree Model 
# Plots the results and calculates the evaluation metrics.
# 
# INPUT : 
#            Object       - cDT               - CART Decision Tree
#            Data frame   - testing_data      - testing dataset 
#
# OUTPUT : 
#            Data frame   - performance       - Performance metrics of the model
#
# Keeps all objects as local to this function
# ************************************************
dtEvaluation<-function(cDT, testing_data) {
  
  # Gets predictions from classifier of being class 1 (ChurnYes) or class 0 (ChurnNo)
  predictUnseen<-predict(cDT, testing_data, type='class')
  
  # Gets known labels of the predictor field from testing dataset 
  expectedClass<-testing_data[,PREDICTOR]
  
  # Outputs confusion matrix values as a data frame
  table<-as.data.frame(table(expectedClass, predictUnseen))
  TP<-table[4,3]
  TN<-table[1,3]
  FP<-table[3,3]
  FN<-table[2,3]
  
  # Calculating Accuracy of the model
  accuracy<- (TP+TN)/(TP+TN+FP+FN)
  
  # Calculating Precision #EDIT THIS for a customer not churning - Number of correctly classified non churning customers out of ALL (actual) non churning customers
  precision.n<- TP /(TP+FP)
  
  # Calculating Precision #EDIT THIS  for a customer churning - Number of correctly classified churning customers out of ALL (actual) churning customers
  precision.y<- TN/(TN+FN)
  
  # Calculating True Positive Rate (TPR) - #EDIT THIS  Number of people not churning that were correctly identified out of all non churning customers
  TPR<-TP/(TP+FN)
  
  # Calculating False Positive Rate (FPR) - #EDIT THIS  Number of people churning that were wrongly identified as not going to churn out of all churning customers
  FPR<- FP/(FP+TN)
  
  # Calculating Matthews Correlation Coefficient (MCC)
  mcc<- mcc(TP = TP, TN = TN, FP = FP, FN = FN)
  
  # Plotting ROC Curve
  predictUnseenProb<-predict(cDT, testing_data, type = "prob")
  plot.roc(expectedClass, predictUnseenProb[,2], legacy.axes = TRUE)
  
  # Calculating area underneath curve (AUC) 
  auc<-auc(expectedClass, predictUnseenProb[,2])
  
  # Creating a data frame with the evaluation metrics
  EvaluationName <- c("Accuracy", "PrecisionNo", "PrecisionYes", "TPR", "FPR", "MCC", "AUC")
  
  # Value of each metric in percent (excluding MCC)
  EvaluationValue <- c(accuracy *100, precision.n * 100, precision.y * 100, TPR * 100, FPR * 100, mcc, auc * 100)
  
  # Storing the evaluation and value in a data frame
  performance <- data.frame(EvaluationName, EvaluationValue)
  
  return(performance)
} #endof dtEvaluation()

# ************************************************
# decisionTree() :
#
# Creates & plots a CART Decision Tree
#
# INPUT : 
#            Data frame   - training_data     - training dataset
#            Data frame   - testing_data      - testing dataset 
#
# OUTPUT : 
#            Data frame   - performance       - Performance metrics of the model
#
# Keeps all objects as local to this function
# ************************************************
decisionTree<-function(training_data, testing_data) {
  
  set.seed(100)
  training_data <- downSample(x = training_data[, -ncol(training_data)],
                              y = factor(training_data[,PREDICTOR]))
  print(head(training_data))

  # Trains model on training datasethow 
  dtree <- rpart(Class~.,
                 data = training_data,
                 method= "class",
                 control= rpart.control(cp=0))

  # Pruning the DT
  dtree<-pruneTree(cDT = dtree)

  # Plots the decision tree model
  fancyRpartPlot(dtree)

  # Evaluate classifier on testing dataset
  performance<-dtEvaluation(cDT = dtree, testing_data = testing_data)

  # Prints variable importance to the 'viewer'
  variableImportance<-as.data.frame(varImp(dtree))
  print(formattable::formattable(variableImportance))

  # Formats data frame object for output to the "Viewer"
  print(formattable::formattable(performance))

  return(performance)
} #endof decisionTree()


# ************************************************
# main() :
#
# entry point to execute the ML data analytics
#
# INPUT: None
#
# OUTPUT :None
# ************************************************
DT<-function(){
  # 
  # library(pacman)
  # pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)
  
  # Reset the pseudo-random number generator to start at the same point
  set.seed(123)
  
  # Read dataset 
  telco<-read.csv(DATASET)
  
  # Remove additional X column
  telco<-subset(telco, select=-c(X))
  

  # Stratified Random Split
  train.index <- createDataPartition(telco[,PREDICTOR], p = 0.7, list = F)
  training<-telco[train.index,]
  testing<-telco[-train.index,]
  
  #Create a TRAINING dataset using first 70% of the records
  #and the remaining 30% is used as TEST
  #use ALL fields (columns)
  # training_records<-round(nrow(telco)*(70/100))
  # training<- telco[1:training_records,]
  # testing<-telco[-(1:training_records),]
  
  # Create, plot, and evaluates DT
  performance<-decisionTree(training_data = training, testing_data = testing)
  sink(file = paste("DT.txt"), append = T)
  
  sink(file = NULL)
  
}

# ************************************************
# This is where R starts execution

# Automatically release memory
gc()

# Tries to clear plots and other graphics in RStudio output
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the RStudio console area
cat("\014")

# ************************************************
# Call our main() function
DT()