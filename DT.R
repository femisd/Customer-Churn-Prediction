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


DATASET <- "newTelco.csv"  # Name of the input dataset csv file
PREDICTOR <- "ChurnYes"  # Field name of the output class to predict

# Define and then load the libraries used in this project
# Library from CRAN     
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

myLibraries<-c("caret", 
               "mlbench", 
               "rpart", 
               "rpart.plot", 
               "ModelMetrics", 
               "mltools", 
               "pROC", 
               "rattle", 
               "assertr")

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
  
  # Visual representation of CP
  plotcp(cDT)
  
  # Determines the best CP by lookin for lowest xerror value
  bestcp <-cDT$cptable[which.min(cDT$cptable[,"xerror"]),"CP"]
  
  # Prunes the DT with best CP value
  cDT<-prune(cDT, cp=bestcp)
  
  return(cDT)
} #endof pruneTree()

# ************************************************
# confusionMatrixCutoff() :
#
# Takes threshold, predictions, and test data to create a confusion matrix
#
# INPUT:   
#
# INPUT : 
#            Object       - cDT               - CART Decision Tree
#            Double       - cutoff            - threshold to be used
#            Double       - pred              - predictions from the model
#            Data frame   - testing_data      - testing dataset 
#
# OUTPUT :
#            List         - confMatrix        - Confusion matrix
# ************************************************
confusionMatrixCutoff <- function(cutoff,pred,testing_data){
  
  # Depending if the probability value of 2nd column (customer churning) is greater/lower than the cut off, 
  # it is either mapped to 'Yes' or 'No' 
  cutoffChurn <- factor((ifelse(pred[,2] >= cutoff, "Yes", "No")))
  actualChurn <- factor(ifelse(testing_data[,PREDICTOR]==1,"Yes","No"))
  
  confMatrix <- caret::confusionMatrix(cutoffChurn, actualChurn, positive = "Yes")
  return(confMatrix)
} #endof confusionMatrixCutoff()

# ************************************************
# dtEvaluation() :
#
# Input the test dataset and obtain predictions from the Decision Tree Model 
# Plots the results and calculates the evaluation metrics
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
  
  # Gets predictions (probabilities) from classifier of a customer churning 
  predictUnseenProb<-predict(cDT, testing_data, type = "prob")
  
  # Creates ROC curve to determine best threshold
  roc <- roc(testing_data[,PREDICTOR],predictUnseenProb[,2],plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, main=paste("Decision Tree"))
  bestThreshold <- round(coords(roc, "best", "threshold",transpose = TRUE)[1],digits = 4)
  
  # Creates confusion matrix
  confMatrix <- confusionMatrixCutoff(cutoff = bestThreshold, pred = predictUnseenProb, testing_data = testing_data)
  
  # Stores values from confusion matrix 
  accuracy <- confMatrix$overall[1]
  sensitivity <- confMatrix$byClass[1]
  specificity <- confMatrix$byClass[2]
  
  # Calculating Matthews Correlation Coefficient (MCC)
  mcc <- ModelMetrics::mcc(predicted = predictUnseenProb[,2], actual = testing_data[,PREDICTOR], cutoff = bestThreshold)
  # Calculating area underneath curve (AUC) 
  auc<-auc(roc)
  
  # Creates a matrix and stores above values
  performance <- t(as.matrix(c(accuracy,sensitivity,specificity,mcc,auc)))
  colnames(performance) <- c("accuracy","sensitivity", "specificity", "MCC", "AUC")
  
  # Determines variable importance from DT model
  importantVariables<- varImp(cDT)
  
  # Outputs model as a set of rules into a .csv file
  TreeRules <- (rpart.rules(cDT, style = "wide", roundint = FALSE))
  FinalRules <- as.data.frame(col_concat(TreeRules, sep=" "))
  write.csv(FinalRules, "DT_Rules.csv")
  
  # Outputs results to a .txt file
  sink(file = paste("DT_Results.txt"), append = T)
  
  cat("Decision Tree")
  cat("\n")
  cat("\n")
  
  cat("-------Confusion Matrix-------")
  cat("\n")
  print(confMatrix)
  
  cat("-------Evaluation-------")
  cat("\n")
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
  
  # Downsamples dataset to ensure all classes have same frequency as the minority class
  training_data <- downSample(x = training_data[, -ncol(training_data)],
                              y = factor(training_data[,PREDICTOR]))
  
  # Trains model on training dataset 
  dtree <- rpart(Class~.,
                 data = training_data,
                 method= "class",
                 control= rpart.control(cp=0))
  
  # Pruning the DT
  dtree<-pruneTree(cDT = dtree)
  
  # Plots the decision tree model and saves it to temp
  fancyRpartPlot(dtree)
  pdf(DT_Plot <- tempfile(fileext = ".pdf"))
  fancyRpartPlot(dtree)
  dev.off()
  cat(DT_Plot)
  
  # Evaluate classifier on testing dataset
  performance<-dtEvaluation(cDT = dtree, testing_data = testing_data)
  
  # Prints evaluation (performance) metrics to the console
  print(performance)
  
  return(performance)
} #endof decisionTree()


# ************************************************
# DT() :
#
# entry point to execute the ML data analytics
#
# INPUT: None
#
# OUTPUT :None
# ************************************************
DT<-function(){
  
  library(pacman)
  pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)
  
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
  
  # Create, plot, and evaluates DT
  performance<-decisionTree(training_data = training, testing_data = testing)
  
}

# *************************************************
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
# Call our DT() function
DT()
