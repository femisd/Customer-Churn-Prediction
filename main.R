MYLIBRARIES<-c(
  "caret",
  "MASS",
  "car",
  "pROC",
  "pscl",
  "ModelMetrics",
  "outliers",
  "corrplot",
  "formattable",
  "stats",
  "PerformanceAnalytics",
  "stringr",
  "partykit",
  "C50",
  "randomForest",
  "keras",
  "h2o",
  "rattle",
  "mltools", 
  "assertr",
  "PerformanceAnalytics",
  "mlbench", 
  "rpart", 
  "rpart.plot", 
  "rattle", 
  "assertr",
  "plotly",
  "ggplot2",
  "gridExtra"
)


main <- function(){
  source("dataUnderstanding.R")
  source("DatasetPreProcessing.R")
  source("LogisticRegression.R")
  source("DT.R")
  source("nueralNet.R")
  source("RandomForest.R")
}


# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

main()


