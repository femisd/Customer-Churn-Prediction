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
  "assertr"
)


main <- function(){
  #source("LogisticRegression.R")
  source("DT.R")
  #source("RandomForest.R")
  #source("nueralNet.R")
}


# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

main()


