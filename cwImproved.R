# ************************************************
# PRACTICAL BUSINESS ANALYTICS
# COM3018/COMM053
#
# Rishi Patel
#
# 16 NOVEMEMBER 2020
#
# ************************************************
# R Script For Pre-processing Data

# clears all objects in "global environment"
rm(list=ls())

# * ***********************************************
# Global Environment Variables (available to all functions)


# Define and then load the libraries used in this project
# Library from CRAN     
# mltools                    
# data.table                     
# caret                   
# dplyr                    
# corrplot               
# memisc                
# plyr 
# pROC
# rattle
# Assertr

myLibraries<-c("mltools", 
               "data.table", 
               "caret", 
               "dplyr", 
               "corrplot", 
               "memisc", 
               "plyr")

library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)

# Functions Below

# ************************************************
# conversion() :
#
# converts fields with values of "no internet service" to "No"
#
# INPUT : 
#            Object       - telco               - Telco dataset
#
# OUTPUT :
#            List         - data frame          - telco data frame
# ************************************************
conversion <- function(telco) {
  telcoNoInternet <- which(telco$InternetService == "No")
  telco[telcoNoInternet,c(10,11,12,13,14,15)]<-"No"
  
  return (telco)
}

# ************************************************
# detectOutlier() :
#
# Detecting outlier function using IQR
#
# INPUT : 
#            Object       - x        - value within given field
#
# OUTPUT :
#            List         - x        - Confusion matrix
# ************************************************
detectOutlier<-function(x) {
  Q1<- quantile(x,probs =.25)
  Q3<-quantile(x, probs =.75)
  iqr = Q3 - Q1
  
  upper_limit = Q3 + (iqr * 1.5)
  lower_limit = Q1 - (iqr * 1.5)
  
  x > upper_limit | x < lower_limit
}

# ************************************************
# removeOutliers() :
#
# loop through the Telco data dictionary columns and apply the detectOutlier function to each column
#
# INPUT : 
#            Object       - data        - Telco dataset
#            Object       - cols        - name of columns
#
# OUTPUT :
#            List         - data        - return telco data frame with outliers removed
# ************************************************
# Removing outliers function
removeOutliers<-function(data, cols = names(data)) {
  for(col in cols) {
    data <- data[!detectOutlier(data[[col]]),]
  }
  return(data)
}

# ************************************************
# normalise() :
# Normalising the data record
#
# INPUT : 
#            Object       - x        - value to be normalised
#
# OUTPUT :
#            Object         - x        - normalised value
# ************************************************
normalise<-function(x){
  x<-(x-min(x))/(max(x)-min(x))
  return(x)
}

# ************************************************
# getCorrelation() :
#
# compares each feild to each other and prints the correlation
#
# INPUT : 
#            Object       - telco               - Telco dataset
#
# OUTPUT :
#            Prints names to console
# ************************************************
# Correlation Function
getCorrelation <- function(telco) { 
  for(i in 1:ncol(telco)){
    if(names(telco)[i] != "ChurnYes" && names(telco)[i] != "ChurnNo") {
      
      for(x in seq(i+1, ncol(telco), 1)){
        if(names(telco)[x] != "ChurnYes" && names(telco)[x] != "ChurnNo") {
          
          if(i != 39){
            correlation <- cor.test(telco[,i], telco[,x], method = "pearson")
            
            if((correlation$estimate[[1]]) >= 0.4){
              print(paste(names(telco)[i],"-----",names(telco)[x]))
            }}}}}}}

# ********************************************************************************
# PreProcessing() :
#
# entry point to execute the pre-processing
#
# INPUT: None
#
# OUTPUT: None
# ************************************************
PreProcessing <- function(){

# Reading Telco
telco<-read.csv("TelcoCustomerChurn.csv")

#BASIC CLEANING
# Getting rid of customerID
telco<-telco[c(2:21)]

#stores data types of each field in a dataframe
numericOrSymbolic <- as.data.frame(sapply(telco, typeof))

#see what type of data each column is
str(telco)

#see if there are any missing values/nulls
print(complete.cases(telco))

#now find where they are
print(summary(telco))

#save the rows with na's in case needed
telcoNAs <- subset(telco, is.na(telco$TotalCharges))
print(paste("% of data which is na's",(nrow(telcoNAs)/nrow(telco))*100,"%"))

#remove the rows as such a small percentage
telco <- na.omit(telco)
  
# Removing any outliers
removeOutliers(telco, cols = names(telco$MonthyCharges))
removeOutliers(telco, cols = names(telco$TotalCharges))

# Standardisation (z-score normalisation)
# z-scale applying to MonthlyCharges and TotalCharges
telco$MonthlyCharges<-as.data.frame(scale(telco$MonthlyCharges,center=TRUE, scale=TRUE))
telco$TotalCharges<-as.data.frame(scale(telco$TotalCharges,center=TRUE, scale=TRUE))

# applying normalise function to MonthlyCharges and TotalCharges
telco$MonthlyCharges<-sapply(telco$MonthlyCharges, normalise)
telco$TotalCharges<-sapply(telco$TotalCharges, normalise)

# Converting no internet service to no
telcoNoInternet <- which(telco$InternetService == "No")
telco[telcoNoInternet,c(9,10,11,12,13,14)]<-"No"
telcoNoInternet <- which(telco$PhoneService == "No")
telco[telcoNoInternet,7]<-"No"

# One hot encoding
dmy <- dummyVars(" ~ .", data = telco)
telco <- data.frame(predict(dmy, newdata = telco))

# Removing unecessary columns
telco<-subset(telco, select=-c(genderFemale)) #no impact on churn rates
telco<-subset(telco, select=-c(PartnerNo))
telco<-subset(telco, select=-c(DependentsNo))
telco<-subset(telco, select=-c(PhoneServiceNo)) #no impact on churn rates 
telco<-subset(telco, select=-c(PhoneServiceYes)) #no impact on churn rates
telco<-subset(telco, select=-c(MultipleLinesNo))
telco<-subset(telco, select=-c(OnlineBackupNo))
telco<-subset(telco, select=-c(DeviceProtectionNo))
telco<-subset(telco, select=-c(TechSupportNo))
telco<-subset(telco, select=-c(StreamingTVNo))
telco<-subset(telco, select=-c(StreamingMoviesNo))
telco<-subset(telco, select=-c(PaperlessBillingNo))
telco<-subset(telco, select=-c(ChurnNo))
telco<-subset(telco, select=-c(genderMale)) #no impact on churn rates
telco<-subset(telco, select=-c(MultipleLinesYes))
###############################################################################

# Prints fields which are correlated based on threshold
getCorrelation(telco)

# Exporting the data frame
write.csv(telco, "PreprocessedTelco.csv")
}

# ************************************************
# Call our PreProcessing() function
PreProcessing()
