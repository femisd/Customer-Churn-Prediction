myLibraries<-c("mltools", "data.table", "caret", "dplyr", "corrplot", "memisc", "caret", "plyr", "DMwR")
library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)

# Functions Below

conversion <- function(telco) {
  telcoNoInternet <- which(telco$InternetService == "No")
  telco[telcoNoInternet,c(10,11,12,13,14,15)]<-"No"
  
  return (telco)
  
}


# ********************************************************************************
# Detecting outlier function using IQR
detectOutlier<-function(x) {
  Q1<- quantile(x,probs =.25)
  Q3<-quantile(x, probs =.75)
  iqr = Q3 - Q1
  
  upper_limit = Q3 + (iqr * 1.5)
  lower_limit = Q1 - (iqr * 1.5)
  
  x > upper_limit | x < lower_limit
}
# ********************************************************************************
# Removing outliers function
removeOutliers<-function(data, cols = names(data)) {
  for(col in cols) {
    data <- data[!detectOutlier(data[[col]]),]
  }
  return(data)
}
# ********************************************************************************
# Normalising the data record
normalise<-function(x){
  x<-(x-min(x))/(max(x)-min(x))
  return(x)
}

# Un-normalise the data

unNormalise<-function(x){
  x<- (x)(max(x)-min(x)) + min(x)
}

# ********************************************************************************
# Normalising all the data (not being used atm)
# normaliseEverything<-function(data) {
#   data<-sapply(data, normalise)
#   return(data)
# }
# ********************************************************************************
# Correlation Function
getCorrelation <- function(telco) { 
  for(i in 1:ncol(telco)){
    if(names(telco)[i] != "ChurnYes" && names(telco)[i] != "ChurnNo") {
      
      for(x in seq(i+1, ncol(telco), 1)){
        if(names(telco)[x] != "ChurnYes" && names(telco)[x] != "ChurnNo") {
          
          if(i != 39){
            correlation <- cor.test(telco[,i], telco[,x], method = "pearson")
            
            if((correlation$estimate[[1]]) >= 0.7){
              print(paste(names(telco)[i],"-----",names(telco)[x]))
            }}}}}}}
# ********************************************************************************


# Reading Telco
telco<-read.csv("Telco.csv")

# Getting rid of customerID
#telco<-telco[c(2:21)]

print(class(telco$SeniorCitizen))

# Can use this code to get rid of NA values as opposed to the code below..
# telco<-telco[complete.cases(telco),]

#BASIC CLEANING
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
# 
# Standardisation (z-score normalisation)
# z-scale
telco$MonthlyCharges<-as.data.frame(scale(telco$MonthlyCharges,center=TRUE, scale=TRUE))
telco$TotalCharges<-as.data.frame(scale(telco$TotalCharges,center=TRUE, scale=TRUE))

# # Normalising continuous ordinals
# telco$MonthlyCharges<-normaliseEverything(telco$MonthlyCharges)
# telco$TotalCharges<-normaliseEverything(telco$TotalCharges)

max<-max(telco["MonthlyCharges"])
print(max)

min<-min(telco["MonthlyCharges"])
print(min)

# Same as above (although when using this you don't need normaliseEverything Function) Which one to use???
telco$MonthlyCharges<-sapply(telco$MonthlyCharges, normalise)
telco$TotalCharges<-sapply(telco$TotalCharges, normalise)

# Converting no internet service to no
telcoNoInternet <- which(telco$InternetService == "No")
telco[telcoNoInternet,c(9,10,11,12,13,14)]<-"No"
telcoNoInternet <- which(telco$PhoneService == "No")
telco[telcoNoInternet,7]<-"No"

# One hot encoding
dmy <- dummyVars("~ Churn", data = telco)
new <- data.frame(predict(dmy, newdata = telco))
new<- subset(new, select=-c(ChurnNo))

telco<-subset(telco, select=-c(Churn))


telco<-merge(telco, new)

#
# #Removing unecessary columns
# telco<-subset(telco, select=-c(genderFemale))
# telco<-subset(telco, select=-c(PartnerNo))
# telco<-subset(telco, select=-c(DependentsNo))
# telco<-subset(telco, select=-c(PhoneServiceNo)) ##delete?
# telco<-subset(telco, select=-c(PhoneServiceYes))
# telco<-subset(telco, select=-c(MultipleLinesNo))
# telco<-subset(telco, select=-c(OnlineBackupNo))
# telco<-subset(telco, select=-c(DeviceProtectionNo))
# telco<-subset(telco, select=-c(TechSupportNo))
# telco<-subset(telco, select=-c(StreamingTVNo))
# telco<-subset(telco, select=-c(StreamingMoviesNo))
# telco<-subset(telco, select=-c(PaperlessBillingNo))
# telco<-subset(telco, select=-c(ChurnNo))
# telco<-subset(telco, select=-c(genderMale))
# telco<-subset(telco, select=-c(MultipleLinesYes))
# ##############################################################################
#
# telco<-subset(telco, select=-c(PartnerYes))
# telco<-subset(telco, select=-c(DependentsYes))
# telco<-subset(telco, select=-c(DeviceProtectionYes))
# telco<-subset(telco, select=-c(PaymentMethodBank.transfer..automatic.))
# telco<-subset(telco, select=-c(InternetServiceDSL))
# telco<-subset(telco, select=-c(OnlineSecurityYes))
# telco<-subset(telco, select=-c(OnlineBackupYes))
# telco<-subset(telco, select=-c(StreamingTVYes))
# telco<-subset(telco, select=-c(StreamingMoviesYes))
# telco<-subset(telco, select=-c(ContractTwo.year))
# telco<-subset(telco, select=-c(PaymentMethodCredit.card..automatic.))
# telco<-subset(telco, select=-c(PaymentMethodMailed.check))
# telco<-subset(telco, select=-c(ContractOne.year))
#
# getCorrelation(telco)

# random <- cor.test(telco$tenure, telco$tenure, method = "pearson")
# print(random$estimate)

# # Creating bins for tenure
# telco <- mutate(telco, tenure_bin = tenure)
# #
# telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
# telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
# telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
# telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
# telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
# telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'
#
# telco$tenure_bin <- as.factor(telco$tenure_bin)

# Exporting the data frame
write.csv(telco, "TelcoNormal.csv")
