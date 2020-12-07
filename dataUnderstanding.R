# Global Environment variables

#Libraries needed
MYLIBRARIES<-c("plotly","ggplot2","gridExtra")

# User defined functions are next
# ************************************************

# ************************************************
# basicCleaning() :
#
# Cleans the dataset of any NA values
# 
#
# INPUT:   Data Frame - dataset - dataset to be cleaned
#
# OUTPUT : Data Frame  - NA values removed
# ************************************************
basicCleaning <- function(dataset){
  #see what type of data each column is
  str(dataset)
  #see if there are any missing values/nulls
  print(complete.cases(dataset))
  #now find where they are
  print(summary(dataset))
  #save the rows with na's in case needed
  telcoNAs <- subset(dataset, is.na(dataset$TotalCharges))
  
  print(paste("% of data which is na's",(nrow(telcoNAs)/nrow(dataset))*100,"%"))
  
  #remove the rows as such a small percentage
  telcoCleaned <- na.omit(dataset)
  
  return(telcoCleaned)
}

# ************************************************
# importantChurnRates() :
#
# Creates a grid of histograms and a bar chart relating to overall data set churn rates,
# tenure churn rates and monthly charges churn rates
# 
#
# INPUT:   Data Frame - dataset - dataset to be analysed
#
# OUTPUT : grid of histograms and a bar chart in the plots tab
# ************************************************
importantChurnRates <- function(dataset){
  churnsplitYes <- table(dataset$Churn)[["Yes"]]
  churnsplitNo <- table(dataset$Churn)[["No"]]
  print(paste("churned: ", round((churnsplitYes/(churnsplitYes+churnsplitNo))*100, digits = 2), "% stayed: ",  round((churnsplitNo/(churnsplitYes+churnsplitNo))*100,digits = 2),"%"))
  
  #churn percentage labels for bar plot
  dfTab <- as.data.frame(table(dataset$Churn))
  colnames(dfTab)[1] <- "Churn"
  dfTab$lab <- paste((round(100 * dfTab$Freq / sum(dfTab$Freq), digits = 2)),"%")
  
  #overall churn rate
  churnRatesBar <- ggplot(dataset, aes(x=Churn, fill=factor(Churn))) +
    geom_bar() +
    geom_text(data=dfTab,aes(x=Churn,y=Freq,label=lab),vjust=0) +
    xlab("Churn") +
    ylab("Total Count") + 
    labs(fill= "Churn")
  
  #tenure churn rates
  churnRatesTenure <- ggplot(dataset, aes(x=tenure, fill=factor(Churn))) +
    geom_histogram(binwidth = 1) +
    xlab("Length of tenure") +
    ylab("Total Count") + 
    labs(fill= "Churn")
  
  #monthly charges churn rates
  churnRatesMonthlyCharges <- ggplot(dataset, aes(x=MonthlyCharges, fill=factor(Churn))) +
    geom_histogram(binwidth = 5) +
    xlab("Monthly charges") +
    ylab("Total Count") + 
    labs(fill= "Churn")
  
  grid.arrange(churnRatesBar,churnRatesTenure, churnRatesMonthlyCharges, nrow=1,top="Important Churn rates")
}

# ************************************************
# CustomerCharacteristics() :
#
# Creates a grid of bar charts relating to the data sets customer demographics/characteristics
# 
#
# INPUT:   Data Frame - dataset - dataset to be analysed
#
# OUTPUT : grid of bar charts in the plots tab
# ************************************************
CustomerCharacteristics <- function(dataset){
  
  #gender characteristics
  dfTabgender <- as.data.frame(table(dataset$gender))
  colnames(dfTabgender)[1] <- "gender"
  dfTabgender$lab <- paste((round(100 * dfTabgender$Freq / sum(dfTabgender$Freq), digits = 2)),"%")
  
  genderRatesBar <- ggplot(dataset, aes(x=gender, fill=factor(gender))) +
    geom_bar() +
    geom_text(data=dfTabgender,aes(x=gender,y=Freq,label=lab),vjust=0) +
    xlab("gender") +
    ylab("Total Count") + 
    labs(fill= "gender")
  
  #senior characteristics
  
  #make graph easier to read by changing 1 and 0 to senior and Not senior
  telcoChangeSenior <- dataset[,3]
  senior <- which(telcoChangeSenior == 1)
  telcoChangeSenior[senior]<-"Senior"
  telcoChangeSenior[-senior]<-"Not Senior"
  
  seniorRatesBar <- ggplot(as.data.frame(telcoChangeSenior), aes(x=telcoChangeSenior, fill=factor(telcoChangeSenior))) +
    geom_bar() +
    xlab("Senior Citizen") +
    ylab("Total Count") +
    labs(fill= "SeniorCitizen")
  
  #partner characteristics
  dfTabpartner <- as.data.frame(table(dataset$Partner))
  colnames(dfTabpartner)[1] <- "Partner"
  dfTabpartner$lab <- paste((round(100 * dfTabpartner$Freq / sum(dfTabpartner$Freq), digits = 2)),"%")
  
  partnerRatesBar <- ggplot(dataset, aes(x=Partner, fill=factor(Partner))) +
    geom_bar() +
    geom_text(data=dfTabpartner,aes(x=Partner,y=Freq,label=lab),vjust=0) +
    xlab("Partner") +
    ylab("Total Count") + 
    labs(fill= "Partner")
  
  #dependent characteristics
  dfTabdependents <- as.data.frame(table(dataset$Dependents))
  colnames(dfTabdependents)[1] <- "Dependents"
  dfTabdependents$lab <- paste((round(100 * dfTabdependents$Freq / sum(dfTabdependents$Freq), digits = 2)),"%")
  
  dependentsRatesBar <- ggplot(dataset, aes(x=Dependents, fill=factor(Dependents))) +
    geom_bar() +
    geom_text(data=dfTabdependents,aes(x=Dependents,y=Freq,label=lab),vjust=0) +
    xlab("Dependents") +
    ylab("Total Count") + 
    labs(fill= "Dependents")
  
  grid.arrange(genderRatesBar,seniorRatesBar,partnerRatesBar,dependentsRatesBar, nrow =2, top = "Customer characteristics")
  
}

# ************************************************
# CustomerCharacteristicsChurnRates() :
#
# Creates a grid of bar charts relating to the data sets customer demographics/characteristics
# churn rates
#
# INPUT:   Data Frame - dataset - dataset to be analysed
#
# OUTPUT : grid of bar charts in the plots tab
# ************************************************
CustomerCharacteristicsChurnRates <- function(dataset){
  #male/females churn rate
  genderChurnRates <- ggplot(dataset, aes(x=gender, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("Gender") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  #make graph easier to read by changing 1 and 0 to senior and Not senior
  telcoChangeSenior <- dataset[,(c(3,ncol(dataset)))]
  senior <- which(telcoChangeSenior$SeniorCitizen == 1)
  telcoChangeSenior[senior,1]<-"Senior"
  telcoChangeSenior[-senior,1]<-"Not Senior"
  
  seniorCitizenChurnRates <- ggplot(telcoChangeSenior, aes(x=SeniorCitizen, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("SeniorCitizen") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  #partner churn rate
  partnerChurnRates <- ggplot(dataset, aes(x=Partner, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("Partner") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  #dependent churn rate
  dependentsChurnRates <- ggplot(dataset, aes(x=Dependents, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("Dependents") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  grid.arrange(genderChurnRates,seniorCitizenChurnRates,partnerChurnRates,dependentsChurnRates, nrow =2, top = "Churn analysis of customer characteristics")
  
}

# ************************************************
# analysisOfServices() :
#
# Creates a grid of bar charts relating to the data sets offered services
# 
#
# INPUT:   Data Frame - dataset - dataset to be analysed
#
# OUTPUT : grid of bar charts in the plots tab
# ************************************************
analysisOfServices <- function(dataset){
  #if no phone service then mutiplelines column can be no
  telcoNoInternet <- which(dataset$PhoneService == "No")
  dataset[telcoNoInternet,8]<-"No"
  
  #if no internet then columns 10 to 15 can be changed to no
  telcoNoInternet <- which(dataset$InternetService == "No")
  dataset[telcoNoInternet,c(10,11,12,13,14,15)]<-"No"
  
  #phoneservice plot
  phoneService <- ggplot(dataset, aes(x=PhoneService, fill=factor(PhoneService))) +
    geom_bar() +
    xlab("Phone Service") +
    ylab("Total Count") +
    labs(fill= "Phone Service")
  
  #Mutiplelines plot
  multipleLinesService <- ggplot(dataset, aes(x=MultipleLines, fill=factor(MultipleLines))) +
    geom_bar() +
    xlab("MultipleLines") +
    ylab("Total Count") +
    labs(fill= "MultipleLines")
  
  #internet service plot
  internetService <- ggplot(dataset, aes(x=InternetService, fill=factor(InternetService))) +
    geom_bar() +
    xlab("Internet Service") +
    ylab("Total Count") +
    labs(fill= "Internet Service")
  
  #Online security plot
  onlineSecurity <- ggplot(dataset, aes(x=OnlineSecurity, fill=factor(OnlineSecurity))) +
    geom_bar() +
    xlab("Online Security") +
    ylab("Total Count") +
    labs(fill= "Online Security")
  
  #Online security plot
  onlineBackup <- ggplot(dataset, aes(x=OnlineBackup, fill=factor(OnlineBackup))) +
    geom_bar() +
    xlab("Online Backup") +
    ylab("Total Count") +
    labs(fill= "Online Backup")
  
  #Device protection plot
  deviceProtection <- ggplot(dataset, aes(x=DeviceProtection, fill=factor(DeviceProtection))) +
    geom_bar() +
    xlab("Device Protection") +
    ylab("Total Count") +
    labs(fill= "Device Protection")
  
  #Tech support plot
  techSupport <- ggplot(dataset, aes(x=TechSupport, fill=factor(TechSupport))) +
    geom_bar() +
    xlab("Tech Support") +
    ylab("Total Count") +
    labs(fill= "Tech Support")
  
  #Streaming TV plot
  streamingTV <- ggplot(dataset, aes(x=StreamingTV, fill=factor(StreamingTV))) +
    geom_bar() +
    xlab("Streaming TV") +
    ylab("Total Count") +
    labs(fill= "Streaming TV")
  
  #Streaming movies plot
  streamingMovies <- ggplot(dataset, aes(x=StreamingMovies, fill=factor(StreamingMovies))) +
    geom_bar() +
    xlab("Streaming Movies") +
    ylab("Total Count") +
    labs(fill= "Streaming Movies")

  grid.arrange(phoneService,multipleLinesService,internetService,onlineSecurity,onlineBackup,deviceProtection,techSupport,streamingTV,streamingMovies, nrow =3, top = "Offered services")
  
}

# ************************************************
# analysisOfServices() :
#
# Creates a grid of bar charts relating to the churn rates of the data sets most popular services 
# 
#
# INPUT:   Data Frame - dataset - dataset to be analysed
#
# OUTPUT : grid of bar charts in the plots tab
# ************************************************
churnAnalysisOfPopularServices <- function(dataset){
  
  #if no internet then columns 10 to 15 can be change to no
  telcoNoInternet <- which(dataset$InternetService == "No")
  dataset[telcoNoInternet,c(10,11,12,13,14,15)]<-"No"
  
  #internet service churn plot
  internetServiceChurnRates <- ggplot(dataset, aes(x=InternetService, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("Internet Service") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  #Streaming TV plot churn plot
  streamingTVChurnRates <- ggplot(dataset, aes(x=StreamingTV, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("Streaming TV") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  #Streaming movies churn plot
  streamingMoviesChurnRates <- ggplot(dataset, aes(x=StreamingMovies, fill=factor(Churn))) +
    geom_bar(position = 'fill') +
    xlab("Streaming Movies") +
    ylab("Percentage") + 
    labs(fill= "Churn")
  
  grid.arrange(internetServiceChurnRates,streamingTVChurnRates,streamingMoviesChurnRates, nrow =1, top = "Churn analysis of most popular services")
}

# ************************************************
# contractAndPaymentChurnAnalysis() :
#
# Creates a grid of bar charts relating to the contracts, billing and payments 
# of the dataset
#
# INPUT:   Data Frame - dataset - dataset to be analysed
#
# OUTPUT : grid of bar charts in the plots tab
# ************************************************
contractAndPaymentChurnAnalysis <- function(dataset){
  
  #contract type churn plot
  contractCountAndChurnRates <- ggplot(dataset, aes(x=Contract, fill=factor(Churn))) +
    geom_bar() +
    xlab("Contract type") +
    ylab("total count") + 
    labs(fill= "Churn")
  
  #contract type churn plot
  billingtypeCountAndChurnRates <- ggplot(dataset, aes(x=PaperlessBilling, fill=factor(Churn))) +
    geom_bar() +
    xlab("Paperless billing") +
    ylab("total count") + 
    labs(fill= "Churn")
  
  paymentMethodCountAndChurnRates <- ggplot(dataset, aes(x=PaymentMethod, fill=factor(Churn))) +
    geom_bar() +
    xlab("Payment method") +
    ylab("total count") + 
    labs(fill= "Churn")
  
  grid.arrange(contractCountAndChurnRates,billingtypeCountAndChurnRates,paymentMethodCountAndChurnRates, nrow =1, top = "Contract And Payment Churn Analysis")
  
  
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
main<-function(){
  #load dataset
  telco <- read.csv(file = './TelcoCustomerChurn.csv')
  #clean dataset
  telcoClean <- basicCleaning(telco)
  
  #basic understanding visualization
  importantChurnRates(telcoClean)
  
  CustomerCharacteristics(telcoClean)
  CustomerCharacteristicsChurnRates(telcoClean)
  
  analysisOfServices(telcoClean)
  churnAnalysisOfPopularServices(telcoClean)
  
  contractAndPaymentChurnAnalysis(telcoClean)
  
}

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#run main
main()






