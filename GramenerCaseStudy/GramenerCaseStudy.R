## EDA Group Case Study
# Setting wd
# setwd("F:/Education/IIIT/Docs/WorkSheets/0015 EDA Group Case Study/02 InProgress")

#importing libraries
library(stringr)
library(ggplot2)
library(reshape2)
#reading the dataset from csv
loan <- read.csv("loan.csv")

#analysing the data
str(loan)
summary(loan)
#NA - there are many columns having only NA or one constant value
#OUTLIERS - we found that there were outliers in multiple attributes below is few of them,during univariate analysis in tableau we removed them
#open_acc -	 for plotting charts we changed max_count to 30 from 44 for  
#delinq 2 yrs -	for plotting charts, we have removed entries for 10 and 11 becasue there was very less number of records 
#annual_inc -	have taken income upto 200,000 for univariate analysis because there was very less number of records 

#custom function : it takes dataframe as input and removes columns which contains only NA values or columns which contains only single value
fnRemoveEmptyColumns <- function(df){
  
  colsToBeDeleted <- c();
  resultDF <- df;
  
  for(columnName in names(df)){
    tempData <- df[,columnName]
    if(all(is.na(tempData))){
      colsToBeDeleted <- c(colsToBeDeleted, columnName)
    }
    
    if((length(unique(tempData)) == 1) & !(columnName %in% colsToBeDeleted) ){
      
      colsToBeDeleted <- c(colsToBeDeleted, columnName)
    }
  }
  
  resultDF <- df[ , -which(names(df) %in% colsToBeDeleted)]
  
  return(resultDF)
}

#**************************************** cleaning the data *****************************************************#

loan <- fnRemoveEmptyColumns(loan)

#below columns are not required for analysis, so removing it
colToBeDeleted <- c("member_id","emp_title","url")

loan <- loan[,-which(names(loan) %in% colToBeDeleted)]

#we need to do find attributes which are responsible for loan default based on charge-off and fully paid, 
#so we are removing the current status
loan <- subset(loan, loan_status != "Current")

#removing % symbol from interest_rate, revol_util and removed months string from term
loan$int_rate <- as.numeric(str_replace(loan$int_rate,"%",""))
loan$revol_util <- as.numeric(str_replace(loan$revol_util,"%",""))
loan$term  <- as.factor(str_replace(loan$term," months",""))


#*************************************************** Derived Columns ****************************************#

# divided annual income into 3 different categories based on the amount
# the following categories have been taken
# 1. Very Low Income : < 30K
# 2. Moderate Income : < 80K
# 3. High income : > 80K
loan$annual_inc_category <-  sapply(loan$annual_inc, function(x){ 
  if(x < 30000){ "very_Low_income" 
  } else if(x < 80000){ "moderate_Income" 
  } else{"High_Income"}})

#created percentage of installment/monthly_income
loan$monthly_inc <- round(loan$annual_inc/12.0,0)
loan$Percent_installment_monthly_inc <- round((loan$installment/loan$monthly_inc)*100,0)

ggplot(loan, aes(x="", y=Percent_installment_monthly_inc)) + geom_boxplot(col="darkblue")

#write.csv(loan, "loanClean.csv")

#****************************************** Univariate Analysis *********************************************#

#PLOT : Annual_Income_category for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "annual_inc_category")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=annual_inc_category)) +  
  geom_bar( stat="identity", position="fill") 

#PLOT : Verification Status for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "verification_status")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=verification_status)) +  
  geom_bar( stat="identity") 

#PLOT : Grade for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "grade")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=grade)) +  
  geom_bar( stat="identity", position="fill") 

#PLOT : Grade for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "grade")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=grade)) +  
  geom_bar( stat="identity", position="fill") 

#PLOT : Loan Amount for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "loan_amnt")]
ggplot(data=dfTemp, aes(fill=loan_status, x=loan_amnt)) + 
  geom_histogram(bins=8, position = "fill")

#PLOT : Home Ownership for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "home_ownership")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=home_ownership)) +  
  geom_bar( stat="identity", position="fill") 

#PLOT : Term for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "term")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=term)) +  
  geom_bar( stat="identity", position="fill") 

#PLOT : Interest Rate for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "int_rate")]
ggplot(data=dfTemp, aes(fill=loan_status, x=int_rate)) + 
  geom_histogram(bins=7, position = "fill")

#PLOT : Open Account for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "open_acc")]
ggplot(data=dfTemp, aes(fill=loan_status, x=open_acc)) + 
  geom_histogram(bins=7, position = "fill")

#PLOT : delinq_2yrs for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "delinq_2yrs")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=delinq_2yrs)) +  
  geom_bar( stat="identity", position="fill") 

#PLOT : Addr_state for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "addr_state")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=addr_state)) +  
  geom_bar( stat="identity") 

#PLOT : Purpose for  Loan Status and id
dfTemp <- loan[,c("id" , "loan_status", "purpose")]
ggplot(dfTemp, aes(fill=loan_status, y=id, x=purpose)) +  
  geom_bar( stat="identity") + theme(axis.text.x=element_text(angle=90, hjust=1))

#PLOT : Percentage of Insallment on MOnthly income vs Loan Status
dfTemp <- loan[,c("id" , "loan_status", "Percent_installment_monthly_inc")]
ggplot(data=dfTemp, aes(fill=loan_status, x=Percent_installment_monthly_inc)) + 
  geom_histogram(bins=5, position = "fill") 

#************* Bivariate ANalysis ********************#

#PLOT :  correlation matrix and heat map
corDF <- loan[,c("loan_amnt","funded_amnt","funded_amnt_inv","installment","annual_inc","dti","open_acc","total_rec_late_fee","delinq_2yrs")]
cormat <-  round(cor(corDF),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ scale_fill_gradient(low = "red", high = "green")

#PLOT : home_ownership vs Annual_Income_category vs Loan Status

homeLoan <- subset(loan, home_ownership != "NONE")
ggplot(homeLoan, aes(x = homeLoan$home_ownership)) + geom_bar( stat = "count",position = "fill", aes(fill =homeLoan$loan_status)) + facet_wrap( ~ homeLoan$annual_inc_category) +theme(axis.text.x=element_text(angle=90, hjust=1))

#PLOT : home_ownership vs Percent_installment_monthly_inc vs Loan Status
ggplot(homeLoan, aes(x = homeLoan$Percent_installment_monthly_inc)) + geom_bar( stat = "count",position = "fill", aes(fill =homeLoan$loan_status)) + facet_wrap( ~ homeLoan$home_ownership) +theme(axis.text.x=element_text(angle=90, hjust=1))

#PLOT : Percent_installment_monthly_inc vs Home owner vs EmpLength vs Loan_Status For Charged off
charged_off <- subset(homeLoan, loan_status == "Charged Off")
ggplot(charged_off, aes(x = charged_off$Percent_installment_monthly_inc)) + 
  geom_bar(stat = "count", aes(fill = charged_off$emp_length)) + facet_wrap( ~ charged_off$home_ownership) 

#PLOT :  Purpose vs home_ownership vs Loan Status
ggplot(homeLoan, aes(x = homeLoan$purpose)) + geom_bar( stat = "count", aes(fill =homeLoan$loan_status)) + facet_wrap( ~homeLoan$home_ownership) +theme(axis.text.x=element_text(angle=90, hjust=1))


#PLOT : Emp_length vs Term Vs Loan_Status
ggplot(loan, aes(x = loan$emp_length)) + geom_bar(stat = "count", aes(fill = loan$loan_status)) + facet_wrap( ~ loan$term) 


#PLOT : Emp_Length vs Annual Income Category
ggplot(loan, aes(x = loan$emp_length)) + geom_bar(stat = "count", aes(fill = loan$loan_status)) + facet_wrap( ~ loan$annual_inc_category)  +theme(axis.text.x=element_text(angle=90, hjust=1))


#PLOT : Emp_length vs Term vs Annual_Income_category vs Loan_Status
ggplot(loan, aes(x = loan$emp_length)) + geom_bar( aes(fill =loan$loan_status)) + facet_wrap( loan$term ~ loan$annual_inc_category) +theme(axis.text.x=element_text(angle=90, hjust=1))


