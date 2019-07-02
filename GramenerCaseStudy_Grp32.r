# --------------------------------------------------------------#
# Description : PGDDA - Gramener - Loan default Case Study #
# Authors          : Aman Raj    								#
#                  : Gaurav      								#
#                  : Mukhund							        #
#---------------------------------------------------------------#

#****************************************Gramener Case Study********************************************#

#Assign the directory for the case study

setwd("C:/PGDDS/Graemener case Study")

#Loading ggplot packages
#loading GGally package

library(ggplot2)
library(GGally)


#*********************************************************************************************************#
#*****************************Checkpoint-1: Data Cleaning and Preparation*********************************#
#*********************************************************************************************************#

#Loading the file loan.csv in r consol

loan_df <- read.csv("loan.csv",stringsAsFactors = FALSE,na.strings=c("", " ", "NA", "N/A","n/a"))

str(loan_df)

# Any NA values in loan status or not
num=sum(is.na(loan_df$loan_status))

loan_df$loan_status<- trimws(tolower(loan_df$loan_status))

# Removing "current" as our analysis will focus on "charged  off" & "fully paid"

loan_df<-loan_df[-which(loan_df$loan_status == "current"), ]

#*********************************************************************************************************#
#********************************************Business Rule************************************************#
#*********************************************************************************************************#

#since the company has decided to work only on driver variable we will 
#subset the loan file with driver variable and some additional information

loan_drv_df <- subset(loan_df,select = c(id,annual_inc,loan_amnt,funded_amnt,int_rate,grade,dti,emp_length,purpose,home_ownership,loan_status,purpose,verification_status,delinq_2yrs,revol_util,pub_rec,pub_rec_bankruptcies,open_acc))

#*********************************************************************************************************#
#***************************************Checking Granularity**********************************************#
#*********************************************************************************************************#

#we will check for any dupliacte values in our driver file, since ID is a unique ID assigned
#we will use it for checking any duplicate entries

# Counting the number of records in the data
loan_drv_nrow<-nrow(loan_drv_df)

# Checking for duplicated records
loan_drv_unrow<-nrow(unique(loan_drv_df))
if(loan_drv_nrow==loan_drv_unrow) {print("No duplicated Records")} else {print("Duplicated records.Need Cleaning")}

#*********************************************************************************************************#
#******************************************Preparing the Data*********************************************#
#*********************************************************************************************************#

#now we will check for any NA values in our driver variable 

loan_drv_df$id <- trimws(tolower(loan_drv_df$id))
loan_drv_df$home_ownership <- trimws(toupper(loan_drv_df$home_ownership))
loan_drv_df$grade <- trimws(toupper(loan_drv_df$grade))

sum(is.na(loan_drv_df)) #1780 values found

sum(is.na(loan_drv_df$id)) #no NA found

sum(is.na(loan_drv_df$annual_inc)) #no NA found

sum(is.na(loan_drv_df$home_ownership))  #no NA found

sum(is.na(loan_drv_df$loan_amnt)) #no NA found

sum(is.na(loan_drv_df$funded_amnt)) #no NA found

loan_drv_df$int_rate<- trimws(tolower(loan_drv_df$int_rate))

sum(is.na(loan_drv_df$int_rate)) #no NA found

loan_drv_df$int_rate <- as.numeric(gsub("%", "", loan_drv_df$int_rate))

sum(is.na(loan_drv_df$grade)) #no NA found

sum(is.na(loan_drv_df$dti))#no NA found

sum(is.na(loan_drv_df$purpose)) #no NA found

sum(is.na(loan_drv_df$delinq_2yrs)) #no NA found

sum(is.na(loan_drv_df$verification_status))#no NA found

loan_drv_df$revol_util <- trimws(tolower(loan_drv_df$revol_util))

sum(is.na(loan_drv_df$revol_util)) # 50 NA values found

loan_drv_df$revol_util <- as.numeric(gsub("%", "", loan_drv_df$revol_util))# converting into numeric

#Revolving line Utilization NA values are set to the median

median(loan_drv_df$revol_util,na.rm = TRUE) # 49.1

loan_drv_df$revol_util[ which(is.na(loan_drv_df$revol_util)) ] = median(loan_drv_df$revol_util,na.rm = TRUE)

#Dealing with NA values in emp_lengths

loan_drv_df$emp_length <- trimws(tolower(loan_drv_df$emp_length))

sum(is.na(loan_drv_df$emp_length)) # 1033 values found

table(loan_drv_df$emp_length)

# since maximum instances are there for 10+ years making all "n\a" values as "10+ years"

loan_drv_df$emp_length[ which(is.na(loan_drv_df$emp_length)) ]="10+ years"

table(loan_drv_df$emp_length)

#processing for bankruptcies

sum(is.na(loan_drv_df$pub_rec_bankruptcies))

table(loan_drv_df$pub_rec_bankruptcies) 

# since 0 number of bankruptcies has most frequency we set NA values to 0

loan_drv_df$pub_rec_bankruptcies[ which(is.na(loan_drv_df$pub_rec_bankruptcies)) ]=0

table(loan_drv_df$pub_rec_bankruptcies)

#processing for derogatory records

sum(is.na(loan_drv_df$pub_rec))

table(loan_drv_df$pub_rec) 


################# Checking and removing outliers of Annual income column #################

boxplot(loan_drv_df$annual_inc)
firstquad <- quantile(loan_drv_df$annual_inc,0.25)
thirdquad<-quantile(loan_drv_df$annual_inc,0.75)
upperlmtforout<-quantile(loan_drv_df$annual_inc,0.75)+1.5*(thirdquad-firstquad)
lowerlmtforout<-quantile(loan_drv_df$annual_inc,0.25)-1.5*(thirdquad-firstquad) # since it is negative npt conisdered
loan_drv_df<-subset(loan_drv_df,loan_drv_df$annual_inc<=upperlmtforout) # Ignoring values above 99 percent quantile
boxplot(loan_drv_df$annual_inc)

################# Checking and removing outliers of Funded Amount column #################
boxplot(loan_drv_df$funded_amnt)
firstquad1 <- quantile(loan_drv_df$funded_amnt,0.25)
thirdquad1<-quantile(loan_drv_df$funded_amnt,0.75)
upperlmtforout1<-quantile(loan_drv_df$funded_amnt,0.75)+1.5*(thirdquad1-firstquad1)
lowerlmtforout1<-quantile(loan_drv_df$funded_amnt,0.25)-1.5*(thirdquad1-firstquad1) # since it is negative npt conisdered
loan_drv_df<-subset(loan_drv_df,loan_drv_df$funded_amnt<=upperlmtforout1) # Ignoring values above 99 percent quantile
boxplot(loan_drv_df$annual_inc)

#Classifying dti into different levels

loan_drv_df$dti_lvl = rep(1,nrow(loan_drv_df))
loan_drv_df$dti_lvl[ which( loan_drv_df$dti <=10) ] = "Low"
loan_drv_df$dti_lvl[ which( loan_drv_df$dti > 10 & loan_drv_df$dti <=20 ) ] = "Medium"
loan_drv_df$dti_lvl[ which( loan_drv_df$dti >20) ] = "High"

#Classifying income into different levels

loan_drv_df$income_lvl = rep(1,nrow(loan_drv_df))
loan_drv_df$income_lvl[ which( loan_drv_df$annual_inc <=40000) ] = "Low"
loan_drv_df$income_lvl[ which( loan_drv_df$annual_inc > 40000 & loan_drv_df$annual_inc <=80000 ) ] = "Medium"
loan_drv_df$income_lvl[ which( loan_drv_df$annual_inc > 80000 & loan_drv_df$annual_inc <=120000) ] = "High"
loan_drv_df$income_lvl[ which( loan_drv_df$annual_inc > 120000) ] = "Affluent"

#filter on loan status->"charged off"
loan_drv_chrg_df<-loan_drv_df[-which(loan_drv_df$loan_status == "fully paid"), ]

#filter on loan status->"fully paid"
loan_drv_paid_df<-loan_drv_df[-which(loan_drv_df$loan_status == "charged off"), ]


#*********************************************************************************************************
#***********************************Checkpoint 2: Exploratory Data Analysis*******************************
#*********************************************************************************************************


#************* Loan Verification vs Status(Histogram) *******************

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$verification_status), fill = loan_drv_df$loan_status)) +geom_bar(position = "dodge") + labs(title = "Loan_Verification vs Loan_Status",x = "Loan Verification Status",y = "Count")


#******************* Loan Grade vs Status(Histogram) *****************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$grade), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") +  labs(title = "Loan_Grade vs Loan_Status",x = "Loan Grade",y = "Count")


#******************* DTI Level vs Status(Histogram) *****************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$dti_lvl), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") + labs(title = "DTI_Level vs Loan_Status",x = "DTI Level",y = "Count")


#************* Purpose vs Status(Histogram) *******************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$purpose), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") + labs(title = "Loan_Purpose vs Loan_Status",x = "Loan Purpose",y = "Count")

#******************Ownership of home vs Status (Histogram) ******************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$home_ownership), fill = loan_drv_df$loan_status)) +geom_bar(position = "dodge") + labs(title = "Home Ownership vs Loan_Status",x = "Home Ownership",y = "Count")

#******************Delinquency within two years vs Status (Histogram)******************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$delinq_2yrs), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") + labs(title = "Delinquency Instances vs Loan_Status",x = "Delinquency Instances",y = "Count")


#******************* Income Level vs Status(Histogram) *****************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$income_lvl), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") + labs(title = "Income_Level vs Loan_Status",x = "Income Level",y = "Count")


#******************* No of Derogatory records vs Status(Histogram) *****************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$pub_rec), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") + labs(title = "No of Derogatory records vs Loan_Status",x = "No of Derogatory records",y = "Count")


#******************* No of Public Bankruptcies vs Status(Histogram) *****************#

ggplot(loan_drv_df, aes(x=factor(loan_drv_df$pub_rec_bankruptcies), fill = loan_drv_df$loan_status)) +geom_bar(position = "dodge") + labs(title = "No of Public Bankruptcies vs Loan_Status",x = "No of Public Bankruptcies",y = "Count")


#******************* No of open public accounts vs Status(Histogram) *****************#
ggplot(loan_drv_df, aes(x=factor(loan_drv_df$open_acc), fill = loan_drv_df$loan_status)) + geom_bar(position = "dodge") + labs(title = "Total no of open credit accounts vs Loan_Status",x = "Total no of open credit accounts ",y = "Count")


#*******************  "Income vs Status" Jittr Plot *****************#
ggplot(loan_drv_df, aes(x=factor(loan_drv_df$loan_status), y= loan_drv_df$annual_inc)) + geom_jitter(position = position_jitter(width = 0.6)) +labs(title = "Income vs Loan Status",x = "Income",y = "Loan_Status")

#*********************************************************************************************************#
#*******************************************Univariate Analysis*******************************************#
#*********************************************************************************************************#


#******************Employee Service Length#******************#
ggplot(loan_drv_chrg_df,aes(emp_length))+geom_bar(fill='Blue')+ylim(0,5000)+ggtitle('Employee Service Length (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(emp_length))+geom_bar(fill='Red')+ylim(0,8000)+ggtitle('Employee Service Length (Loan Paid Off)')

table(loan_drv_chrg_df$emp_length)
table(loan_drv_paid_df$emp_length)

#******************Ownership of home******************#
ggplot(loan_drv_chrg_df,aes(home_ownership))+geom_bar(fill='Blue')+ylim(0,4000)+ggtitle('Home Ownership (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(home_ownership))+geom_bar(fill='Red')+ylim(0,18000)+ggtitle('Home Ownership (Loan Paid Off)')

table(loan_drv_chrg_df$home_ownership)
table(loan_drv_paid_df$home_ownership)

#******************Purpose******************#
ggplot(loan_drv_chrg_df,aes(purpose))+geom_bar(fill='Blue',width=0.6)+ylim(0,5000)+ggtitle('Loan Purpose (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(purpose))+geom_bar(fill='Red',width=0.6)+ylim(0,8000)+ggtitle('Loan Purpose (Loan Paid Off)')

table(loan_drv_chrg_df$purpose)
table(loan_drv_paid_df$purpose)

#******************DTI_level******************#
ggplot(loan_drv_chrg_df,aes(dti_lvl))+geom_bar(fill='Blue')+ylim(0,5000)+ggtitle('DTI (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(dti_lvl))+geom_bar(fill='Red')+ylim(0,20000)+ggtitle(' DTI(Loan Paid Off)')

table(loan_drv_chrg_df$dti_lvl)
table(loan_drv_paid_df$dti_lvl)

#******************Income_level******************#
ggplot(loan_drv_chrg_df,aes(income_lvl))+geom_bar(fill='Blue',width=0.6)+ylim(0,5000)+ggtitle('Income Level (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(income_lvl))+geom_bar(fill='Red',width=0.6)+ylim(0,20000)+ggtitle('Income Level(Loan Paid Off)')

table(loan_drv_chrg_df$income_lvl)
table(loan_drv_paid_df$income_lvl)

#******************Verification Status******************#
ggplot(loan_drv_chrg_df,aes(verification_status))+geom_bar(fill='Blue',width=0.6)+ylim(0,5000)+ggtitle('Verification Status (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(verification_status))+geom_bar(fill='Red',width=0.6)+ylim(0,20000)+ggtitle('Verification Status (Loan Paid Off)')

table(loan_drv_chrg_df$verification_status)
table(loan_drv_paid_df$verification_status)

#******************Delinquency within two years******************#
ggplot(loan_drv_chrg_df,aes(delinq_2yrs))+geom_bar(fill='Blue',width=0.6)+ylim(0,5000)+ggtitle('Delinquency within 2 years (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(delinq_2yrs))+geom_bar(fill='Red',width=0.6)+ylim(0,20000)+ggtitle('Delinquency within 2 years (Loan Paid Off)')

table(loan_drv_chrg_df$delinq_2yrs)
table(loan_drv_paid_df$delinq_2yrs)

#******************Grade******************#
ggplot(loan_drv_chrg_df,aes(grade))+geom_bar(fill='Blue',width=0.6)+ylim(0,5000)+ggtitle('Borrower Grade (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(grade))+geom_bar(fill='Red',width=0.6)+ylim(0,20000)+ggtitle('Borrower Grade (Loan Paid Off)')

table(loan_drv_chrg_df$grade)
table(loan_drv_paid_df$grade)

#******************Public Number of Bankruptcies******************#
ggplot(loan_drv_chrg_df,aes(pub_rec_bankruptcies))+geom_bar(fill='Blue',width=0.6)+ylim(0,10000)+ggtitle('Public Number of Bankruptcies (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(pub_rec_bankruptcies))+geom_bar(fill='Red',width=0.6)+ylim(0,35000)+ggtitle('Public Number of Bankruptcies (Loan Paid Off)')

table(loan_drv_chrg_df$pub_rec_bankruptcies)
table(loan_drv_paid_df$pub_rec_bankruptcies)

#******************Public Number of Derogatory Records******************#
ggplot(loan_drv_chrg_df,aes(pub_rec))+geom_bar(fill='Blue',width=0.6)+ylim(0,10000)+ggtitle('Public Number of Derogatory Records (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(pub_rec))+geom_bar(fill='Red',width=0.6)+ylim(0,35000)+ggtitle('Public Number of Derogatory Records (Loan Paid Off)')

table(loan_drv_chrg_df$pub_rec)
table(loan_drv_paid_df$pub_rec)

#******************Total number of open credit records******************#
ggplot(loan_drv_chrg_df,aes(open_acc))+geom_bar(fill='Blue',width=0.6)+ylim(0,1000)+ggtitle('Total no of open credit accounts (Loan Defaulted)')
ggplot(loan_drv_paid_df,aes(open_acc))+geom_bar(fill='Red',width=0.6)+ylim(0,5000)+ggtitle('Total no of open credit accounts  (Loan Paid Off)')

table(loan_drv_chrg_df$open_acc)
table(loan_drv_paid_df$open_acc)


#*********************************************************************************************************#
#******************************************Multivariate Analysis******************************************#
#*********************************************************************************************************#

#now we want to find out if there is any corelation exist between any of the continuous variables

#we will first subset our data set with only continuous variables

cont_loan  <- subset(loan_drv_df,select = c(annual_inc,funded_amnt,int_rate,open_acc,dti,revol_util,delinq_2yrs))

#now we will use the package ggally to analyse correlation between different variables

ggpairs(data=cont_loan,columns=1:3, title="Correlation between Annual Income,Funded Amount and Interest Rate")

ggpairs(data=cont_loan,columns=4:7, title="Correlation between Number of Credit accounts,DTI,Revolving Utilization Rate and Delinquencies in 2 yrs")

#using Correlation Matrix cor()

cont_loan1 <- subset(loan_drv_df,select = c(annual_inc,funded_amnt,int_rate,open_acc,dti,revol_util,delinq_2yrs,pub_rec,pub_rec_bankruptcies))

cor4csv<-cor(cont_loan1)

View(cor4csv)

#Correlation betwen Funded_Amount and Loan_amount,we can also see this with the help of a Jitter plot

plot1 <- ggplot(loan_drv_df,aes(x=loan_amnt,y=funded_amnt,col = loan_status)) + geom_point(alpha = 0.5)+ geom_jitter()

plot1 + labs(title = "Correlation between Funded Amount and Loan Amount",x = "Loan Amount",y = "Funded Amount")

#Correlation betwen Loan_amount and Purpose of loan,we can also see this with the help of a Jitter plot

plot2<- ggplot(loan_drv_df, aes(x = loan_amnt, y = purpose, col = loan_status )) + geom_point( alpha = 0.2 ) + geom_jitter()

plot2+ labs(title = "Correlation between Loan Amount and Purpose ",x = "Loan Amount",y = "Purpose")

#Correlation between Interest Rate and Grade of loan,we can also see this with the help of a Jitter plot

plot3<- ggplot(loan_drv_df, aes(x = int_rate, y = grade, col = loan_status )) + geom_point( alpha = 0.2 ) + geom_jitter()

plot3+ labs(title = "Correlation between Interest Rate and Grade ",x = "Interest Rate",y = "Grade")

#Correlation between Interest Rate and Grade of loan,we can also see this with the help of a Jitter plot

plot4<- ggplot(loan_drv_df, aes(x = open_acc, y = delinq_2yrs, col = loan_status )) + geom_point( alpha = 0.2 ) + geom_jitter()

plot4+ labs(title = "Correlation between No of Credit accounts opened and Deliquencies in 2yrs ",x = "No of Credit accounts opened",y = "Deliquencies in 2yrs")

