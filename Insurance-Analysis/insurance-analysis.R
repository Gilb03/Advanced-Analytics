# ECO520 FINAL PROJECT KING - dont touch this until tomorrow or until professor answers
library(dplyr)
library(knitr)
setwd("~/zrc/Insurance-Analysis/data")


#LOAD DATASET 
train <- read.csv("Train-1542865627584.csv")
train_outpatient <- read.csv("Train_Outpatientdata-1542865627584.csv")
train_inpatient <- read.csv("Train_Inpatientdata-1542865627584.csv")
train_beneficiary <- read.csv("Train_Beneficiarydata-1542865627584.csv")
#LOAD TEST DATASET ( I could get rid of this)
test <- read.csv("Test-1542969243754.csv")
test_outpatient <- read.csv("Test_Outpatientdata-1542969243754.csv")
test_inpatient <- read.csv("Test_Inpatientdata-1542969243754.csv")
test_beneficiary <- read.csv("Test_Beneficiarydata-1542969243754.csv")




#CLEAN DATASET(S)
main <- train %>%
  mutate(PotentialFraud = if_else(PotentialFraud == "No", 0, 1))





#TRAIN DESCRIPTIVE STATISTICS
str(main)
head(main)
tail(main)
summary(main)
by(main, main$PotentialFraud, summary)
hist(main$PotentialFraud)



str(train_outpatient)
head(train_outpatient)
tail(train_outpatient)
summary(train_outpatient)
by(train_outpatient, train_outpatient$InscClaimAmtReimbursed, summary)
by(train_outpatient, train_outpatient$DeductibleAmtPaid, summary)
hist(train_outpatient$ClmProcedureCode_1)
hist(train_inpatient$ClmProcedureCode_2)
plot(train_outpatient$InscClaimAmtReimbursed ,train_outpatient$DeductibleAmtPaid, main="Scatter plot of claim reimbursed vs dedutible paid", col="blue")
abline(lm(train_outpatient$InscClaimAmtReimbursed~train_outpatient$DeductibleAmtPaid), col="red")



str(train_inpatient)
head(train_inpatient)
tail(train_inpatient)
summary(train_inpatient)
by(train_inpatient, train_inpatient$InscClaimAmtReimbursed, summary)
by(train_inpatient, train_inpatient$DeductibleAmtPaid, summary)
hist(train_inpatient$InscClaimAmtReimbursed)
hist(train_inpatient$ClmProcedureCode_1)
hist(train_inpatient$ClmProcedureCode_2)
hist(train_inpatient$ClmProcedureCode_3)
hist(train_inpatient$ClmProcedureCode_4)




str(train_beneficiary)
head(train_beneficiary)
tail(train_beneficiary)
summary(train_beneficiary)
by(train_beneficiary, train_beneficiary$Gender)
hist(train_beneficiary$Gender)
hist(train_beneficiary$ChronicCond_Alzheimer)
hist(train_beneficiary$ChronicCond_Heartfailure)
hist(train_beneficiary$ChronicCond_KidneyDisease)
hist(train_beneficiary$ChronicCond_Cancer)
hist(train_beneficiary$ChronicCond_Depression)
hist(train_beneficiary$ChronicCond_Diabetes)




#TEST DESCRIPTIVE STATISTICS
str(test)
head(test)
tail(test)
summary(test)
by(test, test$Provider, summary)


str(test_outpatient)
head(test_outpatient)
tail(test_outpatient)
summary(test_outpatient)
by(test_outpatient, test_outpatient$Provider, summary)

str(test_inpatient)
head(test_inpatient)
tail(test_inpatient)
summary(test_inpatient)
by(test_inpatient, test_inpatient$Provider, summary)

str(test_beneficiary)
head(test_beneficiary)
tail(test_beneficiary)
summary(test_beneficiary)



#VISUALIZATIONS
hist(main$PotentialFraud, main = "Potential Fraud Distributions", xlab="YES = 1 || NO =0", ylab="Insurance Provider")



# MERGE SETS 
patient_super <- merge(train_outpatient, train_inpatient, by="BeneID")

#LOGISTIC REGRESSION 

#LINEAR PROBABILITY MODEL 
#COMPARISON OF MODELS
#MODEL VISUALS AND EXPORT TO DASHBOARD