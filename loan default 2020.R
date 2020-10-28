#BANK LOAN EXAMPLE
loan <- read.csv("https://bigblue.depaul.edu/jlee141/econdata/eco520/loan_default.csv")
source("https://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R")
summary(loan)
str(loan)

indata <- loan

#SPLIT INTO TRAIN VS TEST DATA 
train_idx <- sample(nrow(indata),round(.8*nrow(indata)))
train <- indata[train_idx,]
test <- indata[-train_idx,]
testy <- test$Default


#Linear Probability model 
lm0 <- lm(Default~ Credit_score + Age, data=train)
summary(lm0)
yhat0 <- predict(lm0, newdata=test)
conf_table(yhat0,testy,"LPM")
auc_plot(yhat0,testy,"LPM")


lm1 <- lm(Default~ . , data=train)
summary(lm1)
yhat1 <- predict(lm1, newdata=test)
conf_table(yhat1,testy,"LPM")
auc_plot(yhat1,testy,"LPM")

lm2 <- step(lm(Default~ . , data=train), direction = "both")
summary(lm2)
yhat2 <- predict(lm2, newdata=test)
conf_table(yhat2,testy,"LPM")
auc_plot(yhat2,testy,"LPM")


#LOGISTIC REGRESSION MODEL 
logit0 <- glm(formula=Default~Credit_score + Age, data=train, 
             family=binomial(link=logit))
summary(logit0)
loghat0 <- predict(logit0,newdata=test,type="response")
conf_table(loghat0,testy,"LOGIT")
auc_plot(loghat0,testy,"LOGIT")



logit1 <- glm(formula=Default~ . , data=train, 
              family=binomial(link=logit))
summary(logit1)
loghat1 <- predict(logit1,newdata=test,type="response")
conf_table(loghat1,testy,"LOGIT")
auc_plot(loghat1,testy,"LOGIT")




logit2 <- step(glm(formula=Default~ . , data=train, 
              family=binomial(link=logit)), direction = "both")
summary(logit2)
loghat2 <- predict(logit2,newdata=test,type="response")
conf_table(loghat2,testy,"LOGIT")
auc_plot(loghat2,testy,"LOGIT")


# PUT ALL GRAPOHS RTOGETHER TO COMPARE THEM
par(mfrow=c(2,3))
auc_plot(yhat0,testy,"LPM")
auc_plot(yhat1,testy,"LPM")
auc_plot(yhat2,testy,"LPM")
auc_plot(loghat0,testy,"LOGIT")
auc_plot(loghat1,testy,"LOGIT")
auc_plot(loghat2,testy,"LOGIT")
par(mfrow=c(1,1))


# FINAL DECISION 
dec_lpm <- ifelse(yhat1 > .3, 1, 0)
dec_logit  <- ifelse(loghat2 > .3, 1, 0)

table(testy,dec_lpm)
table(testy,dec_logit)

# SAVE THE ALGORITHM 
saveRDS(lm1, "zrc/Advanced_StatisticsII/LOAN_LPM.rds")
saveRDS(logit2, "zrc/Advanced_StatisticsII/LOAN_LOG.rds")
