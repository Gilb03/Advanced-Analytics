#WEEK 9 KING, GILBERT

source("https://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R")
gse <- read.csv("https://bigblue.depaul.edu/jlee141/econdata/fannie_mae/Fannie_Mort_IL_2007.csv")
library("nnet")
str(gse)

FMindata <- subset(gse,select=-c(state,orgyear,fstimebuyer))
str(FMindata)

#TRANSFORM CATEGORY INTO NUMBERICAL
FMindata$purpose <- as.numeric(FMindata$purpose) - 1 
FMindata$relo_flg <- as.numeric(FMindata$relo_flg) - 1
indata$occ_stat <- as.numeric(indata$occ_stat)
indata$occ_stat_dum1 <- ifelse(indata$occ_stat == 1, 1,0)
indata$occ_stat_dum2 <- ifelse(indata$occ_stat == 2, 1,0)
indata$occ_stat_dum3 <- ifelse(indata$occ_stat == 3, 1,0)

str(FMindata)

FMzindata <- min_max_nor(FMindata)

#SPLIT DATA INTO TRAIN AND TEST DATA
set.seed(1937028)
train_ind <- sample(nrow(zindata), round(0.7*nrow(zindata)))
ztrain <- zindata[train_ind,]
ztest <- zindata[-train_ind,]
testy <- ztest$delinq

#NEURAL NETWORK
library(nnet)

#SIMPLE
ztrain$delinq <- as.factor(ztrain$delinq)
nnet1 <- neuralnet(delinq~., data = ztrain, hidden = 2)
#Error: the error derivative contains a NA; varify that the derivative 
#function does not divide by 0 (e.g. cross entropy)
plot(nnet1)
pred1 <- predict(nnet1, newdata=ztest)
pred1 <- pred1[,2]
conf_table(pred1,testy,"nnet1")
auc_plot(pred1,testy,"nnet1")

nnet2 <- neuralnet(delinq~.,data=ztrain,hhidden = c(5,3),stepmax = 1e+07)
plot(nnet2)
pred2 <- predict(nnet2,newdata=ztest)
pred2 <- pred2[,1]

nnet3 <- neuralnet(delinq~.,data=ztrain,hidden=c(7,5,3), stepmax = 1e+07)
plot(nnet3)
pred3 <- predict(nnet3,newdata=ztest)
pred3 <- pred3[,2]

nnet4 <- neuralnet(delinq~.,data = ztrain, hidden = c(9,7,5,3), stepmax = 1e+07)
plot(nnet4)
pred4 <- predict(nnet4,newdata=ztest)
pred4 <- pred4[,2]

par(mfrow=c(2,2))
auc_plot(pred1,testy,"nnet1")
auc_plot(pred2,testy,"nnet2")
auc_plot(pred3,testy,"nnet3")
auc_plot(pred4,testy,"nnet4")

#many errors found. Followed week 9 videos, please advise. nnet not found
#due to error, nothing works. 