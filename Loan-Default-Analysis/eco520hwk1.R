
# R is a calculator
12 + 34
12^2
1*2*3
log(100)
12^3

#define a number to a variable
a <- 10
b <- 20
a+b
a*b
c <- "hi eco520"
a + b + c
d<- substr(c,4,10)
d

#DataTypes
#3.1 vector
v1 <- c(1L,2L,3L,4L,5L)
v2 <- c(TRUE,FALSE,FALSE,FALSE,TRUE)
v3 <- c(12.3,21.5,34.2,33.35,34.0,)
v4 <- c("A","B","C","D","E")
v5 <- c(1L,TRUE,12.4,"A",1)

#3.2 matrix- 2-dimension
m1 <- matrix(1:15,5,3)
m2 <- cbind(v1, v2, v3, v4, v5)
colnames(m2) <- c("A","B","C","D","E")
rownames(m2) <- c("obj1","obj2","obj3","obj4","obj5")
m3 <- rbind(v1,v2,v3,v4,v5)

# array
a1 <- array(c(1:16),dim=c(4,4,2,2))

#list
l1 <- list(v1,v2,v3,v4,v5)


#data frame 
dat1 <- data.frame(v1,v2,v3,v4,v5)
str(dat1)


#4 data.frame operation
#create new variables

dat1$v6 <- dat1$v1^2
names(dat1)[6] <- "square of v1"

#dummy variable
dat1$cat1 <- ifelse(dat1$v3 >30,1,0 )
dat1$cat2 <- ifelse(dat1$v2 == TRUE,1,0 )

dat1$cat3 <- 0
dat1$cat3[v3 < 20] <- 1
dat1$cat3[v3 >=  20 & v3 < 30] <- 2
dat1$cat3[v3 >= 30] <- 3


#creating subset of data 
dat2 <- dat1[c("v1","v2")]
dat3 <- dat1[1:3,]
dat4 <- dat1[,1:3]
dat5 <- dat1[which(v2==TRUE)]
dat9 <- subset(dat1, v3 >20, select=c(cat1:cat3))
dat9 <- subset(dat1, select=c(cat1:cat3))
dat9 <- subset(dat1, select=-c(cat1:cat3))


#random sampling

set.seed(1234)
indata <- dat1
train_ind <- dat1
train_ind <- sample(nrow(indata), 0.6*nrow(indata))
train <- indata[train_ind]
test <- indata[-train_ind,]


#reading csv into r 
#cca <- read.csv("/var/www/html/jlee141/econdata/eco520/chicago_cca.csv")
cca <- read.csv("https://bigblue.depaul.edu/jlee141/econdata/eco520/chicago_cca.csv")
str(cca)
#clean data 
head(cca)
tail(cca)

cca$bclass <- 2
cca$blass[cca$Black > .5] <- 3
cca$blass[cca$Black > .2] <- 1
table(cca$bclass)

#descriptive stat
summary(cca)
summary(cca$Black)
table(cca$bclass)

#6.1 description by variable 
attach(cca)
tapply(Income,bclass,mean)
tapply(Income,bclass,median)

#graphs in r 
hist(Income,col="skyblue")
counts <- table(bclass)
barplot(counts,main="number of CA by class", col="skyblue")


plot(Unemp,Income, main="Scatter Unemployment vs. Income", col="red")
ablinelm(Income~Unemp,col="blue"))

#linear regression model 
#regression
ols1 <- lm(Income ~ Unemp, data=cca)
summary(ols1)

#regression with category
cca$bclass <- as.factor(cca$bclass)
ols2 <- lm(Income ~ Unemp + bclass, data=cca)
summary(ols2)

#multiple regression model 
ols3 <- lm(Income ~ Unemp + Black+ Hispanic, data=cca)
summary(ols3)


#stepwise regression model 
ols4 <- step(lm(Income ~ Unemp + Black+ Hispanic, data=cca), direction="both")
summary(ols4)

#forward or backword
ols5 <- step(lm(Income ~ Unemp + Black+ Hispanic, data=cca), direction="backward")
summary(ols5)


#sampling to train and test data 
set.seed(1234)
train_ind <- sample(nrow(cca), round(0.7*nrow(cca)))
train <-cca[train_ind,]
test <- cca[-train_ind,]



tols3 <- lm(Income ~ Unemp + Black+ Hispanic, data=train)
tols4 <- step(lm(Income ~ Unemp + Black+ Hispanic, data=train), direction="both")

py3 <- predict(tols3,newdta=test)
py4 <- predict(tols4,newdta=test)
pe3 <- predict(tols3,newdta=test)
pe4 <- predict(tols4,newdta=test)


mse3 <- mean(pe3^2)
mse4 <- mean(pe4^2)
rmse3 <-mse3^0.5
rmse4 <-mse4^0.5
print(c(mse3,mse4))

