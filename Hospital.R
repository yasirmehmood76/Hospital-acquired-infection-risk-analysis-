rm(list=ls()) # used to clear the environment
hospital <- read.table("hw4Q1.txt",header=F,col.names=c( "N", "X1","X2","Y" ,"X3","X4", "X5", "X6","X7","X8","X9","X10"))
hospital<-  hospital[,-1]
attach(hospital)
hospital
hospital.model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + factor(X6) + factor(X7) + X8 + X9 + X10)
summary(hospital.model)
#--------------------- best subset and all possible models------------------------
library(leaps)
hospitalx<-hospital[,-3]
# leaps: criteria = ("Cp", "adjr2", "r2") with Mallow's Cp the default
cp3<- leaps(hospitalx, hospital$Y, nbest=3)
cp3<- regsubsets(Y ~ X1 + X2 + X3 + X4 + X5 + factor(X6) + factor(X7) + 
                   X8 + X9 + X10, data = hospital, nbest=3, nvmax=12)
summary.cp3<-summary(cp3)
summary.cp3
with(summary.cp3,round(cbind(which,cp),3))
summary.cp3$cp
min(summary.cp3$cp)
hospital.model <- lm(Y ~ X1 + X3 + X4 + factor(X6) + factor(X7) + X10)
summary(hospital.model)
#--------------------------Part (3) Forward Stepwise regresion -------------------------------
full <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + factor(X6) + factor(X7) + X8 + X9 + X10, data=hospital)
null <- lm(Y ~ 1, data=hospital)
forward.step <- step(null,direction="both",scope=list(upper=full,lower=null))
summary(forward.step)
#--------------------------------Part (4) backward stepwise ---------------------------------
backward.step <- step(full,direction="both",scope=list(upper=full,lower=null))
summary(backward.step)
#------------------------- Part (5) forward addition ---------------------------------------
forward <- step(null,direction="forward",scope=list(upper=full,lower=null))
summary(forward)
#------------------------------ Part (6) backward elimination -------------------------------
backward <- step(full,direction="backward")
summary(backward)
#--------------------------------Question 02-------------------------------------------------
rm(list=ls()) # used to clear the environment
commercial <- read.table("CH06PR18.txt",header=F,col.names=c("Y", "X1","X2","X3", "X4"))
attach(commercial)
commercial.mod123 <- lm(Y ~ X1 + X2 + X3 + X4)  # Multiple Regression of Y on X1,X2,X3
plot(commercial)
cor(commercial)
#install.packages("DAAG")
library(DAAG)                # VIF option included in DAAG package
(bf.vif <- vif(commercial.mod123))      # Obtain VIF1,VIF2,VIF3
commercial.mod123 <- lm(Y ~ X1 + X2 + X3 + X4)  # Multiple Regression of Y on X1,X2,X3
summary(commercial.mod123)
library(MASS)
lambda = seq(0,10,0.1)
model.ridge <- lm.ridge(Y ~ ., data=commercial, lambda=lambda)
plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")
# optimal lambda
lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]
lambda.ridge
# ridge estimator
ridge.est <- coef(model.ridge)[which(lambda==lambda.ridge),]
ridge.est
# compare ridge estimator with OLS
(cbind(ridge.est,coef(commercial.mod123)))
# plot the coefficients and see how they vary as a function of lambda
colors <- rainbow(4)
matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",
        xlab=expression(lambda), ylab=expression(hat(beta)), 
        col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 4), coef(model.ridge)[length(seq(0,10,0.1)),-1], 
     colnames(commercial)[-1], pos=4, col=colors)
(beta.ridge <- coef(model.ridge)[which.min(model.ridge$GCV),])

