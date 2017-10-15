source('RFunctions-1.R')

#############################################################################
#Boston
library(leaps)
library(boot)
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
train_idx <- trainTestSplit(df,trainPercent=75,seed=5)
train <- df[train_idx, ]
test <- df[-train_idx, ]

# Fit the 
fit <- lm(medv~. ,data=train)
summary(fit)

# Best fit
regfit.full=regsubsets(medv~.,train,nvmax=14)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

# Forward selection
#http://rbyexamples.blogspot.in/2015/07/linear-regression.html
fitFwd=regsubsets(medv~.,data=train,nvmax=14,method="forward")



val.errors=rep(NA,14)
test.mat=model.matrix(medv~.,data=test)
for(i in 1:14){
    coefi=coef(regfit.fwd,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((test$medv-pred)^2)
}


val.errors
which.min(val.errors)
regFwd.summary=summary(regfit.fwd)
plot(regFwd.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(regFwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

# Backward selection
regfit.bwd=regsubsets(medv~.,data=train,nvmax=14,method="backward")
val.errors=rep(NA,6)
test.mat=model.matrix(medv~.,data=test)
for(i in 1:6){
    coefi=coef(regfit.bwd,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((test$medv-pred)^2)
}
val.errors
which.min(val.errors)

regBwd.summary=summary(regfit.bwd)
plot(regBwd.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(regBwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
