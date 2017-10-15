source('RFunctions-1.R')
df=read.csv("auto_mpg.csv",stringsAsFactors = FALSE) # Data from UCI
df1 <- as.data.frame(sapply(df,as.numeric))

df2 <- df1 %>% select(cylinder,displacement, horsepower,weight, acceleration, year,mpg)
df3 <- df2[complete.cases(df2),]
train_idx <- trainTestSplit(df3,trainPercent=75,seed=5)
train <- df3[train_idx, ]
test <- df3[-train_idx, ]
# Fit the 
fit <- lm(mpg~. ,data=train)
summary(fit)

train=test
rsquared1=Rsquared(fit,test,test$mpg)
sprintf("R-squared for multi-variate regression is : %f", rsquared1)


library(leaps)
regfit.full=regsubsets(mpg~.,train)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
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


#Forward selection and backward selection
regfit.fwd=regsubsets(mpg~.,data=train,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(mpg~.,data=train,method="backward")
summary(regfit.bwd)
coef(regfit.full,6)
coef(regfit.fwd,6)
coef(regfit.bwd,6)


# Best fit
regfit.best=regsubsets(mpg~.,data=train)
val.errors=rep(NA,6)
for(i in 1:6){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((test$mpg-pred)^2)
}
val.errors
which.min(val.errors)

# Forward selection
regfit.fwd=regsubsets(mpg~.,data=train,method="forward")
val.errors=rep(NA,6)
for(i in 1:6){
    coefi=coef(regfit.fwd,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((test$mpg-pred)^2)
}
val.errors
which.min(val.errors)

# Backward selection
regfit.bwd=regsubsets(mpg~.,data=train,method="backward")
val.errors=rep(NA,6)
for(i in 1:6){
    coefi=coef(regfit.bwd,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((test$mpg-pred)^2)
}
val.errors
which.min(val.errors)

#############################################################################
#Boston
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
regfit.fwd=regsubsets(medv~.,data=train,nvmax=14,method="forward")
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
