source('RFunctions-1.R')
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
a=which.max(reg.summary$adjr2)
points(a,reg.summary$adjr2[a], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
b=which.min(reg.summary$cp)
points(b,reg.summary$cp[b],col="red",cex=2,pch=20)
c=which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(c,reg.summary$bic[c],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,14)

b=sqrt(reg.summary$rss/378)
plot(seq(1,14),b,type="b")

plot(c(1,4),b)
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

k = 10
set.seed(1)
folds = sample(1:k, nrow(df), replace=TRUE) 
# if an observation is labelled 'i', it will be part of the i-th fold.
# e.g. an observation that is labelled 3 will be left out of the 3rd round of cross validation
cv.errors = matrix(NA,k,13, dimnames=list(NULL, paste(1:8)))
# sets up a k by 13 matrix filled with NA's. 
# columns are named 1 to 14

# armed with our new predict() method, we can:
for (j in 1:k) {
    # obtain the required coefficients
    fitFwd=regsubsets(medv~.,data=train,nvmax=14,method="forward")
    # note: if you have more than 8 variables you will need to specify nvmax in the above command
    for (i in 1:14) {
        # now, calculate MSE using the i-th fold
        pred <- predict(best.fit, autosmall[folds == j,],id=i)
        # since best.fit is of class regsubsets, the predict command that was created will work on it.
        cv.errors[j,i] <- mean((autosmall$price[folds == j] - pred)^2)
    }
}
(mean.cv.errors <- apply(cv.errors,2,mean))

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


#############################K-fold
k = 5
set.seed(1)
folds = sample(1:k, nrow(df), replace=TRUE) 
rows=folds==2
df1<-df[rows,]
rows=folds==3
df2<-df[rows,]
fitFwd=regsubsets(medv~.,data=df[folds!=3,],nvmax=14,method="forward")
coefi=coef(fitFwd,id=2)
test=df[folds==3,]
test.mat=model.matrix(medv~.,data=test)
pred=test.mat[,names(coefi)]%*%coefi
mean((test$medv - pred)^2)
