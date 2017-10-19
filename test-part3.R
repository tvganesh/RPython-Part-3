source('RFunctions-1.R')
#Boston
library(leaps)
library(boot)
library(dplyr)
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
names(df) <-c("no","crimeRate","zone","indus","charles","nox","rooms","age",
              "distances","highways","tax","teacherRatio","color","status","cost")
df1 <- df %>% dplyr::select("crimeRate","zone","indus","charles","nox","rooms","age",
                            "distances","highways","tax","teacherRatio","color","status","cost")

bestFit <-regsubsets(cost~.,df1,nvmax=13)
reg.summary=summary(bestFit)

reg.summary$rsq

plot(bestFit$rss,xlab="Number of Variables",ylab="RSS",type="l")
a=which.max(reg.summary$adjr2)
points(a,reg.summary$adjr2[a], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
b=which.min(reg.summary$cp)
points(b,reg.summary$cp[b],col="red",cex=2,pch=20)
c=which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(c,reg.summary$bic[c],col="red",cex=2,pch=20)
plot(bestFit,scale="r2",main="Rsquared vs No Features")
plot(bestFit,scale="Cp",main="Cp vs NoFeatures")
plot(bestFit,scale="bic",main="BIC vs Features")
coef(bestFit,13)

###################################################################
# Forward selection
#http://rbyexamples.blogspot.in/2015/07/linear-regression.html


train_idx <- trainTestSplit(df1,trainPercent=75,seed=5)
train <- df1[train_idx, ]
test <- df1[-train_idx, ]
fitFwd=regsubsets(cost~.,data=train,nvmax=13,method="forward")

val.errors=rep(NA,13)
test.mat=model.matrix(cost~.,data=test)
for(i in 1:13){
    coefi=coef(fitFwd,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((test$cost-pred)^2)
}
which.min(val.errors)
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
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
names(df) <-c("crimeRate","zone","indus","charles","nox","rooms","age",
              "distances","highways","tax","teacherRatio","color","status","cost")
set.seed(1)
k = 5
folds = sample(1:k, nrow(df), replace=TRUE) 
rows=folds==2
df1<-df[rows,]
rows=folds==3
df2<-df[rows,]
fitFwd=regsubsets(cost~.,data=df[folds!=3,],nvmax=14,method="forward")
coefi=coef(fitFwd,id=2)
test=df[folds==3,]
test.mat=model.matrix(medv~.,data=test)
pred=test.mat[,names(coefi)]%*%coefi
mean((test$medv - pred)^2)



df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
names(df) <-c("no","crimeRate","zone","indus","charles","nox","rooms","age",
              "distances","highways","tax","teacherRatio","color","status","cost")
df1 <- df %>% dplyr::select("crimeRate","zone","indus","charles","nox","rooms","age",
                     "distances","highways","tax","teacherRatio","color","status","cost")

set.seed(6)
nvmax<-13
cvError <- NULL
for(i in 1:nvmax){
    k=5
    folds = sample(1:k, nrow(df1), replace=TRUE) 
    cv<-0
    for(j in 1:k){
        train <- df1[folds!=j,]
        test <- df1[folds==j,]
        fitFwd=regsubsets(cost~.,data=train,nvmax=13,method="forward")
        coefi=coef(fitFwd,id=i)
        test.mat=model.matrix(cost~.,data=test)
        pred=test.mat[,names(coefi)]%*%coefi
        rss=mean((test$cost - pred)^2)
        cv=cv+rss
        print(cv)
    }
    cvError[i]=cv/k
}
a <- seq(1,13)
d <- as.data.frame(t(rbind(a,cvError)))
names(d) <- c("Features","CVError")
ggplot(d,aes(x=Features,y=CVError),color="blue") + geom_point() + geom_line(color="blue") +
    xlab("No of features") + ylab("Cross Validation Error") +
    ggtitle("Forward Selection - Cross Valdation Error vs No of Features")
plot(seq(1,13),cvError,type="b",color="blue")

####################################################Ridge

library(glmnet)
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
names(df) <-c("no","crimeRate","zone","indus","charles","nox","rooms","age",
              "distances","highways","tax","teacherRatio","color","status","cost")
df1 <- df %>% dplyr::select("crimeRate","zone","indus","charles","nox","rooms","age",
                            "distances","highways","tax","teacherRatio","color","status","cost")

train_idx <- trainTestSplit(df1,trainPercent=75,seed=5)
train <- df1[train_idx, ]
test <- df1[-train_idx, ]

X=model.matrix(cost~.,df1)[,-1]
y=df1$cost

Xtest=model.matrix(cost~.,test)[,-1]
ytest=test$cost

fitRidge <-glmnet(X,y,alpha=0)
plot(fitRidge,xvar="lambda",label=TRUE)
cvRidge=cv.glmnet(X,y,alpha=0)
plot(cvRidge)


fitLasso <- glmnet(X,y)
plot(fitLasso,xvar="lambda",label=TRUE)
cvLasso=cv.glmnet(X,y)
plot(cvLasso)
coef(cvLasso)
