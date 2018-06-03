## first we read in the data
FuelEff <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/LM_FuelEff/FuelEfficiency.csv")
FuelEff
names(FuelEff)
head(FuelEff)
dim(FuelEff)
nrow(FuelEff)
summary(FuelEff)
str(FuelEff)

sapply(FuelEff, function(x) length(unique(x))))
pairs(FuelEff)

plot(GPM~MPG,data=FuelEff)
plot(GPM~WT,data=FuelEff)
plot(GPM~DIS,data=FuelEff)
plot(GPM~NC,data=FuelEff)
plot(GPM~HP,data=FuelEff)
plot(GPM~ACC,data=FuelEff)
plot(GPM~ET,data=FuelEff)
names(FuelEff)
FuelEff=FuelEff[-1]
names(FuelEff)
FuelEff


## regression on all data
m1=lm(GPM~.,data=FuelEff)
summary(m1)
cor(FuelEff)

## best subset regression in R
library(leaps)
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab
m2=lm(GPM~WT,data=FuelEff)
summary(m2)

## cross-validation (leave one out) for the model on all six regressors
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) 
{
train1=c(1:n)
train=train1[train1!=k]

## the R expression "train1[train1!=k]" picks from train1 those
## elements that are different from k and stores those elements in the
## object train.
## For k=1, train consists of elements that are different from 1; that
## is 2, 3, …, n.

m1=lm(GPM~.,data=FuelEff[train,])
pred=predict(m1,newdat=FuelEff[-train,])
obs=FuelEff$GPM[-train]
diff[k]=obs-pred
percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))

me # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out) for the model on weight only
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
train1=c(1:n)
train=train1[train1!=k]
m2=lm(GPM~WT,data=FuelEff[train,])
pred=predict(m2,newdat=FuelEff[-train,])
obs=FuelEff$GPM[-train]
diff[k]=obs-pred
percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me # mean error
rmse # root mean square error
mape # mean absolute percent error
