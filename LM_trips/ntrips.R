http://NPTS <- read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/Linear_Regression/trips/ntps.csv", header=TRUE)
names(NPTS)
summary(NPTS)
structure(NPTS)

#check the relationship between the different pairs of variables visually using scatterplot.

NPTS1 <- NPTS[c("ntrip","income","urban","wrkrcnt","numadult","auttrcnt","hhsize")]
plot(NPTS1,pch = 16, cex = 1.3, col = "blue" )

#we can see evidence of positive correlation between wrkrctn, numadult, and hhsize. Let us run the linear regression model.
NPTSLR <- lm(ntrip ~ income + urban + wrkrcnt + numadult + auttrcnt + hhsize, data = NPTS)
summary(NPTSLR)

#repeat the process by adding hh_0to4 and hh_5to21

NPTSLR1 <- lm(ntrip ~ income + urban + wrkrcnt + numadult + auttrcnt + hhsize + hh_0to4 + hh_5to21, data = NPTS)
summary(NPTSLR1)

#we need to drop one variable and rerun the regression. dropped hh_0to4 and rerun the model

NPTSLR1 <- lm(ntrip ~ income + urban + wrkrcnt + numadult + auttrcnt + hhsize + hh_5to21, data = NPTS)
summary(NPTSLR1)

#Multi-collinearity can be identified by generating the correlation matrix of the predictors.

library(corrplot)
NPTS2 <- NPTS[c("ntrip","income","urban","wrkrcnt","numadult","auttrcnt","hhsize", "hh_5to21")]
npts2cor <- cor(NPTS2)
corrplot(npts2cor, method = "number")

#There are strong correlations between hhsize and hh_5to21 and hhsize and numadult. 
#One possibility is to consider dropping hhsize. In such cases, having all predictors may lead to incorrect estimates.

library(car)
vif(NPTSLR1)

#Normally when you have VIFs > 4, then we have strong multi-collinearity.
#Normally when you have VIFs > 4, then we have strong multi-collinearity. In our case, we have the VIF for hhsize to be 7.59. 
#The high VIF indicates that hhsize can be explained as a combination of other variables. When you have several variables 
#with VIFs > 4, then remove the variable with the highest VIF. Rerun the model. Calculate the VIF. Repeat the process until 
#all VIFs are <= 4.
#In this model, the worst offender is hhsize. So I will remove hhsize and recalculate the VIFs. NOw all VIFs are less than 4 
#and we have eliminated the multi-collinearity problem.

NPTSLR1 <- lm(ntrip ~ income + urban + wrkrcnt + numadult + auttrcnt + hh_5to21, data = NPTS)
vif(NPTSLR1)


#If we have several variables which are not significant, then you can start removing these variables gradually from the model. 
#Start with the variables with the lowest t statistics and remove them one by one or in groups. When you are removing in groups, 
#you may want to do an F-test and check occasionally.

#The histogram of the standardized residuals appear normal. However, the normal probability plot shows a clear departure 
#from normality at the ends.

NPTS$residstd <- rstandard(NPTSLR1)
qqnorm(NPTS$residstd, ylab="Standardized Residuals", xlab="Normal Scores", main="Probability Plot") 
qqline(NPTS$residstd)

hist(NPTS$residstd, xlab="Standardized Residuals", ylab="Frequency") 
plot(fitted(NPTSLR1), NPTS$residstd, ylab="Standardized Residuals", xlab="Fitted Values") 
abline(0,0)

#Let us construct confidence and prediction intervals on the number of trips. First, we create a new data frame 
#with the relevant information.

NPTSnew <- data.frame("income" = 30000, "urban" = 1, "wrkrcnt" = 1, "numadult" = 2, "auttrcnt" = 1, "hh_5to21" = 1)
#The 95% confidence interval can be calculated as follows.
predict(NPTSLR1, NPTSnew , interval = "confidence")
#The 99% confidence interval can be calculated as follows.
predict(NPTSLR1, NPTSnew , interval = "confidence", level = 0.99) 
#As expected, the 99% confidence interval is wider. The 95% prediction interval can also be calculated as follows. 
#As expected the prediction interval is wider than the confidence interval.predict(NPTSLR1, NPTSnew , interval = "prediction")

#The confint() command provides the 95% confidence interval on the parameter estimates. 
confint(NPTSLR1)



