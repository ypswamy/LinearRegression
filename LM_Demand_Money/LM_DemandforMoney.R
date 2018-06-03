#Central Bank prints paper money each year. For each year they need an estimate of how much money to be printed. 
#The decision is based on various economic indicators like GDP, Interest rate etc. We will try to model the 
#solution to this problem using Multiple linear regression.

money = read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/Linear_Regression/Demand_Money/demandformoney.csv")
str(money)
structure(money)
head(money)

plot(money,col='red')

#Construct a Linear model using R
multilinearmodel = lm (Money_printed ~ GDP + Interest_RATE + WPI, data = money)
summary(multilinearmodel)

#An important point to be noted is the P value for variables GDP, Interest_RATE is significant i.e Pr(>|t|) 
#for these variables is less than 0.05 .
#For variable WPI however the P value is 0.97423 which is greater than 0.05 which is highly insignificant

multilinearmodel = lm (Money_printed ~ GDP + Interest_RATE , data = money)
summary(multilinearmodel)

#Money_printed  = -19060 + 0.3003 * GDP – 16190 * Interest_RATE