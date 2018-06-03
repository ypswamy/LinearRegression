WCData <- read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/Linear_Regression/Water_Consumption/WatCon1.csv", header = TRUE)
WCData
summary(WCData)
structure(WCData)

#The first key assumption in linear regression is the existence of a linear relationship between y and x. 
#To verify this, make sure the scatter plots looks linear.

plot(WCData$POP, WCData$WC, xlab = "Population", ylab = "Water Consumption", pch = 16, cex = 1.3, col = "blue")

WConLR <- lm(WC~POP,data=WCData)
summary(WConLR)

#The intercept is 98.93 and the slope is 0.00007145. If the population increases by 1, does the water consumption 
#increase or decrease? By how much does it increase or decrease?
#Does this mean that the impact of population is insignificant?
#create a new variable NEWPOP which measures the population in 1000s.

WCData$NEWPOP <- WCData$POP / 1000
WConLR1 <- lm(WC~NEWPOP,data=WCData)
summary(WConLR1)



#In the linear regression model, the error term is assumed to have zero mean, constant 
#variance, uncorrelated, and normally distributed. Therefore, in order to validate that 
#the error term has these propeties we will have to conduct further tests on the residuals

WCData$resid <- residuals(WConLR1)
print(mean(WCData$resid))

#If the absolute value of any of the standardized residual is high (>3), that indicates an 
#outlier and should be examined further. In this case, there are no outliers.

WCData$residstd <- rstandard(WConLR1)
print(WCData$residstd)

#Now let us check if the error term or the residuals is normally distributed. 
#We do that using qq probability plot and a histogram. The normal probability plot can be 
#created using the qqnorm() function.

qqnorm(WCData$residstd, ylab="Standardized Residuals", xlab="Normal Scores", main="Probability Plot") 
qqline(WCData$residstd)

hist(WCData$residstd, xlab="Standardized Residuals", ylab="Frequency") 

#If the standardized residuals are truly normal, they should lie exactly or very close to 
#the line in the probability plot. In this case, there appears to be a deviation from normality 
#which is a red flag. The deviation from normality is also verified by the histogram.
#The next step is to plot the studentized residuals versus the fitted values.

plot(fitted(WConLR1), WCData$residstd, ylab="Standardized Residuals", xlab="Fitted Values") 
abline(0,0)

#In this plot, make sure that the residuals are randomly distributed about the 0 line. The residuals 
#must also be contained within a horizontal band around the zero line. There should not be any discernible 
#pattern in the residuals. There is a lot of subjectivity involved in this decision. If the residuals satisfy 
#these properties (which it does in this case), then the constant variance assumption is satisfied.

#If the studentized residuals start showing patterns, that implies the variance is not constant. This could 
#imply the model is missing an important independent variable or transformation of the data might be required 
#or the true relationship between the dependent and independent variables is not linear.



