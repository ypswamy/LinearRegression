data(airquality)# to call the data
attach(airquality)
head(airquality,10)# to see first 10 rows
summary(airquality)

Month5=subset(airquality,Month=5,select = Temp)
Month6=subset(airquality,Month=6,select = Temp)
Month7=subset(airquality,Month=7,select = Temp)
Month8=subset(airquality,Month=8,select = Temp)
Month9=subset(airquality,Month=9,select = Temp)

#Use a boxplot to visualize the daily temperature for month 5, 6, 7, 8 and 9

# 3 rows and 2 columns
par(mfrow = c(1,2))  
boxplot((Month5$Temp~airquality$Day),Main = "Month 5", col = rainbow(3))
boxplot((Month6$Temp~airquality$Day),Main = "Month 6", col = rainbow(3))
boxplot((Month7$Temp~airquality$Day),Main = "Month 7", col = rainbow(3))
boxplot((Month8$Temp~airquality$Day),Main = "Month 8", col = rainbow(3))
boxplot((Month9$Temp~airquality$Day),Main = "Month 9", col = rainbow(3))

#histogram to see the distribution of temperature data.

hist(airquality$Temp,col=rainbow(2))

#use a scatter plot to see if there is a linear pattern between the #‘temperature rise’ and other variables.

plot(airquality$Temp~airquality$Day+airquality$Solar.R+airquality$Wind+airquality$Ozone,col=”blue”)

#It seems that solar.R , Ozone, and wind have a linear pattern with #temperature. Solar and Ozone have a positive relationship and wind #has a negative one.  Use Co-plot to see the effect of wind and #solar radiations combined on Temperature

coplot(Ozone~Solar.R|Wind,panel=panel.smooth,airquality,col =”green” )

#Check for NULLS
sapply(airquality,function(x){sum(is.na(x))})

#replace the null values with mean (since both of the variables are #numerical).

airquality$Ozone[is.na(airquality$Ozone)]=mean(airquality$Ozone,na.rm=T)

airquality$Solar.R[is.na(airquality$Solar.R)]=mean(airquality$Solar.R,na.rm=T)

sapply(airquality,function(x){sum(is.na(x))})

#use corrplot library to visualize the correlation between #variables.

library(corrplot)
# this method can be changed try using method=’circle’
o=corrplot(cor(airquality),method="number") 
#see the effect of multicollinearity (without dropping a parameter) #on our model.

Model_lm1=lm(Temp~.,data=airquality)
summary(Model_lm1)

#Using a ‘Step’ function in R. The step function runs all the #possible parameters and checks the lowest value.

Model_lm_best=step(Model_lm1)

plot(Model_lm_best,col=”blue”)

library(fmsb)
Model_lm1=lm(Temp~ Ozone+Solar.R+Month,data=airquality)
VIF(lm(Month ~ Ozone+Solar.R,data=airquality))
VIF(lm(Ozone ~ Solar.R+Month, data=airquality))
VIF(lm(Solar.R ~ Ozone +Month, data=airquality))

#As a general rule, VIF < 5 is acceptable (VIF = 1 means there is no #multicollinearity), and VIF > 5 is not acceptable and we need to #check our model.

#In ‘Normal Q-Q’ plot it can be seen that residuals are normally #distributed. It can be seen by plotting histogram of residuals also

hist(Model_lm_best$residuals)
