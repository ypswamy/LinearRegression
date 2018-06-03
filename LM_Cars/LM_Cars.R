head(cars)
#Relationship between speed and distance
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed") 
# divide graph area in 2 columns
par(mfrow=c(1, 2)) 
# box plot for 'speed'
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out)) 
# box plot for 'distance'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))   
#Check if the response variable is close to normality
library(e1071)
# divide graph area in 2 columns
par(mfrow=c(1, 2))
# density plot for 'speed'  
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2))) 

polygon(density(cars$speed), col="red")

# density plot for 'dist'
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  
polygon(density(cars$dist), col="red") 

# calculate correlation between speed and distance
cor(cars$speed, cars$dist)

# build linear regression model on full data
linearMod <- lm(dist ~ speed, data=cars) 

# model summary
summary(linearMod) 

# AIC 
AIC(linearMod)  
# BIC 
BIC(linearMod)

#Procedure #2 with training and test data
# setting seed to reproduce results of random sampling
set.seed(100)
#Step 1: Create the training (development) and test (validation) data #samples from original data.
# row indices for training data
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))
# model training data
trainingData <- cars[trainingRowIndex, ]
# test data
testData  <- cars[-trainingRowIndex, ]
#Step 2: Develop the model on the training data and use it to predict #the distance on test data      
# Build the model on training data 
# build the model
lmMod <- lm(dist ~ speed, data=trainingData)  
# predict distance
distPred <- predict(lmMod, testData)  
#Step 3: Review diagnostic measures.
# model summary
summary (lmMod)  
#Step 4: Calculate prediction accuracy and error rates
# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
