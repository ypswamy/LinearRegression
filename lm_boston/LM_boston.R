https://www.kaggle.com/sukeshpabba/linear-regression-with-boston-housing-data

install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.package('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')

library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)

data(BostonHousing)
housing <- BostonHousing
str(housing)

head(housing)

summary(housing)

names(housing)

#Check for any NA’s in the dataframe.
missmap(housing,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

#Correlation plots are a great way of exploring data and seeing if there are any #interaction terms.

corrplot(cor(select(housing,-chas)))

#Train and Test Data

#set a seed 
set.seed(123)

#Split the data , `split()` assigns a booleans to a new column based on the SplitRatio #specified. 

split <- sample.split(housing,SplitRatio =0.75)


train <- subset(housing,split==TRUE)
test <- subset(housing,split==FALSE)

model <- lm(medv ~ crim + rm + tax + lstat , data = train)
summary(model)

res <- residuals(model)

# Convert residuals to a DataFrame 

res <- as.data.frame(res)

#Assessing our Model
error <- test$medv-test$predicted.medv
rmse <- sqrt(mean(error)^2)
