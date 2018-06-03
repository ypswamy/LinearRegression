install.packages("car")
install.packages("ggplot2")
install.packages("MASS")
library(car)
library(ggplot2)
library(MASS)

#https://rpubs.com/FelipeRego/SimpleLinearRegression
head(Prestige,5)
dim(Prestige)
nrow(Prestige)
names(Prestige)
summary(Prestige)
str(Prestige)
summary(Prestige)
plot(Prestige)
pairs(Prestige)
# Subset the data to capture only income and education.
newdata = Prestige[,c(1:2)]
newdata
summary(newdata)
names(newdata)
dim(newdata)
nrow(newdata)
str(newdata)
plot(newdata)

# fit a linear model and run a summary of its results.
set.seed(1)
education.c = scale(newdata$education, center=TRUE, scale=FALSE)
mod = lm(income ~ education.c, data = newdata)
summary(mod)

# visualize residuals and fitted values.
plot(mod, pch=16, which=1)

# Run the box-cox transform on the model results and pin point the #optimal lambda value.
trans = boxcox(mod)

plot(mod2, pch=16, which=1)

#From the output above, we can see that the box-cox transformation #had an almost unnoticeable improvement in the model results. We had #minimal improvement in the R-squared values. The graphs show how the #box-cox transformation on the income variable ‘reshapes’ the data #and gives it a more nomally distributed look. Note also how the #second model’s residual plot still indicates the presence of points #(some new ones) far away from the horizontal line.

#These findings help sediment the belief that a non-linear model is #more appropriate for this dataset.


