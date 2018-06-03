install.packages("devtools")
library(devtools)
install_github("kassambara/datarium")
data("marketing", package = "datarium")
attach(marketing)
dim(marketing)
nrow(marketing)
names(marketing)
summary(marketing)
str(marketing)
head(marketing)
tail(marketing)

plot(marketing)
pairs(marketing)

model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)

summary(model)$coefficient

#newspaper is not significant in the multiple regression model. #changes in the newspaper advertising budget will not significantly #affect sales units.

model  <- lm(sales ~ youtube + facebook, data = marketing)
summary(model)

confint(model)

#Residual Standard Error (RSE), or sigma:
sigma(model)/mean(marketing$sales)

model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)
