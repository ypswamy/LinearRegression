# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/MLR_Startup/50_Startups.csv')
dataset
dim(dataset)
nrow(dataset)
names(dataset)
head(dataset)
summary(dataset)
str(dataset)
pairs(dataset)

ds <- as.data.frame(dataset)
ds$State <- as.numeric(ds$State)
cor(ds)
corrplot(cor(ds))


# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
					   
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

dim(training_set)
dim(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set,stringsAsFactors=F)
			   
summary(regressor)
plot(regressor)
res <- residuals(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)







					   



