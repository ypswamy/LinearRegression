wc.at <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/TBD/case_15/wc-at.csv")
wc.at
attach(wc.at)
nrow(wc.at)
dim(wc.at)
summary(wc.at)
str(wc.at)

plot(Waist,AT)
pairs(wc.at)

# Correlation coefficient value for Waist and Addipose tissue
cor(AT,Waist)
reg<-lm(AT~Waist)
summary(reg)
confint(reg,level = 0.95)
predict(reg,inteval="predict")
# R-squared value for the above model is 0.667. 
# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(AT~log(Waist))  # Regression using logarthmic transformation
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
# R-squared value for the above model is 0.6723. 
# we may have to do different transformation better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(AT)~Waist) # regression using Exponential model
summary(reg_exp)
# R-squared value has increased from 0.67 to 0.7071 
# Higher the R-sqaured value - Better chances of getting good model 
# for Waist and addipose Tissue
