fram = read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/Linear_Regression/framingham/fram1.csv")
str(fram)
structure(fram)
head(fram)


#Check for NULLS
sapply(fram,function(x){sum(is.na(x))})

#replace the null values with mean (since both of the variables are #numerical).

fram$TOTCHOL[is.na(fram$TOTCHOL)]=mean(fram$TOTCHOL,na.rm=T)
fram$BMI[is.na(fram$BMI)]=mean(fram$BMI,na.rm=T)
fram$BPMEDS[is.na(fram$BPMEDS)]=mean(fram$BPMEDS,na.rm=T)
fram$HEARTRTE[is.na(fram$HEARTRTE)]=mean(fram$HEARTRTE,na.rm=T)
fram$CIGPDAY[is.na(fram$CIGPDAY)]=mean(fram$CIGPDAY,na.rm=T)
fram$GLUCOSE[is.na(fram$GLUCOSE)]=mean(fram$GLUCOSE,na.rm=T)

#Check for NULLS
sapply(fram,function(x){sum(is.na(x))})

lm2<-lm(SYSBP~BMI+AGE+SEX+BPMEDS,data=fram)
summary(lm2)
plot(lm2)
confint(lm2)



