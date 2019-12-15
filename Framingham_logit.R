'*******************************logistic regression with framingham dataset*************************************************'
#install and load libraries
install.packages('dplyr')
install.packages('readr')
install.packages('ggplot2')

library(dplyr)
library(readr)
library(ggplot2)

#set wd
wd <- choose.dir()
setwd(wd)

#import dataset
framingham_raw <- read_csv("framingham.csv")
View(framingham_raw)
str(framingham_raw)

#cleaning the dataset
#check for missing values
sapply(framingham_raw, function(x) sum(is.na(x)))

#NAs in education, cigsPerDay, BPMeds, totchol, BMI, heartrate, glucose

framingham_no_na <- framingham_raw

#for continuous values replace with mean value
framingham_no_na$cigsPerDay[is.na(framingham_no_na$cigsPerDay)] <- mean(framingham_no_na$cigsPerDay, na.rm = TRUE)
framingham_no_na$totChol[is.na(framingham_no_na$totChol)] <- mean(framingham_no_na$totChol, na.rm = TRUE)
framingham_no_na$heartRate[is.na(framingham_no_na$heartRate)] <- mean(framingham_no_na$heartRate, na.rm = TRUE)
framingham_no_na$glucose[is.na(framingham_no_na$glucose)] <- mean(framingham_no_na$glucose, na.rm = TRUE)
framingham_no_na$BMI[is.na(framingham_no_na$BMI)] <- mean(framingham_no_na$BMI, na.rm = TRUE)

#delete observations with NA for categorical variables
framingham_complete <- na.omit(framingham_no_na)

#any missing values left?
sapply(framingham_complete, function(x) sum(is.na(x)))

#plausibility check
sapply(framingham_complete, function(x) min(x))
sapply(framingham_complete, function(x) max(x))

#very high values for glucose(anything above 300 is a medical emergency)
framingham_complete %>%
  ggplot(aes(glucose))+
  geom_histogram()

#limit to <200
framingham_complete <- framingham_complete %>% filter(glucose <200)

framingham_complete %>%
  ggplot(aes(glucose))+
  geom_histogram()

#research question - how does weight influence the outcome of hypertension
model <- glm (prevalentHyp ~ BMI, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#AIC 4641

#add male
model <- glm (prevalentHyp ~ BMI + male, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#no decrease of AIC, drop male

#add age
model <- glm (prevalentHyp ~ BMI + age, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#4315 -> keep age

#add education
model <- glm (prevalentHyp ~ BMI + age + education, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#4317 very slight increase, drop education

#add currentsmoker
model <- glm (prevalentHyp ~ BMI + age  + currentSmoker, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#no change drop currentsmoker

#add cigsperday
model <- glm (prevalentHyp ~ BMI + age + cigsPerDay, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#no change, drop cigsperday

#add BP meds
model <- glm (prevalentHyp ~ BMI + age + BPMeds, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#4105.6, not significant though (0.932), drop BPmeds

#add prevalentStroke
model <- glm (prevalentHyp ~ BMI + age + prevalentStroke, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#AIC doesn't improve, drop prevalentStroke

#add diabetes
model <- glm (prevalentHyp ~ BMI + age + diabetes, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#AIC doesn't improve, drop diabetes

#add totchol
model <- glm (prevalentHyp ~ BMI + age  + totChol, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#AIC 4294, keep totChol

#add sysBP
model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#AIC 2419, high decrease, keep sysBP

#add diaBP
model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP + diaBP, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#AIC 2277, slight decrease, keep diaBP

#add heartrate
model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP + diaBP + heartRate, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#2277, no decrease, drop heartRate

#add glucose
model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP + diaBP + glucose, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#2279, no decrease, drop glucose

#add TenYearCHD
model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP + diaBP + TenYearCHD, family=binomial(link='logit'), data=framingham_complete)
summary(model)
#2276, no decrease, drop TenYearCHD

#Final model: 
model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP + diaBP, family=binomial(link='logit'), data=framingham_complete)
summary(model)

#calculate Odds Ratios
logitOR <-model$coefficients
Odds <- exp(logitOR)
se <- summary(model)
Std_Er <- as.numeric(paste(se$coefficients[ , 2]))
upperCI <- exp(logitOR + (1.96 * Std_Er))
lowerCI <- exp(logitOR - (1.96 * Std_Er))

options(scipen=999)# removes scientic notation
OR <- data.frame(Odds, lowerCI, upperCI)
OR
# with age, prevstroke, sysbp, totchol and diabp as confounding variables, the odds of developing prevHyp
# are 7% higher for each additional Unit of BMI

'******************************************testing the prediction accuracy of the model******************************************************************************
# try with train and test prediction'
train <- framingham_complete[1:3000, ]
test <- framingham_complete[3001:3500, ]

model <- glm (prevalentHyp ~ BMI + age + totChol + sysBP + diaBP, family=binomial(link='logit'), data=train)
summary(model)

model2 <-model <- glm (prevalentHyp ~ BMI + age + prevalentStroke + totChol + sysBP + diaBP, family=binomial(link='logit'), data=test)
summary(model)

fitted.results <- 
  predict(model,newdata=subset(
    test),
    type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$prevalentHyp)
print(paste('Accuracy',1-misClasificError)) 
#89%accuracy
'**********************************************************************************************************************************'
  