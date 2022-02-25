library(tidyverse)
library(pROC)
library(MASS)
library(ROCR)
library(DAAG)

source("D:\\Data Science Padova\\Statistic Learning\\Mod B\\R files B\\utility.r")
path = "D:\\DB for R project\\Heart Failure\\data_heart.csv"


data<-load(path, levels='yes')
dim(data)
data<-na.omit(data)
dim(data)

View(data)
glimpse(data)

###################################################################
######################## GLM without time #########################
###################################################################

data<-load(path, levels='yes')

data<-data[,-12]
attach(data)
summary(time)
summary(data$time)

l<-split_train_test(data)
train<-l[[1]]
test<-l[[2]]
dim(train)
dim(test)

formula<-DEATH_EVENT~.-anaemia- creatinine_phosphokinase- smoking-sex-serum_sodium-
  platelets-high_blood_pressure- diabetes
glm.fits<-glm(formula, family=binomial, data=train)
summary(glm.fits) 


predictions_glm<-predict(glm.fits, test, type="response")

plot_ROC(test$DEATH_EVENT, predictions_glm, levels=c("not dead", "dead"))

table_mat_standard<-table(test$DEATH_EVENT, predictions_glm>0.5)
table_mat_standard

best_treshold = 0.2265663 # 0.2502905 for time
table_mat_best <-table(test$DEATH_EVENT, predictions_glm>best_treshold)
table_mat_best


38+23 # total not dead
5+24 # total dead
38+5 # total predicted not dead
23+24 # total predicted dead

24/(24+23) # precision--> when predict patient is dead is correct 51% of the time
24/(24+5) # recall---> for people having heart failure we identify correctly 82% of them

# achieving a high recall is more important than getting a high precision? yes if
# we want to detect as many heart failure patients as possible

# maybe is better choose model with best AUC?

# FALSE means predictions<=0.5 so equal to 0, so equal to not dead 


metrics(table_mat_standard)
metrics(table_mat_best)

# RECALL # when model says that a person died is correct the 83% of the time

plot(glm.fits)

# if u have point above cook lines they are dangerous
# possible outliers are : 9,20, 229
################### WIthout Outliers ##############################

data<-load(path, levels='yes')

data<-data[,-12]
outliers <- c(9,20,229)
glimpse(data)
data.out<-data[-outliers,]
attach(data.out)
length(anaemia) # should be 296

l<-split_train_test(data.out)
train<-l[[1]]
test<-l[[2]]
dim(train)
dim(test)

formula<-DEATH_EVENT~.-anaemia- creatinine_phosphokinase- smoking-sex-serum_sodium-
  platelets-high_blood_pressure- diabetes
logit.out<-glm(DEATH_EVENT~.-anaemia-smoking-sex-diabetes-platelets-
                 serum_sodium-creatinine_phosphokinase-high_blood_pressure, family=binomial, data=data.out)
summary(logit.out) 

glm.fits.out<-glm(formula, family=binomial, data=train)
summary(glm.fits)


predictions_glm.out<-predict(glm.fits.out, test, type="response")

plot_ROC(test$DEATH_EVENT, predictions_glm.out, levels=c("not dead", "dead"))

table_mat_standard.out<-table(test$DEATH_EVENT, predictions_glm.out>0.5)
table_mat_standard.out

best_treshold = 0.5076316 
table_mat_best.out <-table(test$DEATH_EVENT, predictions_glm.out>best_treshold)
table_mat_best.out



metrics(table_mat_standard.out)
metrics(table_mat_best.out)


plot(glm.fits.out)


###################################################################
######################## GLM with time ############################
###################################################################

data<-load(path, levels='yes')
dim(data)
data<-na.omit(data)
dim(data)
glimpse(data)
detach(data)
attach(data)
summary(time)
summary(data$time)


l<-split_train_test(data)
train<-l[[1]]
test<-l[[2]]
dim(train)
dim(test)

formula.time<-DEATH_EVENT~.-anaemia- creatinine_phosphokinase- smoking-sex-serum_sodium-
  platelets-high_blood_pressure- diabetes-1
glm.fits.time<-glm(formula.time, family=binomial, data=train)
summary(glm.fits.time) 


predictions_glm.time<-predict(glm.fits.time, test, type="response")

plot_ROC(test$DEATH_EVENT, predictions_glm.time, levels=c("not dead", "dead"))

table_mat_standard.time<-table(test$DEATH_EVENT, predictions_glm.time>0.5)
table_mat_standard.time

best_treshold = 0.2502905
table_mat_best.time <-table(test$DEATH_EVENT, predictions_glm.time>best_treshold)
table_mat_best.time


metrics(table_mat_standard.time)
metrics(table_mat_best.time)





plot(glm.fits.time)

# if u have point above cook lines they are dangerous

# possible outliers are : 263, 229, 218


###################################################################
######################## GLM with month ############################
###################################################################

detach(data)
data<-load(path, levels='yes')
dim(data)
data<-na.omit(data)
dim(data)
glimpse(data)

data$month<-0
glimpse(data)

# Adding column based on other column:
data<-data %>%
  mutate(month = case_when(time<=30 ~ 1,
                           time<=60 ~ 2,
                           time<=90 ~ 3,
                           time<=120 ~ 4,
                           time<=150 ~ 5,
                           time<=180 ~ 6,
                           time<=210 ~ 7,
                           time<=240 ~ 8,
                           time<=270 ~ 9,
                           time<=300 ~ 10
  ))
View(data)


attach(data)
summary(month)

logit.out<-glm(DEATH_EVENT~.-time, data=data, family=binomial)
summary(logit.out)

logit.out<-glm(DEATH_EVENT~.-time-anaemia-smoking-high_blood_pressure-diabetes-
                 creatinine_phosphokinase-platelets-sex-serum_sodium-1  , data=data, family=binomial)
summary(logit.out)

f<-DEATH_EVENT~.-time-anaemia-smoking-high_blood_pressure-diabetes-
  creatinine_phosphokinase-platelets-sex-serum_sodium-1 



l<-split_train_test(data)
train<-l[[1]]
test<-l[[2]]
dim(train)
dim(test)

glm.fits.month<-glm(f, family=binomial, data=train)
summary(glm.fits.month) 


predictions_glm.month<-predict(glm.fits.month, test, type="response")

plot_ROC(test$DEATH_EVENT, predictions_glm.month, levels=c("not dead", "dead"))

table_mat_standard.month<-table(test$DEATH_EVENT, predictions_glm.month>0.5)
table_mat_standard.time

best_treshold = 0.2440184   
table_mat_best.month <-table(test$DEATH_EVENT, predictions_glm.month>best_treshold)
table_mat_best.month


metrics(table_mat_standard.month)
metrics(table_mat_best.month)





plot(glm.fits.time)

# if u have point above cook lines they are dangerous

########### GLM #############################