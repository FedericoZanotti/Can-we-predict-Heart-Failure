library(tidyverse)
library(pROC)
library(MASS)
library(ROCR)
library(DAAG)

source("D:\\Data Science Padova\\Statistic Learning\\Mod B\\R files B\\utility.r")
path = "D:\\DB for R project\\Heart Failure\\data_heart.csv"


###################################################################
######################## QDA without time #########################
###################################################################
detach(data)
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

formula<-DEATH_EVENT ~ . - anaemia - creatinine_phosphokinase - smoking - 
  sex - serum_sodium - platelets - high_blood_pressure - diabetes
formula

qda.fit <- qda(formula,data=train)
qda.fit.cv <- qda(formula,data=data, CV=TRUE)


qda.fit
qda.pred <- predict(qda.fit,test)
table_qda<-table(test$DEATH_EVENT, qda.pred$class)
table_qda_CV<-table(data$DEATH_EVENT, qda.fit.cv$class)

par(mfrow=c(1,2))


plot_ROC(test$DEATH_EVENT, qda.pred$posterior[,2], levels=c("not dead", "dead"))
plot_ROC(data$DEATH_EVENT, qda.fit.cv$posterior[,2], levels=c("not dead", "dead"))

par(mfrow=c(1,1))

table_qda<-table(test$DEATH_EVENT, qda.pred$posterior[,2]>0.5)
table_qda_best_threshold<-table(test$DEATH_EVENT, qda.pred$posterior[,2]> 0.1734545)
table_qda_CV<-table(data$DEATH_EVENT, qda.fit.cv$posterior[,2]>0.5)
table_qda_CV_best_threshold<-table(data$DEATH_EVENT, qda.fit.cv$posterior[,2]>0.2333944 )

metrics(table_qda)
metrics(table_qda_best_threshold)
metrics(table_qda_CV)
metrics(table_qda_CV_best_threshold)

###################################################################
######################## QDA with time ###########################
###################################################################

detach(data)
data<-load(path, levels='yes')

attach(data)
summary(time)
summary(data$time)

l<-split_train_test(data)
train<-l[[1]]
test<-l[[2]]
dim(train)
dim(test)

formula<-DEATH_EVENT ~ . - anaemia - creatinine_phosphokinase - smoking - 
  sex - serum_sodium - platelets - high_blood_pressure - diabetes
formula

qda.fit.time <- qda(formula,data=train)
qda.fit.cv.time <- qda(formula,data=data, CV=TRUE)


qda.fit.time
qda.pred.time <- predict(qda.fit.time,test)
table_qda.time<-table(test$DEATH_EVENT, qda.pred.time$class)
table_qda_CV.time<-table(data$DEATH_EVENT, qda.fit.cv.time$class)

par(mfrow=c(1,2))


plot_ROC(test$DEATH_EVENT, qda.pred.time$posterior[,2], levels=c("not dead", "dead"))
plot_ROC(data$DEATH_EVENT, qda.fit.cv.time$posterior[,2], levels=c("not dead", "dead"))

par(mfrow=c(1,1))

table_qda.time<-table(test$DEATH_EVENT, qda.pred.time$posterior[,2]>0.5)
table_qda_best_threshold.time<-table(test$DEATH_EVENT, qda.pred.time$posterior[,2]> 0.1180351)
table_qda_CV.time<-table(data$DEATH_EVENT, qda.fit.cv.time$posterior[,2]>0.5)
table_qda_CV_best_threshold.time<-table(data$DEATH_EVENT, qda.fit.cv.time$posterior[,2]>0.3180658  )

metrics(table_qda.time)
metrics(table_qda_best_threshold.time)
metrics(table_qda_CV.time)
metrics(table_qda_CV_best_threshold.time)


