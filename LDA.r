library(tidyverse)
library(pROC)
library(MASS)
library(ROCR)
library(DAAG)

source("D:\\Data Science Padova\\Statistic Learning\\Mod B\\R files B\\utility.r")
path = "D:\\DB for R project\\Heart Failure\\data_heart.csv"


###################################################################
######################## LDA without time #########################
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

lda.fit <- lda(formula,data=train) # with full model auc:  0.7450537, with formula auc: 0.728095
lda.fit.cv <-  lda(DEATH_EVENT~.,data=data, CV=TRUE) # with formula is better than all regressors 0.774 against 0.769


# Prior probabilities of groups: the proportion of training observations 
# in each group. For example, there are 66% of the training observations
# not dead
# Group means: group center of gravity. Shows the mean of each variable 
# in each group.

lda.fit # http://www.cs.unitn.it/~taufer/Labs/L4-LDA.pdf
lda.pred <- predict(lda.fit, test)

# choose the posterior probability column carefully, it may be 
# lda.pred$posterior[,1] or lda.pred$posterior[,2], depending on your factor levels

par(mfrow=c(1,2))


plot_ROC(test$DEATH_EVENT, lda.pred$posterior[,2], levels=c("not dead", "dead"))
plot_ROC(data$DEATH_EVENT, lda.fit.cv$posterior[,2], levels=c("not dead", "dead"))

par(mfrow=c(1,1))


# plot(lda.fit)
table_lda<-table(test$DEATH_EVENT, lda.pred$posterior[,2]>0.5)
table_lda_best_threshold<-table(test$DEATH_EVENT, lda.pred$posterior[,2]>0.2303532 ) 
table_lda_CV<-table(data$DEATH_EVENT, lda.fit.cv$posterior[,2]>0.5)
table_lda_CV_best_threshold<-table(data$DEATH_EVENT, lda.fit.cv$posterior[,2]>0.301412) 
table_lda_CV_best_threshold
metrics(table_lda)
metrics(table_lda_best_threshold)
metrics(table_lda_CV)
metrics(table_lda_CV_best_threshold)



###################################################################
######################## LDA with time #########################
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

lda.fit.time <- lda(formula,data=train) # with full model auc:  0.7450537, with formula auc: 0.728095
lda.fit.cv.time <-  lda(DEATH_EVENT~.,data=data, CV=TRUE) # with formula is better than all regressors 0.774 against 0.769


lda.fit.time 
lda.pred.time <- predict(lda.fit.time, test)



par(mfrow=c(1,2))


plot_ROC(test$DEATH_EVENT, lda.pred.time$posterior[,2], levels=c("not dead", "dead"))
plot_ROC(data$DEATH_EVENT, lda.fit.cv.time$posterior[,2], levels=c("not dead", "dead"))

par(mfrow=c(1,1))


# plot(lda.fit)
table_lda.time<-table(test$DEATH_EVENT, lda.pred.time$posterior[,2]>0.5)
table_lda_best_threshold.time<-table(test$DEATH_EVENT, lda.pred.time$posterior[,2]>0.2631792 ) 
table_lda_CV.time<-table(data$DEATH_EVENT, lda.fit.cv.time$posterior[,2]>0.5)
table_lda_CV_best_threshold.time<-table(data$DEATH_EVENT, lda.fit.cv.time$posterior[,2]>0.4316162) 
table_lda_CV_best_threshold.time
metrics(table_lda.time)
metrics(table_lda_best_threshold.time)
metrics(table_lda_CV.time)
metrics(table_lda_CV_best_threshold.time)


