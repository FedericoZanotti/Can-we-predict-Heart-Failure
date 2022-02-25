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
data<-data[,-12]
glimpse(data)
attach(data)
detach(data)
summary(time)
summary(data$time)


logit.out<-glm(DEATH_EVENT~., data=data, family=binomial)
summary(logit.out)

logit.out<-glm(DEATH_EVENT~.-anaemia- smoking-sex-serum_sodium-platelets-high_blood_pressure
               - diabetes, data=data, family=binomial)
summary(logit.out)

logit.out<-glm(DEATH_EVENT~.- creatinine_phosphokinase-anaemia- smoking-sex-serum_sodium-platelets-high_blood_pressure
               - diabetes, data=data, family=binomial)
summary(logit.out)


################## ANALISYS ###########################
  
continuous <-select_if(data, is.numeric)
summary(continuous)

# Histogram with kernel density curve
library(ggplot2)
ggplot(continuous, aes(x = serum_creatinine)) +
  geom_density(alpha = .2, fill = "#FF6666")

ggplot(continuous, aes(x = age)) +
  geom_density(alpha = .2, fill = "#FF6666")

ggplot(continuous, aes(x = creatinine_phosphokinase)) +
  geom_density(alpha = .2, fill = "#FF6666")

ggplot(continuous, aes(x = ejection_fraction)) +
  geom_density(alpha = .2, fill = "#FF6666")

ggplot(continuous, aes(x = platelets)) +
  geom_density(alpha = .2, fill = "#FF6666")

ggplot(continuous, aes(x = serum_sodium)) +
  geom_density(alpha = .2, fill = "#FF6666")

# Select categorical column
factor <- data.frame(select_if(data, is.factor))
ncol(factor)
factor

# # Create graph for each column
# graph <- lapply(names(factor),
#                 function(x) 
#                   ggplot(factor, aes(get(x))) +
#                   geom_bar() +
#                   theme(axis.text.x = element_text(angle = 90)))
# 
# graph


  
ggplot(data, aes(x = sex, fill = smoking)) +
  geom_bar(position = "fill") +
  theme_classic()
# 
# ggplot(data, aes(x = age, y = ejection_fraction)) +
#   geom_point(aes(color = DEATH_EVENT),
#              size = 0.5) +
#   stat_smooth(method = 'lm',
#               formula = y~poly(x, 2),
#               se = TRUE,
#               aes(color = DEATH_EVENT)) +
#   theme_classic()



##########################################
# USING A TRAINING AND A TEST SET
##########################################

# The summary of our model reveals interesting information. The performance of a logistic regression is evaluated with specific key metrics.
# 
# - AIC (Akaike Information Criteria): This is the equivalent of R2 in logistic regression. It measures the fit when a penalty is applied to the number of parameters. Smaller AIC values indicate the model is closer to the truth.
# - Null deviance: Fits the model only with the intercept. The degree of freedom is n-1. We can interpret it as a Chi-square value (fitted value different from the actual value hypothesis testing).
# - Residual Deviance: Model with all the variables. It is also interpreted as a Chi-square hypothesis testing.
# - Number of Fisher Scoring iterations: Number of iterations before converging.


source("D:\\Data Science Padova\\Statistic Learning\\Mod B\\R files B\\utility.r")

l<-split_train_test(data)
View(train)
test<-l[[2]]
dim(train)
dim(test)

formula<-DEATH_EVENT~.-anaemia- creatinine_phosphokinase- smoking-sex-serum_sodium-
  platelets-high_blood_pressure- diabetes
glm.fits<-glm(formula, family=binomial, data=train)
summary(glm.fits) 

### with time ####

formula<-DEATH_EVENT~.-anaemia- creatinine_phosphokinase- smoking-sex-serum_sodium-
  platelets-high_blood_pressure- diabetes-1
glm.fits<-glm(formula, family=binomial, data=train)
summary(glm.fits)

#######
predictions_glm<-predict(glm.fits, test, type="response")

plot_ROC(test$DEATH_EVENT, predictions_glm, levels=c("not dead", "dead"))

table_mat_standard<-table(test$DEATH_EVENT, predictions_glm>0.5)
table_mat_standard
1-(10+17)/90
best_treshold = 0.2265663 # 0.2502905 for time
table_mat_best <-table(test$DEATH_EVENT, predictions_glm>0.2502905)
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

outliers<-c(263, 21, 218, 229, 263,132)
data<-data[-outliers,]
View(prova)
logit.out<-glm(DEATH_EVENT~.-smoking-diabetes-high_blood_pressure-anaemia-platelets-
                 creatinine_phosphokinase-sex-serum_sodium-1, data=data, family=binomial)
summary(logit.out)
formula.outliers<-DEATH_EVENT~.-smoking-diabetes-high_blood_pressure-anaemia-platelets-
  creatinine_phosphokinase-sex-serum_sodium-1

## 70% of the sample size
smp_size <- floor(0.7 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

dim(train)
dim(test)


glm.fits<-glm(formula.outliers, family=binomial, data=train)
summary(glm.fits) 
plot(glm.fits)
predictions_glm<-predict(glm.fits, test, type="response")

plot_ROC(test$DEATH_EVENT, predictions_glm, levels=c("not dead", "dead"))

table_mat_standard.outliers<-table(test$DEATH_EVENT, predictions_glm>0.5)
table_mat_standard.outliers

table_mat_best.outliers <-table(test$DEATH_EVENT, predictions_glm>0.2188761)
table_mat_best.outliers

metrics(table_mat_standard.outliers)
metrics(table_mat_best.outliers)


#####################################
########  Interaction effects
#####################################

names(data)
formula_2 <- DEATH_EVENT~.+age:platelets-anaemia- creatinine_phosphokinase- smoking-sex-serum_sodium-
  platelets-high_blood_pressure- diabetes
glm.fits.interactions<-glm(formula_2, family=binomial, data=train)
summary(glm.fits.interactions)
summary(glm.fits)


######################################
# Linear Discriminant Analysis (LDA)
######################################
# install.packages("DAAG")


formula
glimpse(train)
lda.fit <- lda(formula,data=train) # with full model auc:  0.7450537, with formula auc: 0.728095
lda.fit.cv <-  lda(DEATH_EVENT~.,data=data, CV=TRUE) # with formula is better than all regressors 0.774 against 0.769
confusion(data$DEATH_EVENT, lda.fit.cv$class)
lda.LOOCV<-data.frame(lda.fit.cv$class,lda.fit.cv$posterior)
head(lda.LOOCV) 

#Prior probabilities of groups: the proportion of training observations 
# in each group. For example, there are 66% of the training observations
# not dead
# Group means: group center of gravity. Shows the mean of each variable 
# in each group.
lda.fit # http://www.cs.unitn.it/~taufer/Labs/L4-LDA.pdf
lda.pred <- predict(lda.fit, test)
# choose the posterior probability column carefully, it may be 
# lda.pred$posterior[,1] or lda.pred$posterior[,2], depending on your factor levels

par(mfrow=c(1,2))

# pred_lda <- prediction(lda.pred$posterior[,1], test$DEATH_EVENT) 
# perf_lda <- performance(pred_lda,"tpr","fpr")
# plot(perf_lda,colorize=TRUE, lwd=3)
# auc_lda<-performance(pred_lda, measure = "auc")
# auc_lda@y.values[[1]][1]
#   
# pred_lda_cv <- prediction(lda.fit.cv$posterior[,1], data$DEATH_EVENT) 
# perf_lda_cv <- performance(pred_lda_cv,"tpr","fpr")
# plot(perf_lda_cv,colorize=TRUE, lwd=3)
# auc_lda_cv<-performance(pred_lda_cv, measure = "auc")
# auc_lda_cv@y.values[[1]][1]

par(mfrow=c(1,2))


plot_ROC(test$DEATH_EVENT, lda.pred$posterior[,2], levels=c("not dead", "dead"))
plot_ROC(data$DEATH_EVENT, lda.fit.cv$posterior[,2], levels=c("not dead", "dead"))

par(mfrow=c(1,1))


# plot(lda.fit)
table_lda<-table(test$DEATH_EVENT, lda.pred$posterior[,2]>0.5)
table_lda_best_threshold<-table(test$DEATH_EVENT, lda.pred$posterior[,2]>0.2631792) # 0.2303532 NO TIME
table_lda_CV<-table(data$DEATH_EVENT, lda.fit.cv$posterior[,2]>0.5)
table_lda_CV_best_threshold<-table(data$DEATH_EVENT, lda.fit.cv$posterior[,2]>0.4316162) # 0.3014121 NO TIME
table_lda_CV_best_threshold
metrics(table_lda)
metrics(table_lda_best_threshold)
metrics(table_lda_CV)
metrics(table_lda_CV_best_threshold)





#####################################################
# truepos <- numeric(19)
# falsepos <- numeric(19)
# p1 <- (1:19)/20
# for (i in 1:19) {
#   p <- p1[i]
#   data.CV1p <- lda(formula, data = data, CV = TRUE, prior = c(p,1 - p))
#   confmat <- confusion(data$DEATH_EVENT, data.CV1p$class, printit = FALSE)
#   falsepos[i] <- confmat$confusion[1, 2]
#   truepos[i] <- confmat$confusion[2, 2]
# }
# 
# plot(truepos ~ falsepos, type = "l", xlab = "False positive rate",
#      ylab = "True positive rate (Sensitivity)")
######################################
# Quadratic Discriminant Analysis (QDA)
######################################

glimpse(train)
formula
qda.fit <- qda(formula,data=train)
qda.fit.cv <- qda(formula,data=data, CV=TRUE)
qda.LOOCV<-data.frame(qda.fit.cv$class,qda.fit.cv$posterior)

qda.fit
qda.pred <- predict(qda.fit,test)
table_qda<-table(test$DEATH_EVENT, qda.pred$class)
table_qda_CV<-table(data$DEATH_EVENT, qda.fit.cv$class)

par(mfrow=c(1,2))

qda.pred$posterior
plot_ROC(test$DEATH_EVENT, qda.pred$posterior[,2], levels=c("not dead", "dead"))
plot_ROC(data$DEATH_EVENT, qda.fit.cv$posterior[,2], levels=c("not dead", "dead"))

par(mfrow=c(1,1))

table_qda<-table(test$DEATH_EVENT, qda.pred$posterior[,2]>0.5)
table_qda_best_threshold<-table(test$DEATH_EVENT, qda.pred$posterior[,2]> 0.1180351) # 0.1734545 NO TIME
table_qda_CV<-table(data$DEATH_EVENT, qda.fit.cv$posterior[,2]>0.5)
table_qda_CV_best_threshold<-table(data$DEATH_EVENT, qda.fit.cv$posterior[,2]>0.3180658 ) # 0.2333944 NO TIME

metrics(table_qda)
metrics(table_qda_best_threshold)
metrics(table_qda_CV)
metrics(table_qda_CV_best_threshold)




###################################################

################# LDA vs QDA  ####################



?plot

  
# par(mfrow=c(3,2))
source("D:\\Data Science Padova\\Statistic Learning\\Mod B\\R files B\\utility.r")

plot_ROC(test$DEATH_EVENT, qda.pred$posterior[,2], levels=c("not dead", "dead"), noauc = FALSE)
plot_ROC(data$DEATH_EVENT, qda.fit.cv$posterior[,2], levels=c("not dead", "dead"),
         add=T, col="blue", noauc = FALSE)
plot_ROC(test$DEATH_EVENT, lda.pred$posterior[,2], levels=c("not dead", "dead"),
         add=T, col="red", noauc = FALSE)
plot_ROC(data$DEATH_EVENT, lda.fit.cv$posterior[,2], levels=c("not dead", "dead"),
         add=T, col="green", noauc = FALSE)
plot_ROC(test$DEATH_EVENT, predictions_glm, levels=c("not dead", "dead"),
         add=T, col="violet", noauc = FALSE)

legend(0.1,0.6,c('QDA','QDA CV', 'LDA', 'LDA CV', 'GLM'),col=c('black','blue','red',
                                                               'green', 'violet'),lwd=3)


######################################

#### GLM WITH CV ##########
library(boot)
formula
glm.fits.cv<-glm(formula, family=binomial, data=data)
cv.err<-cv.glm(data, glm.fits.cv)
1-cv.err$delta[1]
?cv.glm

k<-10
set.seed(17)
cv.error <-rep(0,k)
for (i in 1:k) {
  glm.fits.cv<-glm(formula, family=binomial, data=data)
  cv.error[i]<-cv.glm(data, glm.fits.cv, K=k)$delta[1]
}
max(1-cv.error)
1-cv.error
cv.error
plot(cv.error, type='b')



### BEST SUBSET SELECTION #########


library(leaps)
regfit <- regsubsets(DEATH_EVENT~., nvmax=15, data=data, method="exhaustive")
summary(regfit)

reg.summary <- summary(regfit)
plot_best_subset(reg.summary)
prova<-coef(regfit.best, id=11)
names(prova)
class(prova)
if ("diabetes1" %in% names(prova)==TRUE) {
  ind = match("diabetes1", names(prova))
  names(prova)[5]="diabetes"

}
names(prova)
prova
############ BEST ADJ R^2 ###############################
coef(regfit.best, 7)

best_R_formula = DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+
  high_blood_pressure+serum_creatinine
best_R_formula
glm_best_R <- glm(best_R_formula, data = train, family="binomial")
summary(glm_best_R)

predictions_glm_best_R<-predict(glm_best_R, test, type="response")

table_mat_standard_best_R<-table(test$DEATH_EVENT, predictions_glm_best_R>0.2981373)
table_mat_standard_best_R
1-(16+5)/90
metrics(table_mat_standard_best_R)
plot_ROC(test$DEATH_EVENT, predictions_glm_best_R, levels=c("not dead", "dead"))
plot(glm_best_R)
# look at outliers and decide if remive themn
View(data)
# if u have point above cook lines they are dangerous
#########################################################

########### BEST CP ####################################
best_CP_formula = DEATH_EVENT~.-anaemia-diabetes-platelets-sex-smoking
best_CP_formula
glm_best_CP <- glm(best_CP_formula, data = train, family="binomial")
summary(glm_best_R)

predictions_glm_best_CP<-predict(glm_best_CP, test, type="response")

table_mat_standard_best_CP<-table(test$DEATH_EVENT, predictions_glm_best_CP> 0.2678399)
table_mat_standard_best_CP
metrics(table_mat_standard_best_CP)
plot_ROC(test$DEATH_EVENT, predictions_glm_best_CP, levels=c("not dead", "dead"))


#############################################################

########### BEST BIC ####################################
best_BIC_formula = DEATH_EVENT~.-creatinine_phosphokinase-high_blood_pressure-serum_sodium-anaemia-diabetes-platelets-sex-smoking
best_BIC_formula
glm_best_BIC <- glm(best_BIC_formula, data = train, family="binomial")
summary(glm_best_BIC)

predictions_glm_best_BIC<-predict(glm_best_BIC, test, type="response")

table_mat_standard_best_BIC<-table(test$DEATH_EVENT, predictions_glm_best_BIC> 0.5)
table_mat_standard_best_BIC
metrics(table_mat_standard_best_BIC)
plot_ROC(test$DEATH_EVENT, predictions_glm_best_BIC, levels=c("not dead", "dead"))

# BEST BIC IS EQUAL TO GLM WITH BEST THRESHOLD

#############################################################


#### cv and k fold for reg subset ##########################
# if u want to do it look first this: https://www.youtube.com/watch?v=BQ1VAZ7jNYQ
regfit.best <- regsubsets(DEATH_EVENT~., data=train,nvmax=19)
summary(regfit.best)
test.mat <- model.matrix(DEATH_EVENT~., data=test)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

length(test$DEATH_EVENT)
test_response <- as.numeric(test$DEATH_EVENT)
test_response[test_response==1]=0
test_response
test_response[test_response==2]=1
test_response
length((test_response))
test.mat <- model.matrix(DEATH_EVENT~., data=test)
dim(test.mat)
cost(predictions_glm-test_response)
dim(train)
prova<-coef(regfit.best, id=11)

change_names <- function(p) {
  if ("diabetes1" %in% names(p)==TRUE) {
    ind = match("diabetes1", names(p))
    names(p)[ind] = "diabetes"
  }
  if ("anaemia1" %in% names(p)==TRUE) {
    ind = match("anaemia1", names(p))
    names(p)[ind] = "anaemia"
  }
  if ("high_blood_pressure1" %in% names(p)==TRUE) {
    ind = match("high_blood_pressure1", names(p))
    names(p)[ind] = "high_blood_pressure"
  }
  if ("sexmale" %in% names(p)==TRUE) {
    ind = match("sexmale", names(p))
    names(p)[ind] = "sex"
  }
  if ("smoking1" %in% names(p)==TRUE) {
    ind = match("smoking1", names(p))
    names(p)[ind] = "smoking"
  }
  p
}
glm.try <- glm(DEATH_EVENT~ejection_fraction+serum_creatinine+time-1, data=train, family=binomial)
summary(glm.try)
coefi <- coef(regfit.best, id=3)
coefi
length(coefi)

glm.try$coefficients
glm.try$coefficients<-coefi
glm.try$coefficients
summary(glm.try)

pred<-predict(glm.try, test, type="response")
pred
cost <- function(r, pi) mean(abs(r-pi) >0.5)
1-cost(test_response, pred)
table(test$DEATH_EVENT, predictions_glm_best_R>0.2981373)
1-(8+18)/90
1-(16+5)/90
val.errors <- rep(NA,11)
for(i in 1:11){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  print(pred)
  val.errors[i] <- cost(test_response,pred)
}
val.errors

plot(val.errors,type='b')

set.seed(1)
k=5
folds <- sample(1:k, nrow(data), replace=TRUE)
cv.errors <- matrix(NA,k,11)
colnames(cv.errors) <- 1:11
folds
cv.errors
test_temp<-as.numeric(data$DEATH_EVENT[folds==1])
test_temp
test_temp[test_temp==1]=0
test_temp[test_temp==2]=1
test_temp
for(j in 1:k){
  best.fit <- regsubsets(DEATH_EVENT~., data=data[folds!=j,], nvmax=12)
  test.mat <- model.matrix(DEATH_EVENT~., data=data[folds==j,])
  for(i in 1:11){
    coefi <- coef(best.fit, id=i)
    if ((j==k) && (i==11)) {
      print(names(coefi))
      coef_res<- coefi
    }
    pred <- test.mat[,names(coefi)]%*%coefi
    test_temp<-as.numeric(data$DEATH_EVENT[folds==j])
    test_temp[test_temp==1]=0
    test_temp[test_temp==2]=1
    roc.out <- roc(test_temp, as.numeric(pred))
    print(roc.out$auc)
    if ((j==k) && (i==11)) {
      print(pred)
      print(test_temp)
    }
    cv.errors[j,i] <- cost(test_temp,pred)
  }
}
coef_res
glm.res <- glm(DEATH_EVENT~., data = train, family=binomial)

summary(glm.res)
coef_res
glm.res$coefficients
glm.res$coefficients<-coef_res
glm.res$coefficients
summary(glm.res)

pred.res<- predict(glm.res, test, type="response")
pred.res
table_mat_standard<-table(test$DEATH_EVENT, pred.res>0.5)
table_mat_standard


?cv.glm
cv.errors







#######################################
# forward stepwise selection
#######################################

full<- glm(DEATH_EVENT~.,  data=train, family=binomial) 
empty<- glm(DEATH_EVENT~1, data=train, family=binomial)
summary(full)
summary(empty)
# try steps=20 and steps=10
step.mod.f <- step(empty, steps=10, trace=0, scope=list(lower=formula(empty),
                                                      upper=formula(full)), direction="forward")

step.mod.b <- step(full, steps=10, trace=0, scope=list(lower=formula(empty),
                                                          upper=formula(full)), direction="backward")

summary(step.mod.f)
summary(step.mod.b)
class(step.mod.b)

predictions.f<-predict(step.mod.f, test, type="response")
predictions.b<-predict(step.mod.b, test, type="response")


table_mat.f<-table(test$DEATH_EVENT, predictions.f>0.2265663)
table_mat.f

table_mat.b<-table(test$DEATH_EVENT, predictions.b>0.2265663)
table_mat.b

metrics(table_mat.f)
metrics(table_mat.b)
metrics(table_mat_best)



source("utility.r")
par(mfrow=c(1,2))

plot_ROC(test$DEATH_EVENT, predictions.f, c("not dead", "dead"))
plot_ROC(test$DEATH_EVENT, predictions.b, c("not dead", "dead"))

par(mfrow=c(1,1))

#########################

 ################### try to see for aic, cp, bic anfd R^2....

formula_to_try<- DEATH_EVENT ~ . - anaemia - creatinine_phosphokinase - smoking - 
  sex - serum_sodium - platelets - high_blood_pressure - diabetes-1
formula_to_try
formula
X<-model.matrix(formula, train)
X
#X<-X[,-1]
dim(X)
# design matrix 
#
X <- model.matrix(train$DEATH_EVENT~.,train)
X
# remove the first column relative to the intercept 
#
X <- X[,-1]
X
# vector of responses

y <- train$DEATH_EVENT
y[100:115]
# grid of lambda values

grid <- 10^seq(10, -2, length=100)
grid
library(glmnet)

ridge.mod <- glmnet(X, y, alpha=0, lambda=grid, family=binomial)
dim(coef(ridge.mod))
# 13 rows and 100 cols. first col are the parameters with lamnda =0.01 and so on


set.seed(1)

cv.out <- cv.glmnet(X, y, alpha=0, nfolds=10, lambda=grid, family=binomial)
cv.out
plot(cv.out)
bestlam <- cv.out$lambda.min 
bestlam


ridge.mod <- glmnet(X, y, alpha=0, lambda=bestlam, family=binomial)
coef(ridge.mod)
newx<-model.matrix(test$DEATH_EVENT~.- anaemia - creatinine_phosphokinase - smoking - 
                     sex - serum_sodium - platelets - high_blood_pressure - diabetes,test)
newx<-newx[,-1]
predictions<- predict(ridge.mod, s=bestlam, newx, type="response")

table_mat.ridge<-table(test$DEATH_EVENT, predictions>.5)
table_mat.ridge
table_mat.ridge.best<-table(test$DEATH_EVENT, predictions> 0.2786639 )
table_mat.ridge.best

metrics(table_mat)
metrics(table_mat_best)


prova <- as.numeric(predictions)
prova
plot_ROC(test$DEATH_EVENT, predictions, c("not dead", "dead"))






source("utility.r")



library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(data, as.integer))
# Plot the graph
help(ggcorr)
ggcorr(corr,
method = c("pairwise", "spearman"),
nbreaks = 6,
hjust = 0.8,
label = TRUE,
label_size = 3,
color = "grey50")

