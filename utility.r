path = "D:\\DB for R project\\Heart Failure\\data_heart.csv"

load<- function(path, levels='yes') {
  my_data <- read.csv(path, header=TRUE)
  my_data$anaemia<- as.factor(my_data$anaemia)
  my_data$diabetes<- as.factor(my_data$diabetes)
  my_data$high_blood_pressure<- as.factor(my_data$high_blood_pressure)
  my_data$smoking<-as.factor(my_data$smoking)
  if (levels=='yes') {
    my_data$sex<- as.factor(my_data$sex)
    levels(my_data$sex) = c("female","male")
    my_data$DEATH_EVENT<- as.factor(my_data$DEATH_EVENT)
    levels(my_data$DEATH_EVENT) = c("not dead","dead")
  }
  my_data
}


plot_ROC <- function(variable, predictions, levels, add=FALSE, col='black', noauc=TRUE) {
  roc.out <- roc(variable, predictions, levels=levels)
  plot(roc.out,  print.auc=noauc, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate",
         add=add, col=col)
 
  print("BEST THRESHOLD:")
  coords(roc.out, "best")
}

plot_best_subset <- function(summary) {
  d<-data.frame(
    Adj.R2 = which.max(summary$adjr2),
    CP = which.min(summary$cp),
    BIC = which.min(summary$bic)  )
  print(d)
  par(mfrow=c(2,2))
  
  # panel 1
  plot(summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
  rss<-which.max(summary$rss)
  points(rss,summary$rss[rss], col="red",cex=2,pch=20)
  
  # panel 2
  plot(summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
  adjr2<-which.max(summary$adjr2)
  points(adjr2,summary$adjr2[adjr2], col="red",cex=2,pch=20)
  
  # panel 3
  plot(summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
  cp<-which.min(summary$cp)
  points(cp,summary$cp[cp],col="red",cex=2,pch=20)
  
  # panel 4
  plot(summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
  bic<-which.min(summary$bic)
  points(bic,summary$bic[bic],col="red",cex=2,pch=20)
  par(mfrow=c(1,1))
  
}


precision <- function(matrix) {  # first varibale, second predictions in table
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

metrics <- function(matrix) {
  acc= sum(diag(matrix)) / sum(matrix)
  p = precision(matrix)
  r = recall(matrix)
  f1 =  2 * ((p * r) / (p + r))
  d<-data.frame("Accuracy"= acc, "Precision"=p, "Recall"=r, "F1 score"=f1)
  d
}

split_train_test <- function(data) {
  ## 70% of the sample size
  smp_size <- floor(0.7 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  return(list(train, test))
}

