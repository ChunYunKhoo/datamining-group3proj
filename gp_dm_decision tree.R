install.packages("corrplot")
install.packages("class")
install.packages("caret")
install.packages("dplyr")
install.packages("Metrics")
install.packages("e1071")
install.packages("c50")
install.packages("gmodels")
install.packages("tidyverse")
install.packages('randomForest')
install.packages('caret')
install.packages('GGally')
install.packages("ggforce")
install.packages("ggplot2")
install.packages('Metrics')
install.packages('Boruta')
install.packages('MLmetrics')
install.packages('tictoc')
install.packages('mlbench')
install.packages("ROSE")
library(MLmetrics)
library(corrplot)
library(class)
library(caret)
library(dplyr)
library(Metrics)
library(e1071)
library(C50)
library(gmodels)
library(randomForest)
library(ggforce)
library(tidyverse)
library(randomForest)
library(ggplot2)
library(GGally)
library(Metrics)
library(mlbench)
library(Metrics)
library(ROSE)
#set working directory 
setwd("~/Desktop/data_mining")
data <- read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
str(data)
#change all integer variables to numeric variables
features <- colnames(data[,sapply(data,is.integer)])
str(features)

for(x in features){
  data[[x]] <- as.numeric(data[[x]])
}

str(data)

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

result <- list()
#5-fold cross-validation
set.seed(123)
cv <- createFolds(data$y, k=5)
proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  set.seed(123)
  train_data <- data[-cv[[i]],]
  test_data <- data[cv[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
    train_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
  }
  #Remove in train_data: default, poutcome
  #############decision tree###########
  set.seed(123)
  dt <- C5.0(train_data[,!(names(train_data) %in% c("default","poutcome","y"))],train_data$y)
  dt_pred <- predict(dt, newdata = test_data)
  cm <- confusionMatrix(dt_pred, reference = test_data$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt_pred_prob <- predict(dt, newdata = test_data,type = "prob")
  actual_labels_numeric <- ifelse(test_data$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,dt_pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  result[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
result
avg_mcc <- mean(sapply(result, function(res) res$MCC))
avg_mcc
#0.4618688




#2nd model: baseline with discretized previous and pdays

#read "bank-full.csv" file
data1 <- read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
#change all integer variables to numeric variables
features <- colnames(data1[,sapply(data1,is.integer)])
str(features)

for(x in features){
  data1[[x]] <- as.numeric(data1[[x]])
}

str(data1)
#discretizing previous into 5 categories in new variable pdays_disc
categories_pdays <- cut(data1$pdays, breaks = c(-2, -1, 90, 180, 270, 871),
                        labels = c("nc", "1_90d", "91_180d", "181_270d", "271d_plus"),
                        right = TRUE)
data1$pdays_disc <- data.frame(data1$pdays, categories_pdays)$categories_pdays
str(data)
categories_previous <- cut(data1$previous, breaks = c(-1,0, 1, 2, 3, 4, 5, 6, 7, 275),
                           labels = c("nc", "1c", "2c", "3c", "4c", "5c", "6c", "7c", "8c_plus"),
                           right = TRUE)
data1$previous_disc <- data.frame(data1$previous, categories_previous)$categories_previous
str(data)
data1 <- subset(data1, select = -c(pdays, previous))
str(data1)

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}
#5-fold cross-validation
set.seed(123)
cv1 <- createFolds(data1$y, k=5)

result1 <- list()
proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  set.seed(123)
  train_data1 <- data1[-cv1[[i]],]
  test_data1 <- data1[cv1[[i]],]

  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  }
  #Remove in train_data: default, poutcome
  #############decision tree###########
  set.seed(123)
  dt1 <- C5.0(train_data1[,!(names(train_data1) %in% c("default","poutcome","y"))],train_data1$y)
  dt1_pred <- predict(dt1, newdata = test_data1)
  cm <- confusionMatrix(dt1_pred, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt1_pred_prob <- predict(dt1, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,dt1_pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  result1[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#result
result1
avg_mcc1 <- mean(sapply(result1, function(res) res$MCC))
avg_mcc1
#0.4613763

###########3rd model: after tuning
#5-fold cross-validation
set.seed(123)
cv <- createFolds(data$y, k=5)
proportions <- list()
resultboost <- list()
for(i in 1:5) {
  # Split into training and validation set
  set.seed(123)
  train_data <- data[-cv[[i]],]
  test_data <- data[cv[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
    train_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
  }
  #Remove in train_data: default, poutcome
  #############decision tree###########
  set.seed(123)
  dt_boost10 <- C5.0(train_data[,!(names(train_data) %in% c("default","poutcome","y"))],train_data$y,trials = 10)
  dt_pred <- predict(dt_boost10, newdata = test_data)
  cm <- confusionMatrix(dt_pred, reference = test_data$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt_pred_prob <- predict(dt_boost10, newdata = test_data,type = "prob")
  actual_labels_numeric <- ifelse(test_data$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,dt_pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  resultboost[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
resultboost
avg_mcc <- mean(sapply(resultboost, function(res) res$MCC))
avg_mcc
#0.5057347
