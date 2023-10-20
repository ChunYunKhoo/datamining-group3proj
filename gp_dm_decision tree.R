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
#model1: Remove default, poutcome and discretized previous and pdays
#read "bank-full.csv" file
data1 <-read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
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
categories_previous <- cut(data1$previous, breaks = c(-1,0, 1, 2, 3, 4, 5, 6, 7, 275),
                           labels = c("nc", "1c", "2c", "3c", "4c", "5c", "6c", "7c", "8c_plus"),
                           right = TRUE)
data1$previous_disc <- data.frame(data1$previous, categories_previous)$categories_previous
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
dt_cv1 <- createFolds(data1$y, k=5)

dt_result1 <- list()
dt_proportions <- list()

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
  dt_model1 <- C5.0(train_data1[,!(names(train_data1) %in% c("default","poutcome","y"))],train_data1$y)
  dt1_pred <- predict(dt_model1, newdata = test_data1)
  cm <- confusionMatrix(dt1_pred, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt1_pred_prob <- predict(dt_model1, newdata = test_data1,type = "prob")
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
  dt_result1[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#result
dt_result1
dt_avg_mcc1 <- mean(sapply(dt_result1, function(res) res$MCC))
dt_avg_mcc1
##0.4613763
#average_metrics
dt_average_metrics1 <- colMeans(do.call(rbind, lapply(dt_result1, function(x) x$Metric)))
dt_average_metrics1
#Accuracy      Recall.   Specificity   Precision         AUC 
#0.8705847    0.6186396   0.9039628   0.4603018      0.8930833 

###############model 1 Boosting#####################
#5-fold cross-validation
set.seed(123)
dt_cv2<- createFolds(data$y, k=5)
dt_proportions <- list()
dt_result1boost <- list()
for(i in 1:5) {
  # Split into training and validation set
  set.seed(123)
  train_data1 <- data1[-cv[[i]],]
  test_data1 <- data1[cv[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  }
  #Remove in train_data: default, poutcome
  #############decision tree###########
  set.seed(123)
  dt1_boost10 <- C5.0(train_data1[,!(names(train_data1) %in% c("default","poutcome","y"))],train_data1$y,trials = 10)
  dt_pred_boost <- predict(dt1_boost10, newdata = test_data1)
  cm <- confusionMatrix(dt_pred_boost, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt_pred_boost_prob <- predict(dt_boost10, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,dt_pred_boost_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  dt_result1boost[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
dt_result1boost
dt_avg_mcc2 <- mean(sapply(dt_result1boost, function(res) res$MCC))
dt_avg_mcc2##0.4990969
#average_metrics
dt_average_metrics2 <- colMeans(do.call(rbind, lapply(dt_result1boost, function(x) x$Metric)))
dt_average_metrics2
#   Accuracy   Recall    Specificity   Precision         AUC 
# 0.8940746   0.5738318   0.9365012    0.5449931     0.9815806 


############Business model 2################################################
#5-fold cross-validation
set.seed(123)
dt_cv3 <- createFolds(data$y, k=5)

dt_result2 <- list()
dt_proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  set.seed(123)
  train_data1 <- data1[-dt_cv3[[i]],]
  test_data1 <- data1[dt_cv3[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  }
  #Remove in train_data: default, poutcome, duration, campaign
  #############decision tree###########
  set.seed(123)
  dt3 <- C5.0(train_data1[,!(names(train_data1) %in% c("default","poutcome","duration","campaign","y"))],train_data1$y)
  dt3_pred <- predict(dt3, newdata = test_data1)
  cm <- confusionMatrix(dt3_pred, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt3_pred_prob <- predict(dt3, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,dt3_pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  dt_result2[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#result
dt_result2
dt_avg_mcc3 <- mean(sapply(dt_result2, function(res) res$MCC))
dt_avg_mcc3
#0.2211623
#average_metrics
dt_average_metrics3 <- colMeans(do.call(rbind, lapply(dt_result2, function(x) x$Metric)))
dt_average_metrics3
#Accuracy      Recall   Specificity   Precision         AUC 
#0.8065515   0.3974266   0.8607534   0.2743311       0.7156306 



###########4th model: tuning 3rd model
#5-fold cross-validation
set.seed(123)
dt_cv4<- createFolds(data$y, k=5)
proportions <- list()
dt_result2boost <- list()
for(i in 1:5) {
  # Split into training and validation set
  set.seed(123)
  train_data1 <- data1[-dt_cv4[[i]],]
  test_data1 <- data1[dt_cv4[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  }
  #Remove in train_data: default, poutcome, duration, campaign
  #############decision tree###########
  set.seed(123)
  dt3_boost10 <- C5.0(train_data1[,!(names(train_data1) %in% c("default","poutcome","duration","campaign","y"))],train_data1$y,trials = 10)
  dt3_pred_boost <- predict(dt3_boost10, newdata = test_data1)
  cm <- confusionMatrix(dt3_pred_boost, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  dt3_pred_boost_prob <- predict(dt_boost10, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,dt3_pred_boost_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  dt_result2boost[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
dt_result2boost
dt_avg_mcc4 <- mean(sapply(dt_result2boost, function(res) res$MCC))
dt_avg_mcc4
#0.3006906
#average_metrics
dt_average_metrics4 <- colMeans(do.call(rbind, lapply(dt_result2boost, function(x) x$Metric)))
dt_average_metrics4
#Accuracy      Recall    Specificity   Precision         AUC 
#0.8708722    0.3197166   0.9438907    0.4302007     0.9815806 
                                                      
