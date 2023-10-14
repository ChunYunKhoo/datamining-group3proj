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
library(xgboost)
library(mltools)
library(ROSE)
library(ROCR)
#import data
setwd('C:/Users/treei/OneDrive/Desktop/Courses/MH6151-Data Mining/project/datamining-group3proj-main')
data <- read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
str(data)

##############DATA PREPARATION ############################################

#change all integer variables to numeric variables
features <- colnames(data[,sapply(data,is.integer)])
str(features)

for(x in features){
  data[[x]] <- as.numeric(data[[x]])
}

str(data)

#check for missing values
summary(data)
MissingPercentage <- function(datax) {sum(is.na(datax))/length(datax)*100}
apply(data, 2, MissingPercentage)
#no missing values

#check for duplicate records
sum(duplicated(data))

str(data)

summary(data)
#check distribution of target variable
prop.table(table(data$y))
#target variable seems highly imbalanced towards no (> 88%)




set.seed(123)
split <- sample(1:nrow(data),size = round(0.7 * nrow(data)))
  
# Create training and testing sets
train_data <- data[split, ]
test_data <- data[-split, ]

#create upsampling training set
train_up <- caret::upSample(x = train_data %>% select(-y),
                     y = as.factor(train_data$y),
                     yname = "y")
str(train_up)
#check proportions
print("Train Dataset")
prop.table(table(train_data$y))
prop.table(table(train_up$y))
print("Test Dataset")
prop.table(table(test_data$y))

##############################################################################
#RANDOM FOREST
##########################################################################
matthews_correlation_coefficient <- function(cm) {
  tp <- cm[2,2]
  tn <- cm[1,1]
  fp <- cm[2,1]
  fn <- cm[1,2]
  
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}
############################################################################
#without default 
#without duration
#mtry = 3

results2 <- list()

# Define your cross-validation folds
set.seed(123)
folds <- createFolds(data$y,k=5)

# Loop through cross-validation folds
for (i in 1:5) {
  # Create training and test datasets for this fold
  print("fold")
  print(i)
  train_fold <- data[-folds[[2]], ]
  test_fold <- data[folds[[1]], ]
  
  # Apply oversampling to the training fold
  set.seed(123)
  train_oversampled <- ovun.sample(y~.,data = train_fold,method = "over", N=2*sum(train_fold$y == "no"))$data

  set.seed(123)
  rf_model2 <- randomForest(train_oversampled[,-c(5,12,17)],train_oversampled$y)
  # Save the model in a list  

  #Evaluate the model on the test fold and store results
  predictions <- predict(rf_model2, newdata = test_fold)
  cm <- confusionMatrix(predictions, reference = test_fold$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  
  #get the roc-auc value
  pred_prob <- predict(rf_model2, newdata = test_fold,type = "prob")
  actual_labels_numeric <- ifelse(actual_labels == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  results2[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
results2
save(results2,"results.rds")
load("results.rds")
avg_mcc <- mean(sapply(results2, function(res) res$MCC))
avg_mcc

