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
# Initialize a list to store results
library(ROCR)
library(pROC)
results2 <- list()

# Define your cross-validation folds
set.seed(123)
folds <- createFolds(data$y,k=5)

rf_models2 <- list()

# Loop through cross-validation folds
for (i in 1:5) {
  # Create training and test datasets for this fold
  print("fold")
  print(i)
  train_fold <- data[-folds[[2]], ]
  test_fold <- data[folds[[1]], ]
  
  # Apply oversampling to the training fold
  train_oversampled <- ovun.sample(y~.,data = train_fold,method = "over", N=2*sum(train_fold$y == "no"))$data
  rf_model2 <- randomForest(train_oversampled[,-c(5,12,17)],train_oversampled$y)
  # Save the model in a list
  rf_models2[[i]] <- rf_model2
  

  #Evaluate the model on the test fold and store results
  predictions <- predict(rf_model2, newdata = test_fold)
  cm <- confusionMatrix(predictions, reference = test_fold$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  
  #get the roc-auc value
  pred_prob <- predict(rf_model2, newdata = test_fold,type = "prob")
  actual_labels_numeric <- ifelse(actual_labels == "yes", 1, 0)
  roc_auc <- as.numeric(performance(p, "auc")@y.values[[1]])
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
save(results2, file = "results2.rds")
save(rf_models2,file = 'rf_models2.rda')

avg_mcc <- mean(sapply(results2, function(res) res$MCC))
avg_mcc
load('results2.rds')
results2
#########################################################################
#without default 
#with duration
#mtry = 3
# Initialize a list to store results
results <- list()

rf_models <- list()
set.seed(123)
# Loop through cross-validation folds
for (i in 1:5) {
  # Create training and test datasets for this fold
  print("fold",i)
  train_fold <- data[-folds[[i]], ]
  test_fold <- data[folds[[i]], ]
  
  # Apply oversampling to the training fold
  train_oversampled <- ovun.sample(y~.,data = train_fold,method = "over", N=2*sum(train_fold$y == "no"))$data
  rf_model <- randomForest(train_oversampled[,-c(5,17)],train_oversampled$y)
  # Save the best model in the list
  rf_models[[i]] <- rf_model
  
  # Evaluate the model on the test fold and store results
  predictions <- predict(rf_model, newdata = test_fold)
  cm <- confusionMatrix(predictions, reference = test_fold$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  pred_prob <- predict(rf_model, newdata = test_fold,type = "prob")
  actual_labels_numeric <- ifelse(actual_labels == "yes", 1, 0)
  p <- prediction(pred_prob[,2], actual_labels_numeric)
  roc_auc <- as.numeric(performance(p, "auc")@y.values[[1]])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  results[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
results
rf_model
save(results, file = "results.rds")
save(rf_models,file = 'rf_models')
avg_mcc <- mean(sapply(results, function(res) res$MCC))
avg_mcc
#######################################################################
#without default and poutcome
#mtry = 3
# Initialize a list to store results
results3 <- list()

rf_models3 <- list()
set.seed(123)
# Loop through cross-validation folds
for (i in 1:5) {
  # Create training and test datasets for this fold
  print("fold")
  print(i)
  train_fold <- data[-folds[[i]], ]
  test_fold <- data[folds[[i]], ]
  
  # Apply oversampling to the training fold
  if(sum(train_fold$y == "yes") < sum(train_fold$y == "no")) {
    train_oversampled <- ovun.sample(y ~ ., data = train_fold, method = "over", N = 2*sum(train_fold$y == "no"))$data
  }
  rf_model3 <- randomForest(train_oversampled[,-c(5,16,17)],train_oversampled$y)
  # Save the best model in the list
  rf_models3[[i]] <- rf_model3
  
  # Evaluate the model on the test fold and store results
  predictions <- predict(rf_model3, newdata = test_fold)
  cm <- confusionMatrix(predictions, reference = test_fold$y,positive = "yes")
  #m <- matthews_correlation_coefficient(cm$table)
  m <- mcc(predictions,test_fold$y)
  #get the roc-auc value
  roc_auc <- 1
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  results3[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
results3
rf_model3
varImpPlot(rf_model3)
avg_mcc <- mean(sapply(results3, function(res) res$MCC))
avg_mcc

#########################################################################
#without default 
#with duration
#mtry = 6
# Initialize a list to store results
hyperparameter_grid <- expand.grid(
  mtry = c(3,4,6)
)
results7 <- list()

# Define your cross-validation folds
set.seed(123)
folds <- createFolds(data$y,k=5)
rf_models7 <- list()

# Loop through cross-validation folds
for (i in 1:5) {
  # Create training and test data sets for this fold
  print("fold",i)
  train_fold <- data[-folds[[i]], ]
  test_fold <- data[folds[[i]], ]
  
  # Apply oversampling to the training fold
  train_oversampled <- ovun.sample(y~.,data = train_fold,method = "over", N=2*sum(train_fold$y == "no"))$data
  rf_tune <- train(y~., data = train_fold[,-c(5)], 
                    method = "rf",
                    metric = 'Accuracy',
                    tuneGrid = hyperparameter_grid)
  # Save the best model in the list
  rf_tunes[[i]] <- rf_tune$finalModel
  
  # Evaluate the model on the test fold and store results
  predictions <- predict(rf_tune$finalModel, newdata = test_fold)
  cm <- confusionMatrix(predictions, reference = test_fold$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  #get the roc-auc value
  pred_prob <- predict(rf_tune$finalModel, newdata = test_fold,type = "prob")
  p <- prediction(pred_prob[,2], test_fold$y)
  roc_auc <- as.numeric(performance(p, "auc")@y.values[[1]])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  results7[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
results7
rf_tunes
save(results, file = "results7.rds")
save(rf_models,file = 'rf_tunes')

##########################################################################

set.seed(123)
ctrl <- trainControl(method = "cv", number = 5,verboseIter = TRUE,savePredictions=TRUE)
set.seed(123)
r6<- train(y~., data = train_up[,-c(5,12)], 
           method = "rf",
           metric = 'Accuracy',
           trControl = ctrl,
           tuneGrid = hyperparameter_grid)
r6
r6_predict <- predict(r6, test_data)
confusionMatrix(r6_predict, test_data$y, mode = "everything", positive = "yes")
#mcc
mcc(r6_predict, test_data$y)
save(r6, file = "r6.rda")
# Precision : 0.43930         
#Recall : 0.44828 
#0.37

############################################################################
#with duration
hyperparameter_grid <- expand.grid(
  mtry = c(2,3,4,6)
)

set.seed(123)
ctrl <- trainControl(method = "cv", number = 5,verboseIter = TRUE,savePredictions=TRUE)
set.seed(123)
r9<- train(y~., data = train_up[,-5], 
           method = "rf",
           metric = 'Accuracy',
           trControl = ctrl,
           tuneGrid = hyperparameter_grid)
r9
r9_predict <- predict(r9, test_data)
confusionMatrix(r9_predict, test_data$y, mode = "everything", positive = "yes")
#mcc
mcc(r9_predict, test_data$y)
save(r9, file = "r9_duration.rda")
#Precision : 0.54054        
#Recall : 0.67688
#MCC: 0.5468471

