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
library(Metrics)
library(mlbench)
library(ROSE)
library(caret)
library(pROC)
library(mltools)
library(readr)
library(tidymodels)

rm(list=ls())
#import data
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

############Business model 1################################################
###Logistic Regression (baseline model)#######
##remove in train_data: Default, Poutcome ####

matthews_correlation_coefficient <- function(cm) {
  TP <- as.numeric(cm[1,1])
  TN <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[2,1])
  FN <- as.numeric(cm[1,2])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}

# Prepare for 5-fold cross-validation
set.seed(123)
folds <- createFolds(data$y, k=5)

log_results <- list()
log_proportions <- list()

for(i in 1:5) {
  # Split into training and test set
  train_data <- data[-folds[[i]],]
  test_data <- data[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  set.seed(123)
  if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
    train_data <- ovun.sample(y ~ ., data = train_data, method = "over", 
                              N = 2*sum(train_data$y == "no"))$data
  }
  #remove default and poutcome in train set after cross-validation
  train_data <- subset(train_data, select= c(-default, -poutcome))
  
  # Scale only the specified numeric columns
  numeric_cols_test <- c(1, 6, 10, 12, 13, 14, 15)
  numeric_cols_train <- c(1, 5, 9, 11, 12, 13, 14)
  train_data[, numeric_cols_train] <- scale(train_data[, numeric_cols_train])
  test_data[, numeric_cols_test] <- scale(test_data[, numeric_cols_test])
  
  # Train log regression
  set.seed(123)
  log_model <- glm(y ~., data = train_data, family = binomial(link = "logit"))
  
  ## to predict using logistic regression model in test data, probabilities obtained
  # Validate the model
  log_test_pred <- predict(log_model, test_data,type = 'response')
  predicted_log_labels <- factor(ifelse(log_test_pred>= 0.5, "yes", "no"))
  
  #generating confusion matrix
  table_log <- table(Predicted = predicted_log_labels, Reference = test_data[,15])
  table_log
  
  test_data_labels <- as.factor(test_data$y)
  length(test_data_labels)
  length(predicted_log_labels)
  
  str(test_data_labels)
  str(predicted_log_labels)
  
  table(test_data_labels)
  table(predicted_log_labels)
  if(!identical(levels(predicted_log_labels), levels(test_data_labels))) {
    levels(predicted_log_labels) <- levels(test_data_labels)
  }
  
  #confusion matrix
  cm_metrics <- confusionMatrix(predicted_log_labels, test_data_labels, 
                                      positive = "yes",mode = "everything")
  print(cm_metrics)
  cm <- confusionMatrix(predicted_log_labels, test_data_labels, 
                             positive = "yes",mode = "everything")$table
  mcc <- matthews_correlation_coefficient(cm)
  log_results[[i]] <- list(confusion=cm, MCC=mcc)

  # Print the confusion matrix
  cat(sprintf("Fold %d Confusion Matrix:\n", i))
  print(cm)
  
}

avg_mcc <- mean(sapply(log_results, function(res) res$MCC))
avg_mcc

##2nd model: baseline w discretized previous and pdays###

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

###Logistic Regression (2nd model)#######
##Discretized previous and pdays #############
##remove in train_data: Default, Poutcome ####

matthews_correlation_coefficient <- function(cm) {
  TP <- as.numeric(cm[1,1])
  TN <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[2,1])
  FN <- as.numeric(cm[1,2])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}

# Prepare for 5-fold cross-validation
set.seed(123)
folds1 <- createFolds(data1$y, k=5)

log_results1 <- list()
log_proportions1 <- list()

for(i in 1:5) {
  # Split into training and test set
  train_data1 <- data1[-folds1[[i]],]
  test_data1 <- data1[folds1[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  set.seed(123)
  if(sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", 
                              N = 2*sum(train_data1$y == "no"))$data
  }
  #remove default and poutcome in train set after cross-validation
  train_data1 <- subset(train_data1, select= c(-default, -poutcome))
  
  # Scale only the specified numeric columns
  numeric_cols_test1 <- c(1, 6, 10, 12, 13)
  numeric_cols_train1 <- c(1, 5, 9, 11, 12)
  train_data1[, numeric_cols_train1] <- scale(train_data1[, numeric_cols_train1])
  test_data1[, numeric_cols_test1] <- scale(test_data1[, numeric_cols_test1])
  
  # Train log regression
  set.seed(123)
  log_model1 <- glm(y ~., data = train_data1, family = binomial(link = "logit"))
  
  ## to predict using logistic regression model in test data, probabilities obtained
  # Validate the model
  log_test_pred1 <- predict(log_model1, test_data1,type = 'response')
  predicted_log_labels1 <- factor(ifelse(log_test_pred1>= 0.5, "yes", "no"))
  
  #generating confusion matrix
  table_log1 <- table(Predicted = predicted_log_labels1, Reference = test_data1[,13])
  table_log1
  
  test_data_labels1 <- as.factor(test_data1$y)
  length(test_data_labels1)
  length(predicted_log_labels1)
  
  str(test_data_labels1)
  str(predicted_log_labels1)
  
  table(test_data_labels1)
  table(predicted_log_labels1)
  if(!identical(levels(predicted_log_labels1), levels(test_data_labels1))) {
    levels(predicted_log_labels1) <- levels(test_data_labels1)
  }
  
  #confusion matrix
  cm_metrics1 <- confusionMatrix(predicted_log_labels1, test_data_labels1, 
                                      positive = "yes",mode = "everything")
  print(cm_metrics1)
  cm <- confusionMatrix(predicted_log_labels1, test_data_labels1, 
                        positive = "yes",mode = "everything")$table
  mcc <- matthews_correlation_coefficient(cm)
  log_results1[[i]] <- list(confusion=cm, MCC=mcc)
  
  # Print the confusion matrix
  cat(sprintf("Fold %d Confusion Matrix:\n", i))
  print(cm)
  
}

avg_mcc1 <- mean(sapply(log_results1, function(res) res$MCC))
avg_mcc1

### finding of the cut-off point###
#Predict probabilities on the test data
log_test_pred <- predict(log_model, test_data,type = 'response')
test_roc = roc(test_data$y ~ log_test_pred, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)


#write a function write a function 
#which allows use to make predictions based on different probability cutoffs.

get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

test_pred_10 = get_logistic_pred(log_model, data = test_data, res = "y", 
                                 pos = "yes", neg = "no", cut = 0.1)
test_pred_50 = get_logistic_pred(log_model, data = test_data, res = "y", 
                                 pos = "yes", neg = "no", cut = 0.5)
test_pred_90 = get_logistic_pred(log_model, data = test_data, res = "y", 
                                 pos = "yes", neg = "no", cut = 0.9)

test_pred_10
length(test_pred_10)
length(test_data$y)

test_tab_10 = table(predicted = test_pred_10, actual = test_data$y)
test_tab_50 = table(predicted = test_pred_50, actual = test_data$y)
test_tab_90 = table(predicted = test_pred_90, actual = test_data$y)

test_con_mat_10 = confusionMatrix(test_tab_10, positive = "yes")
test_con_mat_50 = confusionMatrix(test_tab_50, positive = "yes")
test_con_mat_90 = confusionMatrix(test_tab_90, positive = "yes")

metrics = rbind(
  
  c(test_con_mat_10$overall["Accuracy"], 
    test_con_mat_10$byClass["Sensitivity"], 
    test_con_mat_10$byClass["Specificity"]),
  
  c(test_con_mat_50$overall["Accuracy"], 
    test_con_mat_50$byClass["Sensitivity"], 
    test_con_mat_50$byClass["Specificity"]),
  
  c(test_con_mat_90$overall["Accuracy"], 
    test_con_mat_90$byClass["Sensitivity"], 
    test_con_mat_90$byClass["Specificity"])
  
)
rownames(metrics) = c("c = 0.10", "c = 0.50", "c = 0.90")
metrics

#A good model will have a high AUC, 
#that is as often as possible a high sensitivity and specificity.

#will stick with cutoff of 0.5 since we want a 
#balanced trade-off between sensitivity and specificity, 
#then c = 0.50 seems like a reasonable choice. 
#It has reasonably high sensitivity and specificity.


##improve the model based on the 3 cut off points ###
###Logistic Regression (3rd model)#######
##Discretized previous and pdays #############
##remove in train_data: Default, Poutcome ####

matthews_correlation_coefficient <- function(cm) {
  TP <- as.numeric(cm[1,1])
  TN <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[2,1])
  FN <- as.numeric(cm[1,2])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}

# Initialize a list to store results
log_results2 <- vector("list", length = 5)

for (i in 1:5) {
  set.seed(123)  # Ensure consistent results within each fold
  
  # Split into training and test set
  train_data1 <- data1[-folds1[[i]], ]
  test_data1 <- data1[folds1[[i]], ]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if (sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", 
                               N = 2 * sum(train_data1$y == "no"))$data
  }
  
  # Remove 'default' and 'poutcome' in the train set after cross-validation
  train_data1 <- subset(train_data1, select = -c(default, poutcome))
  
  # Scale only the specified numeric columns
  numeric_cols_train1 <- c(1, 5, 9, 11, 12)
  numeric_cols_test1 <- c(1, 6, 10, 12, 13)
  train_data1[, numeric_cols_train1] <- scale(train_data1[, numeric_cols_train1])
  test_data1[, numeric_cols_test1] <- scale(test_data1[, numeric_cols_test1])
  
  # Train logistic regression
  set.seed(123)
  log_model1 <- glm(y ~ ., data = train_data1, family = binomial(link = "logit"))
  
  # Predict using the logistic regression model in test data, probabilities obtained
  log_test_pred2 <- predict(log_model1, test_data1, type = 'response')
  
  # Define probability thresholds for classification
  thresholds <- c(0.1, 0.5, 0.9)
  
  # Initialize a list to store results for this fold
  fold_results <- list()
  fold_mcc_values <- list()
  
  for (threshold in thresholds) {
    predicted_log_labels2 <- factor(ifelse(log_test_pred2 >= threshold, "yes", "no"))
    
    # Generate the confusion matrix
    cm <- table(Predicted = predicted_log_labels2, Reference = test_data1$y)
    
    # Calculate MCC
    mcc <- matthews_correlation_coefficient(cm)
    
    # Store MCC value in the fold_mcc_values list
    fold_mcc_values[[as.character(threshold)]] <- mcc
    
    # Use confusionMatrix function to calculate other metrics
    confusion_matrix_result <- confusionMatrix(predicted_log_labels2, test_data1$y, 
                                               positive = "yes", mode = "everything")
    
    # Store confusion matrix and MCC in the fold_results list
    fold_results[[as.character(threshold)]] <- list(
      confusion = cm, 
      MCC = mcc
    )
  }
  
  # Calculate the average MCC for each threshold level across all folds
  avg_mcc_per_threshold <- sapply(thresholds, function(j) {
    mean(sapply(log_results2, function(res) res$fold_mcc_values[[as.character(j)]]))
  })
  
  # Store fold_mcc_values and avg_mcc_per_threshold in the log_results2 list
  log_results2[[i]] <- list(
    fold_mcc_values = fold_mcc_values)
  
  # Print the confusion matrix for each threshold
  cat(sprintf("Fold %d Confusion Matrix:\n", i))
  for (threshold in thresholds) {
    print(fold_results[[as.character(threshold)]])
  }
}

# Calculate the average MCC over all folds
log_results2


############Business model 2################################################
############data1 + removal of duration + campaign##########################
###Logistic Regression (baseline model)#######
##remove in train_data: Default, Poutcome + duration + campaign####

matthews_correlation_coefficient <- function(cm) {
  TP <- as.numeric(cm[1,1])
  TN <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[2,1])
  FN <- as.numeric(cm[1,2])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}

# Prepare for 5-fold cross-validation
set.seed(123)
folds <- createFolds(data$y, k=5)

log_resultsb <- list()
log_proportionsb <- list()

for(i in 1:5) {
  # Split into training and test set
  train_datab <- data[-folds[[i]],]
  test_datab <- data[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  set.seed(123)
  if(sum(train_datab$y == "yes") < sum(train_datab$y == "no")) {
    train_datab <- ovun.sample(y ~ ., data = train_datab, method = "over", 
                              N = 2*sum(train_datab$y == "no"))$data
  }
  #remove default and poutcome in train set after cross-validation
  train_datab <- subset(train_data, select= c(-default, -poutcome, -duration, -campaign))
  
  # Scale only the specified numeric columns
  numeric_cols_testb <- c(1, 6, 10, 12, 13, 14, 15)
  numeric_cols_trainb <- c(1, 5, 9, 11, 12)
  train_datab[, numeric_cols_trainb] <- scale(train_datab[, numeric_cols_trainb])
  test_datab[, numeric_cols_testb] <- scale(test_datab[, numeric_cols_testb])
  
  # Train log regression
  set.seed(123)
  log_modelb <- glm(y ~., data = train_datab, family = binomial(link = "logit"))
  
  ## to predict using logistic regression model in test data, probabilities obtained
  # Validate the model
  log_test_predb <- predict(log_modelb, test_datab,type = 'response')
  predicted_log_labelsb <- factor(ifelse(log_test_predb>= 0.5, "yes", "no"))
  
  #generating confusion matrix
  table_logb <- table(Predicted = predicted_log_labelsb, Reference = test_datab[,13])
  table_logb
  
  test_data_labelsb <- as.factor(test_datab$y)
  length(test_data_labelsb)
  length(predicted_log_labelsb)
  
  str(test_data_labelsb)
  str(predicted_log_labelsb)
  
  table(test_data_labelsb)
  table(predicted_log_labelsb)
  if(!identical(levels(predicted_log_labelsb), levels(test_data_labelsb))) {
    levels(predicted_log_labelsb) <- levels(test_data_labelsb)
  }
  
  #confusion matrix
  cm_metricsb <- confusionMatrix(predicted_log_labelsb, test_data_labelsb, 
                                positive = "yes",mode = "everything")
  print(cm_metricsb)
  
  #confusion matrix
  cm <- confusionMatrix(predicted_log_labelsb, test_data_labelsb, 
                        positive = "yes",mode = "everything")$table
  mcc <- matthews_correlation_coefficient(cm)
  log_resultsb[[i]] <- list(confusion=cm, MCC=mcc)
  
  # Print the confusion matrix
  cat(sprintf("Fold %d Confusion Matrix:\n", i))
  print(cm)
  
}

avg_mccb <- mean(sapply(log_resultsb, function(res) res$MCC))
avg_mccb

############Business model 2################################################
############data1 + removal of duration + campaign##########################
###Logistic Regression (2nd model)#######
##Discretized previous and pdays #############
##remove in train_data: Default, Poutcome + duration + campaign####
