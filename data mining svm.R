#load necessary libraries
library(ROSE)
library(kernlab)
library(caret)
library(mltools)
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
library(rcompanion)

#import data
data <- read.csv("C:/Users/kelly/Desktop/bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
str(data)

##############DATA PREPARATION ############################################

#change all integer variables to numeric variables
features <- colnames(data[,sapply(data,is.integer)])
str(features)

for(x in features){
  data[[x]] <- as.numeric(data[[x]])
}

###########################SVM#################################################
##########################1st Model########################################
#create function for calculating mcc
matthews_correlation_coefficient <- function(cm) {
  TN <- as.numeric(cm[1,1])
  TP <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[1,2])
  FN <- as.numeric(cm[2,1])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}
# Specify the file to store or retrieve results
results_file <- "my_results.rds"

# Check if previous results exist
if (file.exists(results_file)) {
  # Load stored results
  results <- readRDS(results_file)
  
  # Extracting metrics from stored results
  mcc_list <- results$mcc_list
  accuracy_list <- results$accuracy_list
  sensitivity_list <- results$sensitivity_list
  specificity_list <- results$specificity_list
} else {
  # If no previous results, compute them
  set.seed(123)
  folds <- createFolds(data$y, k=5)
  mcc_list <- vector("numeric", length(folds))
  accuracy_list <- vector("numeric", length(folds))
  specificity_list <- vector("numeric", length(folds))
  sensitivity_list <- vector("numeric", length(folds))
  
  # Loop through each fold
  for(i in 1:5) {
    # Split the data into training and validation sets
    train_data <- data[-folds[[i]],]
    test_data <- data[folds[[i]],]
    
    # Remove certain columns from training and test data
    train_data <- subset(train_data, select = -c(default, poutcome))
    test_data <- subset(test_data, select = -c(default, poutcome))
    
    # Scale numeric columns
    numeric_cols <- c("age", "balance", "day", "duration", "previous", "campaign","pdays")
    train_data[, numeric_cols] <- scale(train_data[, numeric_cols])
    test_data[, numeric_cols] <- scale(test_data[, numeric_cols])
    
    # Train SVM model
    model <- ksvm(y ~ ., data=train_data, kernel="rbfdot", C=1, kpar=list(sigma=0.1))
    
    # Predict on test data
    predictions <- predict(model, test_data)
    
    # Compute confusion matrix
    cm_obj <- confusionMatrix(predictions, test_data$y)
    
    # Extract metrics
    mcc <- matthews_correlation_coefficient(cm_obj$table)
    mcc_list[i] <- mcc
    accuracy <- cm_obj$overall['Accuracy']
    accuracy_list[i] <- accuracy
    sensitivity <- cm_obj$byClass['Sensitivity']
    sensitivity_list[i] <- sensitivity
    specificity <- cm_obj$byClass['Specificity']
    specificity_list[i] <- specificity
    
    # Display results for current fold
    cat(sprintf("Fold %d Confusion Matrix:\n", i))
    print(cm_obj$table)
    cat(sprintf("MCC: %f\n", mcc))
    cat(sprintf("Accuracy: %f\n", accuracy))
    cat(sprintf("Sensitivity: %f\n", sensitivity))
    cat(sprintf("Specificity: %f\n", specificity))
    cat("------------------------------\n")
  }
  
  # Store results for future use
  results <- list(
    mcc_list = mcc_list,
    accuracy_list = accuracy_list,
    sensitivity_list = sensitivity_list,
    specificity_list = specificity_list
  )
  saveRDS(results, results_file)
}

# Compute average metrics across all folds
avg_mcc <- mean(mcc_list) #0.407345
avg_accuracy <- mean(accuracy_list) #0.899781
avg_sensitivity <- mean(sensitivity_list) #0.976679
avg_specificity <- mean(specificity_list) #0.319340

# Display average results
cat(sprintf("Average MCC: %f\n", avg_mcc))
cat(sprintf("Average Accuracy: %f\n", avg_accuracy))
cat(sprintf("Average Sensitivity: %f\n", avg_sensitivity))
cat(sprintf("Average Specificity: %f\n", avg_specificity))



###############################2nd Model, Discretize previous and pdays#####################
data1 <- data

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
data1 <- subset(data1, select = -c(pdays, previous))
#create function for calculating mcc
matthews_correlation_coefficient <- function(cm) {
  TN <- as.numeric(cm[1,1])
  TP <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[1,2])
  FN <- as.numeric(cm[2,1])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}
# Specify the file to store or retrieve results
results_file1 <- "my_results1.rds"

# Check if previous results exist
if (file.exists(results_file1)) {
  # Load stored results
  results1 <- readRDS(results_file1)
  
  # Extracting metrics from stored results
  mcc_list1 <- results1$mcc_list
  accuracy_list1 <- results1$accuracy_list
  sensitivity_list1 <- results1$sensitivity_list
  specificity_list1 <- results1$specificity_list
} else {
  # If no previous results, compute them
  set.seed(123)
  folds <- createFolds(data1$y, k=5)
  mcc_list <- vector("numeric", length(folds))
  accuracy_list <- vector("numeric", length(folds))
  specificity_list <- vector("numeric", length(folds))
  sensitivity_list <- vector("numeric", length(folds))
  
  # Loop through each fold
  for(i in 1:5) {
    # Split the data into training and validation sets
    train_data <- data1[-folds[[i]],]
    test_data <- data1[folds[[i]],]
    
    # Remove certain columns from training and test data
    train_data <- subset(train_data, select = -c(default, poutcome))
    test_data <- subset(test_data, select = -c(default, poutcome))
    
    # Scale numeric columns
    numeric_cols <- c("age", "balance", "day", "duration", "campaign")
    train_data[, numeric_cols] <- scale(train_data[, numeric_cols])
    test_data[, numeric_cols] <- scale(test_data[, numeric_cols])
    
    # Train SVM model
    model <- ksvm(y ~ ., data=train_data, kernel="rbfdot", C=1, kpar=list(sigma=0.1))
    
    # Predict on test data
    predictions <- predict(model, test_data)
    
    # Compute confusion matrix
    cm_obj <- confusionMatrix(predictions, test_data$y)
    
    # Extract metrics
    mcc <- matthews_correlation_coefficient(cm_obj$table)
    mcc_list[i] <- mcc
    accuracy <- cm_obj$overall['Accuracy']
    accuracy_list[i] <- accuracy
    sensitivity <- cm_obj$byClass['Sensitivity']
    sensitivity_list[i] <- sensitivity
    specificity <- cm_obj$byClass['Specificity']
    specificity_list[i] <- specificity
    
    # Display results for current fold
    cat(sprintf("Fold %d Confusion Matrix:\n", i))
    print(cm_obj$table)
    cat(sprintf("MCC: %f\n", mcc))
    cat(sprintf("Accuracy: %f\n", accuracy))
    cat(sprintf("Sensitivity: %f\n", sensitivity))
    cat(sprintf("Specificity: %f\n", specificity))
    cat("------------------------------\n")
  }
  
  # Store results for future use
  results1 <- list(
    mcc_list = mcc_list,
    accuracy_list = accuracy_list,
    sensitivity_list = sensitivity_list,
    specificity_list = specificity_list
  )
  saveRDS(results1, results_file1)
}

# Compute average metrics across all folds
avg_mcc1 <- mean(mcc_list) #0.404248
avg_accuracy1 <- mean(accuracy_list) #0.899715
avg_sensitivity1 <- mean(sensitivity_list) #0.977481
avg_specificity1 <- mean(specificity_list) #0.312724

# Display average results
cat(sprintf("Average MCC: %f\n", avg_mcc1))
cat(sprintf("Average Accuracy: %f\n", avg_accuracy1))
cat(sprintf("Average Sensitivity: %f\n", avg_sensitivity1))
cat(sprintf("Average Specificity: %f\n", avg_specificity1))

###############################tuning for 1st model########################
#create function for calculating mcc
matthews_correlation_coefficient <- function(cm) {
  TN <- as.numeric(cm[1,1])
  TP <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[1,2])
  FN <- as.numeric(cm[2,1])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}
# Specify the file to store or retrieve results
results_file2 <- "my_results2.rds"

# Check if previous results exist
if (file.exists(results_file2)) {
  # Load stored results
  results2 <- readRDS(results_file2)
  
  # Extracting metrics from stored results
  mcc_list2 <- results2$mcc_list
  accuracy_list2 <- results2$accuracy_list
  sensitivity_list2 <- results2$sensitivity_list
  specificity_list2 <- results2$specificity_list
} else {
  # If no previous results, compute them
  set.seed(123)
  folds <- createFolds(data$y, k=5)
  mcc_list <- vector("numeric", length(folds))
  accuracy_list <- vector("numeric", length(folds))
  specificity_list <- vector("numeric", length(folds))
  sensitivity_list <- vector("numeric", length(folds))
  
  # Tuning ranges
  C_values <- c(0.1, 1, 10)
  sigma_values <- c(0.01, 0.1, 1)
  
  # Loop through each fold
  for(i in 1:5) {
    # Split the data into training and validation sets
    train_data <- data[-folds[[i]],]
    test_data <- data[folds[[i]],]
    
    # Remove certain columns from training and test data
    train_data <- subset(train_data, select = -c(default, poutcome))
    test_data <- subset(test_data, select = -c(default, poutcome))
    
    # Scale numeric columns
    numeric_cols <- c("age", "balance", "day", "duration", "previous", "campaign","pdays")
    train_data[, numeric_cols] <- scale(train_data[, numeric_cols])
    test_data[, numeric_cols] <- scale(test_data[, numeric_cols])
    
    # Grid search for tuning within each fold
    best_mcc <- -1 # start with the worst possible MCC
    best_C <- NA
    best_sigma <- NA
    for (C in C_values) {
      for (sigma in sigma_values) {
        model <- ksvm(y ~ ., data=train_data, kernel="rbfdot", C=C, kpar=list(sigma=sigma))
        predictions <- predict(model, test_data)
        
        # Compute confusion matrix for current parameters
        cm_temp <- table(predictions, test_data$y)
        current_mcc <- matthews_correlation_coefficient(cm_temp)
        
        if (!is.na(current_mcc) && current_mcc > best_mcc) {
          best_mcc <- current_mcc
          best_C <- C
          best_sigma <- sigma
        }
      }
    }
    
    # Train SVM model with best parameters
    model <- ksvm(y ~ ., data=train_data, kernel="rbfdot", C=best_C, kpar=list(sigma=best_sigma))
    
    # Predict on test data
    predictions <- predict(model, test_data)
    
    # Compute confusion matrix
    cm_obj <- confusionMatrix(predictions, test_data$y)
    
    # Extract metrics
    mcc <- matthews_correlation_coefficient(cm_obj$table)
    mcc_list[i] <- mcc
    accuracy <- cm_obj$overall['Accuracy']
    accuracy_list[i] <- accuracy
    sensitivity <- cm_obj$byClass['Sensitivity']
    sensitivity_list[i] <- sensitivity
    specificity <- cm_obj$byClass['Specificity']
    specificity_list[i] <- specificity
    
    # Display results for current fold
    cat(sprintf("Fold %d (Best C: %f, Best Sigma: %f)\n", i, best_C, best_sigma))
    print(cm_obj$table)
    cat(sprintf("MCC: %f\n", mcc))
    cat(sprintf("Accuracy: %f\n", accuracy))
    cat(sprintf("Sensitivity: %f\n", sensitivity))
    cat(sprintf("Specificity: %f\n", specificity))
    cat("------------------------------\n")
  }
  
  # Store results for future use
  results2 <- list(
    mcc_list = mcc_list,
    accuracy_list = accuracy_list,
    sensitivity_list = sensitivity_list,
    specificity_list = specificity_list
  )
  saveRDS(results2, results_file2)
}

# Compute average metrics across all folds
avg_mcc2 <- mean(mcc_list)
avg_accuracy2 <- mean(accuracy_list)
avg_sensitivity2 <- mean(sensitivity_list)
avg_specificity2 <- mean(specificity_list)

# Display average results
cat(sprintf("Average MCC: %f\n", avg_mcc2))
cat(sprintf("Average Accuracy: %f\n", avg_accuracy2))
cat(sprintf("Average Sensitivity: %f\n", avg_sensitivity2))
cat(sprintf("Average Specificity: %f\n", avg_specificity2))



