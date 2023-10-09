matthews_correlation_coefficient <- function(cm) {
  TP <- as.numeric(cm[1,1])
  TN <- as.numeric(cm[2,2])
  FP <- as.numeric(cm[2,1])
  FN <- as.numeric(cm[1,2])
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}

#load necessary libraries
library(ROSE)
library(kernlab)
library(caret)

data <- subset(data, select = -default)
# Prepare for 5-fold cross-validation
set.seed(12345)
folds <- createFolds(data$y, k=5)

results <- list()
proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  train_data <- data[-folds[[i]],]
  valid_data <- data[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
    train_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
  }
  
  # Scale only the specified numeric columns
  numeric_cols <- c(1, 5, 9, 11, 12, 13, 14)
  train_data[, numeric_cols] <- scale(train_data[, numeric_cols])
  valid_data[, numeric_cols] <- scale(valid_data[, numeric_cols])
  
  # Train SVM
  model <- ksvm(y ~ ., data=train_data, kernel="rbfdot", C=1, kpar=list(sigma=0.1))
  
  # Validate the model
  predictions <- predict(model, valid_data)
  # After the prediction step
  cm <- confusionMatrix(predictions, valid_data$y)$table
  mcc <- matthews_correlation_coefficient(cm)
  results[[i]] <- list(confusion=cm, MCC=mcc)
  
  # Print the confusion matrix
  cat(sprintf("Fold %d Confusion Matrix:\n", i))
  print(cm)
}

# Calculate average MCC
avg_mcc <- mean(sapply(results, function(res) res$MCC))
avg_mcc



