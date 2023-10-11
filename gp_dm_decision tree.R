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
#read "bank-full.csv" file
data <- read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
#Exploring and preparing the data
str(data)
summary(data)
data

#random sample the data, and divide them into training data (70%) and test data (the rest)
set.seed(123)
data <- subset(data, select = -default)

#5-fold cross-validation
set.seed(123)
cv <- createFolds(data$y, k=5)

results <- list()
proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  train_data <- data[-cv[[i]],]
  test_data <- data[cv[[i]],]

  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
    train_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
  }
}

  str(test_data)
  str(train_data)
  # Scale only the specified numeric columns
  numeric_cols <- c(1, 5, 9, 11, 12, 13, 14)
  train_data[, numeric_cols] <- scale(train_data[, numeric_cols])
  test_data[, numeric_cols] <- scale(test_data[, numeric_cols])

  str(test_data)
  str(train_data)

#############decision tree###########
set.seed(123)
dt <- C5.0(train_data[-16], train_data$y) 
summary(dt)

#Evaluating model performance
dt_pred <- predict(dt, test_data)

cm <- confusionMatrix(dt_pred,test_data$y,positive = "yes",mode = "everything")
cm
#Accuracy : 0.8113

# Calculate MCC
conf_matrix <- matrix(c(6574, 296, 1410, 761), nrow = 2, byrow = TRUE)
conf_matrix
tp <- conf_matrix[2, 2]
tn <- conf_matrix[1, 1]
fp <- conf_matrix[1, 2]
fn <- conf_matrix[2, 1]

mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
cat("Matthew's Correlation Coefficient (MCC):", mcc, "\n")
##Matthew's Correlation Coefficient (MCC): 0.4087204 