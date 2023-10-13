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

rm(list=ls())
#import data
data <- read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
str(data)

#remove "default" 
data <- subset(data, select = -default)

# Prepare for 5-fold cross-validation
set.seed(123)
folds <- createFolds(data$y, k=5)

results <- list()
proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  train_data <- data[-folds[[i]],]
  test_data <- data[folds[[i]],]
  
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

######Logistic Regression#####################################
##cross validation + feature selection (removing "default")##
log.model <- glm(y ~., data = train_data, family = binomial(link = "logit"))

summary(log.model)

## to predict using logistic regression model in test data, probabilities obtained
log_test_pred <- predict(log.model, test_data,type = 'response')
predicted_log_labels <- factor(ifelse(log_test_pred>= 0.5, "yes", "no"))

#generating confusion matrix
cm_log <- table(Predicted = predicted_log_labels, Reference = test_data[,16])
cm_log

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
cm_log <- confusionMatrix(predicted_log_labels, test_data_labels, 
                          positive = "yes",mode = "everything")
cm_log

cm_log$table[1,1]
cm_log$table[1,2]
cm_log$table[2,1]
cm_log$table[2,2]

mcc(confusionM = matrix(c(cm_log$table[1,1],cm_log$table[1,2],cm_log$table[2,1],
                          cm_log$table[2,2]),nrow=2,byrow = TRUE))

### finding of the cut-off point###
#Predict probabilities on the test data
log_test_pred_1 <- predict(log.model, test_data,type = 'response')
test_roc = roc(test_data$y ~ log_test_pred_1, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)


#write a function write a function 
#which allows use to make predictions based on different probability cutoffs.

get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

test_pred_10 = get_logistic_pred(log.model, data = test_data, res = "y", 
                                 pos = "yes", neg = "no", cut = 0.1)
test_pred_50 = get_logistic_pred(log.model, data = test_data, res = "y", 
                                 pos = "yes", neg = "no", cut = 0.5)
test_pred_90 = get_logistic_pred(log.model, data = test_data, res = "y", 
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

####remove "default" + "poutcome"##########################
data <- subset(data, select = -poutcome)
data

str(data)

# Prepare for 5-fold cross-validation
set.seed(123)
folds <- createFolds(data$y, k=5)

results <- list()
proportions <- list()

for(i in 1:5) {
  # Split into training and validation set
  train_data1 <- data[-folds[[i]],]
  test_data1 <- data[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  if(sum(train_data1$y == "yes") < sum(train_data1$y == "no")) {
    train_data1 <- ovun.sample(y ~ ., data = train_data1, method = "over", 
                               N = 2*sum(train_data1$y == "no"))$data
  }
}

# Scale only the specified numeric columns
numeric_cols <- c(1, 5, 9, 11, 12, 13, 14)
train_data1[, numeric_cols] <- scale(train_data1[, numeric_cols])
test_data1[, numeric_cols] <- scale(test_data1[, numeric_cols])

str(test_data1)
str(train_data1)

######Logistic Regression#####################################
##cross validation + feature selection (removing "default")##
log.model1 <- glm(y ~., data = train_data1, family = binomial(link = "logit"))

summary(log.model1)

## to predict using logistic regression model in test data, probabilities obtained
log_test_pred1 <- predict(log.model1, test_data1,type = 'response')
predicted_log_labels1 <- factor(ifelse(log_test_pred1>= 0.5, "yes", "no"))

#generating confusion matrix
cm_log1 <- table(Predicted = predicted_log_labels1, Reference = test_data1[,15])
cm_log1

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
cm_log1 <- confusionMatrix(predicted_log_labels1, test_data_labels1, 
                          positive = "yes",mode = "everything")
cm_log1

cm_log1$table[1,1]
cm_log1$table[1,2]
cm_log1$table[2,1]
cm_log1$table[2,2]

mcc(confusionM = matrix(c(cm_log1$table[1,1],cm_log1$table[1,2],cm_log1$table[2,1],
                          cm_log1$table[2,2]),nrow=2,byrow = TRUE))
