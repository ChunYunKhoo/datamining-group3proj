#################################### MODEL 1 #################################################
#Model 1: data1 (removing default and poutcome)
rf_result <- list()

# Define your cross-validation folds
set.seed(123)
folds1 <- createFolds(data1$y,k=5)

# Loop through cross-validation folds
for (i in 1:5) {
  print("fold")
  print(i)
  train_data1 <- data1[-folds1[[i]], ]
  test_data1 <- data1[folds1[[i]], ]
  
  # Apply oversampling to the training fold
  set.seed(123)
  train_oversampled1 <- ovun.sample(y~.,data = train_data1,method = "over", N=2*sum(train_data1$y == "no"))$data
  
  set.seed(123)
  rf2_model <- randomForest(train_oversampled1[,!(names(train_oversampled1) %in% c("default","poutcome","y"))],train_oversampled1$y)
  
  #Evaluate the model on the test fold and store results
  predictions <- predict(rf2_model, newdata = test_data1)
  cm <- confusionMatrix(predictions, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  
  #get the roc-auc value
  pred_prob <- predict(rf2_model, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  rf2_result[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
rf2_result
#save(rf2_result, file = "rf2_result.rds")
#average mcc
avg_mcc2 <- mean(sapply(rf2_result, function(res) res$MCC))
avg_mcc2
#average_metrics
average_metrics2 <- colMeans(do.call(rbind, lapply(rf2_result, function(x) x$Metric)))
average_metrics2
#load('rf2_result.rds')
#rf2_result
#avg_mcc2
#0.5503525
#Accuracy      Recall Specificity   Precision         AUC 
#0.9011745   0.6470029   0.9348480   0.5682124   0.9309471 
                                                   
##################################################### MODEL TUNE ##############################################
rf_tune_m <- vector("list", length = 3)
rf_tune_m_mcc <- list()
mtry_val <- list(4,6,8)

#Define your cross-validation folds
set.seed(123)
folds <- createFolds(data1$y,k=5)

for (k in 1:3){
  rf_tune_m[[k]] <- vector("list", length = 5)
  
  for(i in 1:5) {
    print(k)
    print("fold")
    print(i)
    train_data <- data1[-folds[[i]],]
    #test_data1 <- data1[folds[[i]],]
    set.seed(123)
    split <- sample(1:nrow(train_data),size = round(0.8 * nrow(train_data)))
    # Create training and validating sets
    train_data1 <- train_data[split, ]
    valid_data1 <- train_data[-split, ]
    
    set.seed(123)
    train_oversampled1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
    
    set.seed(123)
    tune <- randomForest(train_oversampled1[,!(names(train_oversampled1) %in% c("default","poutcome","y"))],train_oversampled1$y,mtry = mtry_val[[k]])
    
    
    #predict on test data and evaluate, then store 
    prediction <- predict(tune, newdata = valid_data1)
    
    cm <- confusionMatrix(prediction, mode = "everything", reference = valid_data1$y, positive = "yes")
    #create MCC function and find MCC value, store in m. 
    m <- matthews_correlation_coefficient(cm$table)
    pred_prob <- predict(tune, newdata = valid_data1,type = "prob")
    actual <- ifelse(valid_data1$y == "yes", 1, 0)
    roc_auc <- Metrics::auc(actual,pred_prob[,2])
    metrics_cal = rbind(
      c(cm$overall["Accuracy"], 
        cm$byClass["Recall"], 
        cm$byClass["Specificity"],
        cm$byClass["Precision"],
        roc_auc))
    colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
    metrics_cal
    
    #store confusion matrix and MCC
    rf_tune_m[[k]][[i]] <- list(confusion = cm$table, MCC = m, Metric = metrics_cal)
  }
  
  avg_mcc <- mean(sapply(rf_tune_m[[3]], function(res) res$MCC))
  avg_mcc
  
  rf_tune_m_mcc[[k]] <- avg_mcc
  
}

rf_tune_m_mcc
save(rf_tune_m, file = "rf_tune_manual2.rds")

#mtry = 4  mtry = 6   mtry = 8
#0.5348563 0.5368851  0.5298793

########################### Fit on mtry = 6 #################################
rf4_result <- list()

# Define your cross-validation folds
set.seed(123)
folds1 <- createFolds(data1$y,k=5)

# Loop through cross-validation folds
for (i in 1:5) {
  # Create training and test datasets for this fold
  print("fold")
  print(i)
  train_data1 <- data1[-folds1[[i]], ]
  test_data1 <- data1[folds1[[i]], ]
  
  # Apply oversampling to the training fold
  set.seed(123)
  train_oversampled1 <- ovun.sample(y~.,data = train_data1,method = "over", N=2*sum(train_data1$y == "no"))$data
  
  set.seed(123)
  rf4_model <- randomForest(train_oversampled1[,!(names(train_oversampled1) %in% c("default","poutcome","y"))],train_oversampled1$y,mtry = 6)
  
  #Evaluate the model on the test fold and store results
  predictions <- predict(rf4_model, newdata = test_data1)
  cm <- confusionMatrix(predictions, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  
  #get the roc-auc value
  pred_prob <- predict(rf4_model, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  rf4_result[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
rf4_result
save(rf4_result, file = "rf4_result.rds")
#average mcc
avg_mcc2 <- mean(sapply(rf4_result, function(res) res$MCC))
avg_mcc2
#average_metrics
average_metrics2 <- colMeans(do.call(rbind, lapply(rf4_result, function(x) x$Metric)))
average_metrics2
# Display the average metrics
rf4_result

#mcc
# 0.5350347

#Accuracy      Recall Specificity   Precision         AUC 
#0.9017716   0.6055951   0.9410099   0.5763259   0.9290742 

################################################ Model 2 ######################################################
rf_result1 <- list()

# Define your cross-validation folds
set.seed(123)
folds1 <- createFolds(data1$y,k=5)

# Loop through cross-validation folds
for (i in 1:5) {
  print("fold")
  print(i)
  train_data1 <- data1[-folds1[[i]], ]
  test_data1 <- data1[folds1[[i]], ]
  
  # Apply oversampling to the training fold
  set.seed(123)
  train_oversampled1 <- ovun.sample(y~.,data = train_data1,method = "over", N=2*sum(train_data1$y == "no"))$data
  
  set.seed(123)
  rf_model1 <- randomForest(train_oversampled1[,!(names(train_oversampled1) %in% c("default","poutcome","duration","campaign","y"))],train_oversampled1$y)
  
  #Evaluate the model on the test fold and store results
  predictions <- predict(rf_model1, newdata = test_data1)
  cm <- confusionMatrix(predictions, reference = test_data1$y,positive = "yes")
  m <- matthews_correlation_coefficient(cm$table)
  
  #get the roc-auc value
  pred_prob <- predict(rf_model1, newdata = test_data1,type = "prob")
  actual_labels_numeric <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(actual_labels_numeric,pred_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Recall"], 
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Recall", "Specificity","Precision","AUC")
  metrics_cal
  rf_result1[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}
#results
rf_result1
save(rf_result1, file = "rf_result1_without.rds")
#average mcc
avg_mcc2 <- mean(sapply(rf_result1, function(res) res$MCC))
avg_mcc2
#average_metrics
average_metrics2 <- colMeans(do.call(rbind, lapply(rf_result1, function(x) x$Metric)))
average_metrics2
#load('rf_result1.rds')
#rf_result1
#avg_mcc2
#0.3513213
# Accuracy      Recall Specificity   Precision         AUC 
# 0.8698546   0.4100953   0.9307650   0.4395941   0.7821059 
############################################################ Model Tune ####################################################
                                                   
                                                   
                                                   
