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
install.packages('rcompanion')
install.packages("mltools")
install.packages("grid")
install.packages("gridExtra")
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
library(grid)
library(gridExtra)

#import data
data <- read.csv("C:/Users/chuny/Documents/GitHub/datamining-group3proj/bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
str(data)

##############DATA PREPARATION ############################################

#change all integer variables to numeric variables
features <- colnames(data[,sapply(data,is.integer)])
str(features)

for(x in features){
  data[[x]] <- as.numeric(data[[x]])
}

str(data)

#create data1 for 2nd model
data1 <- data
str(data1)


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


##############DATA EXPLORATION ############################################

#pairplot for numerical variables to get an overview
num_var <- data[,c("age","balance","duration", "campaign", "pdays", "previous", "day")]
ggpairs(num_var, title="exploration of numeric variables")+
theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))
# ggcorrplot(cor(num_var), method = "number")
#pdays and previous have a corr = 0.45, but still not that high that it is concerning

##############age and y
grid.arrange(
  
  ggplot(data=data, mapping=aes(x=age, fill=y)) +
    geom_histogram(position = "identity", binwidth = 5)+
    scale_x_continuous(name="", breaks=seq(15,100,5))+
    scale_y_continuous(name="")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=9)),
  
  ggplot(data=data, mapping=aes(x=age, fill=y)) +
    geom_histogram(position = "fill", binwidth = 5) +
    scale_x_continuous(name="", breaks=seq(15,100,5)) + 
    scale_y_continuous(name="")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=9)),
  
  nrow = 2,
  
  top="Age and y"
)

#mean age for both target class values are similar.
#no: mode age range is between 32.5 to 35.5
#yes:mode age range id between 27.5 to 37.5
#boxplot
ggplot(data = data, aes(x=y, y=age, fill = y)) +
  geom_boxplot(alpha = 0.5)
#yes and no roughly have similar age range,corresponds to age range of clients


#age distribution for each class value of y
# create mean by class
mean <- data %>% group_by(y)%>%summarise(mean_val=mean(age))
mean
#those who say yes and no have similar mean


###############balance and y

#after

#boxplot
grid.arrange(
  ggplot(data = data, aes(x=y, y=balance, fill = y)) + geom_boxplot(alpha = 0.5) + coord_flip(),
  nrow = 1,
  top="balance and y"
)

#histogram
grid.arrange(
ggplot(data = data, aes(x=balance, fill=y)) + geom_histogram(position = "identity") + xlim(-3000,8000),
nrow = 1,
top="balance and y"
)

#distribution of no and yes across balance is similar. balance may not be significant. 

###############day and y

grid.arrange(
  ggplot(data = data, aes(x=y, y=day, fill = y)) + geom_boxplot(alpha = 0.5)+ coord_flip()+
    scale_y_continuous(name="", breaks=seq(0,31,1))+
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5, size=8)),
  ggplot(data = data, aes(x=day, fill=y)) + 
    scale_x_continuous(name="", breaks=seq(0,31,1))+
    geom_histogram(position = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=8)),
  nrow = 2,
  
  top="day and y"
  
)


###############duration and y

#check duration with y since when duration = 0, likely y = no.
num_y <- as.numeric(data$y)
cor.test(data$duration, num_y) #some weak-moderate correlation at 0.39
filter(data, duration == 0 & y == 'no')
#only 3 records with 0 & y=no, there could be something more meaningful to explore for other ranges of duration

#duration, y
m <- data %>% group_by(y)%>% summarise(m_d=mean(duration))
m
summary(data$duration)
#the mean call duration is longer for those who subscribed

#boxplot and histogram to explore relationship between duration and y
grid.arrange(
  ggplot(data = data, aes(x=y, y=duration, fill = y)) + geom_boxplot(alpha = 0.5),
  ggplot(data = data, aes(x=duration, fill=y)) + 
    geom_histogram(position = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=7))+
    scale_x_continuous(breaks =seq(0,5000,300), limits = c(0,1800)),
  
  nrow = 1,
  
  top="duration and y"
)

#further analysis

#campaigns, duration, y
ggplot(data = data, aes(campaign, duration, colour = y)) + geom_point()
#number of people subscribing is higher when number of contacts performed are low and duration of call is longer (>5 min)


################pdays and y

summary(data)
filter(data, pdays == -1) #36954 customers
filter(data, pdays > -1 & pdays <= 90) #718 customers were contacted previously within 3 months back
filter(data, pdays > 90 & pdays <= 180) #2480 customers were contacted 3-6 months ago
filter(data, pdays > 180 & pdays <= 270) #2082 customers in 6-9 months ago
filter(data, pdays > 270) #2977 customers called more than 9 months ago
datav <- data
categories_pdays <- cut(datav$pdays, breaks = c(-2, -1, 90, 180, 270, 871),
                        labels = c("-1 i.e. no contact", "1_90d", "91_180d", "181_270d", "271d_plus"),
                        right = TRUE)
datav$pdays_grouped <- data.frame(datav$pdays, categories_pdays)$categories_pdays
str(data1)
CrossTable(datav$pdays_grouped, data$y)

#zoom into the range of positive pdays
CrossTable(data$pdays, data$y)

#zoom in to see that majority of observations have '-1'value
grid.arrange(
ggplot(data = data, aes(x = pdays, fill=y)) + geom_histogram(binwidth = 1)+
scale_x_continuous(breaks=c(-1,0,1,2,3,4,5,6,7), limits = c(-2,7)),
top="pdays and y"
)

#observe distribution for positive values of pdays
grid.arrange(
  ggplot(data = data, aes(x = pdays, fill=y)) +geom_histogram(binwidth = 5) + xlim(0,500)+
    scale_x_continuous(breaks=c(0,30,60,90,120,150,180,210,240,270,300,365), limits = c(0,500)),
  top="pdays and y"
)

################previous and y

#previous,y
summary(data$previous)
#strange outlier of previous = 275. this means the bank called them 275 times previously! quite implausible
#check top 10 highest values in previous
top10_previous <- data %>%                                     
  arrange(desc(data$previous)) %>% 
  slice(1:10)
top10_previous #top 1 is 275, & the rest are between 30-50+.
p_y <- data %>%group_by(y) %>% summarise(average=mean(previous)) 
p_y #higher mean number of contacts performed for those who subscribed
CrossTable(data$previous, data$y, prop.c = F, prop.t = F, prop.chisq = F)
#the no. of records for each value beyond previous >9 is <100,quite small
#possible to group records above a certain value into 1 group.
#seems like cdf curve starts flattening after 10, proportion of 'previous' seem to stabilize
plot(ecdf(data[,"previous"]),
     xlim=c(0,50),
     col="blue")
#to take a closer look, limit x to 20
plot(ecdf(data[,"previous"]),
     xlim=c(0,10),
     col="blue")
#can group all those > 7 as one group


#check distribution of previous
grid.arrange(
  ggplot(data = data, aes(x = previous, y=y, color=y)) + geom_point() + scale_x_continuous(breaks=seq(0,300,10)),
  
  ggplot(data = data, aes(x = previous, y=y, color=y)) + 
    geom_boxplot(alpha=0.5) + scale_x_continuous(breaks=seq(0,10,1), limits = c(1,10)),
  
  nrow=2,
  
  top="distribution of previous in relation to y"
)


#####################overview of categorical variables
str(data)
summary(data)

grid.arrange(
  
  ggplot(data=data, mapping=aes(x=job, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=7)),
  
  ggplot(data=data, mapping=aes(x=marital, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop"),
  
  ggplot(data=data, mapping=aes(x=education, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop")+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=7)),
  
  ggplot(data=data, mapping=aes(x=default, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop"),
  
  ggplot(data=data, mapping=aes(x=housing, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop"),
  
  ggplot(data=data, mapping=aes(x=loan, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop"),
  
  ggplot(data=data, mapping=aes(x=contact, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop"),
  
  ggplot(data=data, mapping=aes(x=month, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=7)),
  
  ggplot(data=data, mapping=aes(x=poutcome, fill=y)) +
    geom_bar(position = "fill") +
    scale_y_continuous(name="Prop")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size=7)),
  
  nrow=3,
  
  top="overview of categorical variables")

##singles have highest proportion of subscribing.
#mar, dec, oct, sep have higher proportion of yes than nos. for dec

##pull out unknowns to show that only unknowns for poutcome is overwhelming and will be removed 
grid.arrange(
  
  ggplot(data=data, mapping=aes(x=job, fill=y)) +
    geom_bar(position = "identity") +
    scale_y_continuous(name="Count")+
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size=7)),
  
  
  ggplot(data=data, mapping=aes(x=education, fill=y)) +
    geom_bar(position = "identity") +
    scale_y_continuous(name="Count")+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=7)),
  
  
  ggplot(data=data, mapping=aes(x=contact, fill=y)) +
    geom_bar(position = "identity") +
    scale_y_continuous(name="Count"),
  
  ggplot(data=data, mapping=aes(x=poutcome, fill=y)) +
    geom_bar(position = "identity") +
    scale_y_continuous(name="Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size=7)),
  
  nrow=2,
  
  top="Categorical variables with 'unknown'")


#####nature of call vs y

#campaigns, duration, y
ggplot(data = data, aes(campaign, duration, colour = y)) + geom_point()
#number of people subscribing is higher when number of contacts performed are low and duration of call is longer (>5 min)


#previous, poutcome and y
ggplot(data = data, aes(previous, poutcome, colour = y)) + geom_point()
#clients who were previously contacted with failure also tend to not subscribe to term deposit.

p_y <- data %>%group_by(y) %>% summarise(mean(pdays)) 
p_y
#mean number of days for yes is a lot higher than that for no. 

#explore pdays and y
num_y <- as.numeric(data$y)
cor.test(data$pdays, num_y)
#pdays has very weak correlation with target variable, can keep
summary(data$pdays) #median is -1.0
describe(data$pdays) #559 distinct values
filter(data, pdays == -1) #36954 customers
#the rest of the records ranges from 1-871, huge range. 
#may need to discretize into bins for more meaningful interpretation


########################DATA PREPROCESSING#########################################

#discretizing pdays
#zoom into range where most positive values lie in order to gauge how to bin
ggplot(data = data, aes(x = pdays)) +geom_histogram(binwidth = 5) + xlim(0,500) 
filter(data, pdays == -1) #36954 customers
filter(data, pdays > -1 & pdays <= 90) #718 customers were contacted previously within 3 months back
filter(data, pdays > 90 & pdays <= 180) #2480 customers were contacted 3-6 months ago
filter(data, pdays > 180 & pdays <= 270) #2082 customers in 6-9 months ago
filter(data, pdays > 270) #2977 customers called more than 9 months ago
#discretizing previous into 5 categories in new variable pdays_disc
categories_pdays <- cut(data1$pdays, breaks = c(-2, -1, 90, 180, 270, 871),
                        labels = c("nc", "1_90d", "91_180d", "181_270d", "271d_plus"),
                        right = TRUE)
data1$pdays_disc <- data.frame(data1$pdays, categories_pdays)$categories_pdays
str(data1)
CrossTable(data1$pdays_disc, data$y)


#discretizing previous into categories: 1,2,3,4,5,6,7 >7 in new variable previous_disc
categories_previous <- cut(data1$previous, breaks = c(-1,0, 1, 2, 3, 4, 5, 6, 7, 275),
                           labels = c("nc", "1c", "2c", "3c", "4c", "5c", "6c", "7c", "8c_plus"),
                           right = TRUE)
data1$previous_disc <- data.frame(data1$previous, categories_previous)$categories_previous
CrossTable(data1$previous_disc, data$y)
str(data1)

#contact
ggplot(data = data, aes(x = contact)) +geom_bar()
contact_corr <- chisq.test(data$contact, data$y, correct=FALSE)
contact_corr
CrossTable(data$contact, data$y)
testing <- cramerV(data$contact, data$y, bias.correct = TRUE)
testing
#since p-value extremely small, contact & y could be associated i.e. contact cld be a useful feature
#28.8% of the records have unknown contact 
#on second thought, suggest keeping the unknown 
#as redistributing 1/3 of the records to the other 2 subcategories may be giving extra information to the model that may cause overfitting
#we can just let the missing values can be treated as a separate category by itself, which alrdy a form of imputation
#https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4


data1 <- subset(data1, select = -c(pdays, previous))
str(data1)

set.seed(123)
split <- sample(1:nrow(data),size = round(0.7 * nrow(data)))

# Create training and testing sets
train <- data1[split, ]
test <- data1[-split, ]

#create upsampling training set
train_up <- upSample(x = train_data %>% select(-y),
                     y = as.factor(train_data$y),
                     yname = "y")
str(data)
#check proportions
print("Train Dataset")
train$y
prop.table(table(train$y))
prop.table(table(train$y))
print("Test Dataset")
prop.table(table(test$y))


#feature selection using boruta algorithm
set.seed(123)
library(Boruta)
boruta <- Boruta(y ~ ., data = train, doTrace = 2, maxRuns = 20)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.8)
attStats(boruta)
Makedecision <- TentativeRoughFix(boruta)  # help us to make quick decision
print(Makedecision)
#all confirmed except 'default'

library(mlbench)
library(caret)
#recursive feature elimination
set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", repeats=2, number=5,verbose=TRUE)
# run the RFE algorithm
results <- rfe(train[,c(1:14,16:17)], train[,15], sizes=c(1:13,15:17), rfeControl=control, scale = TRUE)
# summarize the results
print(results)
#top 5 variables (out of 15): duration, month, housing, day, contact
#top 5 variables after discretisation: duration, month, housing, contact, day
# list the chosen features
predictors(results)
#before discretisation: #all except 'default'
#after discretisation: all

# plot the results
ggplot(data = results , metric = "Accuracy") 
ggplot(data = results, metric = "Kappa")

predictions <- predict(results, test[, results$optVariables])
identical(levels(predictions),levels(test$y))
levels <- levels(predictions[, 1])
levels <- levels[order(levels)]  
confusionMatrix(table(ordered(predictions[,1],levels), ordered(test_data$y, levels)), mode = "everything", positive = 'yes')
################################## MCC #################################################
matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}
#################################### RANDOM FOREST CLASSIFICATION#########################
#Business Model 1
#################################### Model 1 ############################################
rf2_result <- list()

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
save(rf2_result, file = "rf2_result_without_previous.rds")
#average mcc
avg_mcc2 <- mean(sapply(rf2_result, function(res) res$MCC))
avg_mcc2
#average_metrics
average_metrics2 <- colMeans(do.call(rbind, lapply(rf2_result, function(x) x$Metric)))
average_metrics2
# Display the average metrics
print(average_metrics2)
load('rf2_result.rds')
rf2_result
#> avg_mcc2
#[1] 0.5503525
#   Accuracy      Recall Specificity   Precision         AUC 
#0.9011745   0.6470029   0.9348480   0.5682124   0.9309471 
#################################### Model Tune ############################################
#Model 3: data1 (tune)
#manual
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
    train_data1 <- data1[-folds[[i]],]
    test_data1 <- data1[folds[[i]],]
    
    set.seed(123)
    train_oversampled1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
 
    set.seed(123)
    tune <- randomForest(train_oversampled1[,!(names(train_oversampled1) %in% c("default","poutcome","y"))],train_oversampled1$y,mtry = mtry_val[[k]])

    #predict on test data and evaluate, then store 
    prediction <- predict(tune, newdata = test_data1)
    
    cm <- confusionMatrix(prediction, mode = "everything", reference = test_data1$y, positive = "yes")
    #create MCC function and find MCC value, store in m. 
    m <- matthews_correlation_coefficient(cm$table)
    pred_prob <- predict(tune, newdata = test_data1,type = "prob")
    actual <- ifelse(test_data1$y == "yes", 1, 0)
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
  
  avg_mcc <- mean(sapply(rf_tune_m[[k]], function(res) res$MCC))
  avg_mcc
  
  
  rf_tune_m_mcc[[k]] <- avg_mcc
  
}
rf_tune_m
rf_tune_m_mcc
save(rf_tune_m, file = "rf_tune_manual.rds")
load("rf_tune_manual.rds")
average_metrics2 <- colMeans(do.call(rbind, lapply(rf_tune_m[[2]], function(x) x$Metric)))
average_metrics2
# mtry = 4  mtry = 6   mtry = 8
# 0.5408778 0.5322827  0.5243434
########################################################################################
#Business Model 2
#################################### Model 1 ############################################   
rf_result1 <- list()

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
# Display the average metrics
print(average_metrics2)
load('rf_result1.rds')
rf_result1
#avg_mcc2
#0.3513213
#   Accuracy      Recall Specificity   Precision         AUC 
#   0.8698546   0.4100953   0.9307650   0.4395941   0.7821059 
##################################### Model Tune #########################################
             rf_tune_m1 <- vector("list", length = 3)
rf_tune_m_mcc1 <- list()
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
    train_data1 <- data1[-folds[[i]],]
    test_data1 <- data1[folds[[i]],]
    
    set.seed(123)
    train_oversampled1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
    
    tune1 <- randomForest(train_oversampled1[,!(names(train_oversampled1) %in% c("default","poutcome","duration","campaign","y"))],train_oversampled1$y,mtry = mtry_val[[k]])
    
    
    #predict on test data and evaluate, then store 
    prediction <- predict(tune1, newdata = test_data1)
    
    cm <- confusionMatrix(prediction, mode = "everything", reference = test_data1$y, positive = "yes")
    #create MCC function and find MCC value, store in m. 
    m <- matthews_correlation_coefficient(cm$table)
    pred_prob <- predict(tune1, newdata = test_data1,type = "prob")
    actual <- ifelse(test_data1$y == "yes", 1, 0)
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
    rf_tune_m1[[k]][[i]] <- list(confusion = cm$table, MCC = m, Metric = metrics_cal)
  }
  
  avg_mcc <- mean(sapply(rf_tune_m1[[k]], function(res) res$MCC))
  avg_mcc
  
  
  rf_tune_m_mcc1[[k]] <- avg_mcc
  
}
rf_tune_m1
rf_tune_m_mcc1
save(rf_tune_m1, file = "BM2rf_tune_manual1.rds")
load("BM2rf_tune_manual1.rds")
average_metrics2 <- colMeans(do.call(rbind, lapply(rf_tune_m1[[1]], function(x) x$Metric)))
average_metrics2
#mtry = 4  mtry = 6   mtry = 8
#0.3349693 0.3251881  0.3217429                                      
                                                   
#####################################  NAIVE BAYES CLASSFIER  #############################

#Business Model 1

####################### MODEL 1 ##################################

str(data)

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

results1 <- list()

set.seed(123)
folds <- createFolds(data$y, k=5)

library("ROSE")

for(i in 1:5) {
  # Split into training and validation set
  train_data <- data[-folds[[i]],]
  test_data <- data[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  
  train_data <- subset(train_data,select = -c(default,poutcome))
  str(train_data)
  set.seed(123)
  train_data1 <- ovun.sample(y ~ .,data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
  str(train_data)
  
  # laplace_tuning <- list(1,2,3,4,5)
  
  #train model
  nb_model1 <- naiveBayes(y~ ., data = train_data) 
  
  #predict on test data and evaluate, then store 
  nb_prediction1 <- predict(nb_model1, newdata = test_data)   
  cm <- confusionMatrix(nb_prediction1, mode = "everything", reference = test_data$y, positive = "yes")
  cm
  #create MCC function and find MCC value, store in m. 
  m <- matthews_correlation_coefficient(cm$table)
  m
  pred1_prob <- predict(nb_model1, newdata = test_data,type = "raw")
  test_data$y <- ifelse(test_data$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(test_data$y,pred1_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
  metrics_cal
  
  #store confusion matrix and MCC
  results1[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}

results1
avg_mcc1 <- mean(sapply(results1, function(res) res$MCC))
avg_accuracy <- mean(sapply(results1, function(res) res$Metric[,1]))
avg_senstivity <- mean(sapply(results1, function(res) res$Metric[,2]))
avg_specificity <- mean(sapply(results1, function(res) res$Metric[,3]))
avg_precision <- mean(sapply(results1, function(res) res$Metric[,4]))
avg_auc <- mean(sapply(results1, function(res) res$Metric[,5]))
avg_mcc1
avg_accuracy
avg_senstivity
avg_specificity
avg_precision
avg_auc
#0.3936153
#0.4178745 if only discretisize pdays and previous
#0.417902 if only discretisize pdays and previous with laplace =1

# avg_mcc1
# [1] 0.3936153
# > avg_accuracy
# [1] 0.8744993
# > avg_senstivity
# [1] 0.465302
# > avg_specificity
# [1] 0.9287108
# > avg_precision
# [1] 0.4641465
# > avg_auc
# [1] 0.8521787

####################### MODEL 2 ##################################

#data for analysis, data1 to discretize

summary(data$age)
#strange outlier of previous = 275. this means the bank called them 275 times previously! quite implausible
#check top 10 highest values in previous
ggplot(data = data, aes(x = age)) +geom_histogram() + xlim(0,100)
CrossTable(data$age, data$y)
#seems like cdf curve starts flattening after 70, proportion of age seem to stabilize
plot(ecdf(data[,"age"]),
     xlim=c(0,100),
     col="blue")
#yes, proportion of age seem to stabilize after 70
plot(ecdf(data[,"age"]),
     xlim=c(15,70),
     col="blue")
filter(data, age > 55 & age <=65)
filter(data, age > 65 & age <=75) #490 customers
filter(data, age > 75) #611 customers

categories_age <- cut(data1$age, breaks = c(15,30, 45, 60, 75, 100),
                      labels = c("16_30y", "31_45y", "46y_60", "60_75y", "75y_plus"),
                      right = TRUE)
data1$age_disc <- data.frame(data1$age, categories_age)$categories_age
CrossTable(data1$age_disc, data1$y)
str(data1)

#discretize balance
summary(data$balance)
#take a closer look at where most of the values of the records lie to gauge how to bin
ggplot(data = data, aes(x = balance)) +geom_histogram() + xlim(-3000,8000)
#mostly 0 and then having a long right tail 
filter(data, balance < 0) 
filter(data, balance == 0)
filter(data, balance > 0) 
categories_balance <- cut(data1$balance, breaks = c(-9000,-1,0, 102128),
                          labels = c("negative", "zero", "positive"),
                          right = TRUE)
data1$balance_disc <- data.frame(data1$balance, categories_balance)$categories_balance
CrossTable(data1$balance_disc, data1$y)
str(data1)

#discretize duration
summary(data$duration)
ggplot(data = data, aes(x = duration)) +geom_histogram() + xlim(-1,500)
#overly short conversation
filter(data, duration <= 100)
filter(data, duration > 100 & duration <= 200)
filter(data, duration > 200 & duration <= 300)
filter(data, duration > 300 & duration <= 400)
filter(data, duration > 400 & duration <= 500)
filter(data, duration > 500 & duration <= 600)
filter(data, duration > 600)
categories_duration <- cut(data1$duration, breaks = c(-1,100,200,300,400,500,600,5000),
                           labels = c("100s", "101_200s", "201_300s","301_400s","401_500s","501_600s","601s_plus"),
                           right = TRUE)
data1$duration_disc <- data.frame(data1$duration, categories_duration)$categories_duration
CrossTable(data1$duration_disc, data1$y)
str(data1)

#discretize day
data1$day <- as.factor(data1$day)
str(data1)

#discretize campaign
summary(data$campaign)
ggplot(data = data, aes(x = campaign)) +geom_histogram() + xlim(0,20)
filter(data, campaign == 1)
filter(data, campaign == 2)
filter(data,campaign == 3)
filter(data,campaign > 3)
categories_campaign <- cut(data1$campaign, breaks = c(0,1,2,3,65),
                           labels = c("1call", "2call", "3call","4call_plus"),
                           right = TRUE)
data1$campaign_disc <- data.frame(data1$campaign, categories_campaign)$categories_campaign
CrossTable(data1$campaign_disc, data$y)
str(data1)

#discretizing pdays into 5 categories in new variable pdays_disc
categories_pdays <- cut(data1$pdays, breaks = c(-2, -1, 90, 180, 270, 871),
                        labels = c("nc", "1_90d", "91_180d", "181_270d", "271d_plus"),
                        right = TRUE)
data1$pdays_disc <- data.frame(data1$pdays, categories_pdays)$categories_pdays
str(data1)
CrossTable(data1$pdays_disc, data1$y)


#discretizing previous into categories: 1,2,3,4,5,6,7 >7 in new variable previous_disc
categories_previous <- cut(data1$previous, breaks = c(-1,0, 1, 2, 3, 4, 5, 6, 7, 275),
                           labels = c("nc", "1c", "2c", "3c", "4c", "5c", "6c", "7c", "8c_plus"),
                           right = TRUE)
data1$previous_disc <- data.frame(data1$previous, categories_previous)$categories_previous
CrossTable(data1$previous_disc, data1$y)
str(data1)

# Prepare for 5-fold cross-validation
data1 <- subset(data1, select = c(job,marital,education,housing,loan,contact,day,month,default,poutcome,y,pdays_disc,previous_disc,age_disc,balance_disc,duration_disc,campaign_disc))
str(data1)

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

nb_results2 <- list()

set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for(i in 1:5) {
  # Split into training and validation set
  train_data1 <- data1[-folds[[i]],]
  test_data1 <- data1[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  
  train_data1 <- subset(train_data1,select = -c(default,poutcome))
  # test_data1 <- subset(test_data1,select = -c(default,poutcome))
  str(train_data1)
  set.seed(123)
  train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  str(train_data1)
  
  
  #train model
  nb_model2 <- naiveBayes(y~ ., data = train_data1,laplace=1) 
  
  #predict on test data and evaluate, then store 
  nb_prediction2 <- predict(nb_model2, newdata = test_data1)
  
  cm <- confusionMatrix(nb_prediction2, mode = "everything", reference = test_data1$y, positive = "yes")
  cm
  #create MCC function and find MCC value, store in m. 
  m <- matthews_correlation_coefficient(cm$table)
  m
  pred2_prob <- predict(nb_model2, newdata = test_data1,type = "raw")
  test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(test_data1$y,pred2_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
  metrics_cal
  
  #store confusion matrix and MCC
  nb_results2[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}

nb_results2

avg_mcc2 <- mean(sapply(nb_results2, function(res) res$MCC))
avg_mcc2

avg_accuracy2 <- mean(sapply(nb_results2, function(res) res$Metric[,1]))
avg_senstivity2 <- mean(sapply(nb_results2, function(res) res$Metric[,2]))
avg_specificity2 <- mean(sapply(nb_results2, function(res) res$Metric[,3]))
avg_precision2 <- mean(sapply(nb_results2, function(res) res$Metric[,4]))
avg_auc2 <- mean(sapply(nb_results2, function(res) res$Metric[,5]))
avg_mcc2
avg_accuracy2
avg_senstivity2
avg_specificity2
avg_precision2
avg_auc2


# avg_mcc2
# [1] 0.4221034
# > avg_accuracy2
# [1] 0.7920859
# > avg_senstivity2
# [1] 0.7963699
# > avg_specificity2
# [1] 0.7915183
# > avg_precision2
# [1] 0.3360316
# > avg_auc2
# [1] 0.8729072

####################MODEL 3: TUNING USING LAPLACE + removing correlated features ################################

#tuning laplace value based on MCC

nb_results3 <- list()
nb_results3tune <- list()

set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for (k in 1:5){
  
  for(i in 1:5) {
    # Split into training and validation set
    train_data1 <- data1[-folds[[i]],]
    test_data1 <- data1[folds[[i]],]
    
    # Check for class imbalance and use ROSE for oversampling if necessary
    
    train_data1 <- subset(train_data1,select = -c(default,poutcome))
    str(train_data1)
    set.seed(123)
    train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
    str(train_data1)
    
    laplace <- list(1,2,3,4,5)
    
    #train model
    nb_model3 <- naiveBayes(y~ ., data = train_data1, laplace = laplace[[k]]) 
    
    #predict on test data and evaluate, then store 
    nb_prediction3 <- predict(nb_model3, newdata = test_data1)
    
    cm <- confusionMatrix(nb_prediction3, mode = "everything", reference = test_data1$y, positive = "yes")
    cm
    #create MCC function and find MCC value, store in m. 
    m <- matthews_correlation_coefficient(cm$table)
    m
    pred3_prob <- predict(nb_model3, newdata = test_data1,type = "raw")
    test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
    roc_auc <- Metrics::auc(test_data1$y,pred3_prob[,2])
    metrics_cal = rbind(
      c(cm$overall["Accuracy"], 
        cm$byClass["Sensitivity"],
        cm$byClass["Specificity"],
        cm$byClass["Precision"],
        roc_auc))
    colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
    metrics_cal
    
    #store confusion matrix and MCC
    nb_results3[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
  }
  
avg_mcc3 <- mean(sapply(nb_results3, function(res) res$MCC))
avg_mcc3
nb_results3tune[[k]] <- avg_mcc3
}
nb_results3
nb_results3tune

#avg_mcc before tuning: 0.4221034
#avg_mcc after tuning:
# [[1]]
# [1] 0.4221315
# 
# [[2]]
# [1] 0.4220625
# 
# [[3]]
# [1] 0.4221474
# 
# [[4]]
# [1] 0.4219814
# 
# [[5]]
# [1] 0.42201

#optimal laplace value = 3

################# REMOVING CORRELATED FEATURES ########################################


#use laplace = 3, then tune by removing feature previous_disc that has strong association with pdays_disc

#ascertain that cramersV is moderately large for previous_disc and pdays_disc
library('vcd')
assocstats(xtabs(~data1$previous_disc + data1$pdays_disc))
library(rcompanion)
cramerV(data1$previous_disc, data1$pdays_disc, bias.correct = TRUE) #cramersV is 0.5031

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

nb_results3 <- list()

str(data1)
set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for(i in 1:5) {
  # Split into training and validation set
  train_data1 <- data1[-folds[[i]],]
  test_data1 <- data1[folds[[i]],]
  
  
  #
  train_data1 <- subset(train_data1,select = -c(default,poutcome,previous_disc))
  str(train_data1)
  #oversample training data
  set.seed(123)
  train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  str(train_data1)
  
  
  #train model
  nb_model3 <- naiveBayes(y~ ., data = train_data1, laplace=3) 
  
  #predict on test data and evaluate, then store 
  nb_prediction3 <- predict(nb_model3, newdata = test_data1)
  
  cm <- confusionMatrix(nb_prediction3, mode = "everything", reference = test_data1$y, positive = "yes")
  cm
  #create MCC function and find MCC value, store in m. 
  m <- matthews_correlation_coefficient(cm$table)
  m
  pred3_prob <- predict(nb_model3, newdata = test_data1,type = "raw")
  test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(test_data1$y,pred3_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
  metrics_cal
  
  #store confusion matrix and MCC
  nb_results3[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}

nb_results3
avg_mcc3 <- mean(sapply(results3, function(res) res$MCC))
avg_accuracy3 <- mean(sapply(nb_results3, function(res) res$Metric[,1]))
avg_senstivity3 <- mean(sapply(nb_results3, function(res) res$Metric[,2]))
avg_specificity3 <- mean(sapply(nb_results3, function(res) res$Metric[,3]))
avg_precision3 <- mean(sapply(nb_results3, function(res) res$Metric[,4]))
avg_auc3 <- mean(sapply(nb_results3, function(res) res$Metric[,5]))
avg_mcc3
avg_accuracy3
avg_senstivity3
avg_specificity3
avg_precision3
avg_auc3 

#remove previous_disc
# avg_mcc3
# [1] 0.4349558
# > avg_accuracy3
# [1] 0.7946294
# > avg_senstivity3
# [1] 0.8150878
# > avg_specificity3
# [1] 0.7919191
# > avg_precision3
# [1] 0.3416799
# > avg_auc3 
# [1] 0.8796205
#avg_mcc: 0.4349558 if remove previous_disc and laplace = 3

#try removing pdays_disc instead of previous_disc to see which MCC is higher
nb_results3 <- list()

str(data1)
set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for(i in 1:5) {
  # Split into training and validation set
  train_data1 <- data1[-folds[[i]],]
  test_data1 <- data1[folds[[i]],]
  
  
  #
  train_data1 <- subset(train_data1,select = -c(default,poutcome,pdays_disc))
  str(train_data1)
  #oversample training data
  set.seed(123)
  train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  str(train_data1)
  
  
  #train model
  nb_model3 <- naiveBayes(y~ ., data = train_data1, laplace=3) 
  
  #predict on test data and evaluate, then store 
  nb_prediction3 <- predict(nb_model3, newdata = test_data1)
  
  cm <- confusionMatrix(nb_prediction3, mode = "everything", reference = test_data1$y, positive = "yes")
  cm
  #create MCC function and find MCC value, store in m. 
  m <- matthews_correlation_coefficient(cm$table)
  m
  pred3_prob <- predict(nb_model3, newdata = test_data1,type = "raw")
  test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(test_data1$y,pred3_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
  metrics_cal
  
  #store confusion matrix and MCC
  nb_results3[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}

nb_results3
avg_mcc3 <- mean(sapply(results3, function(res) res$MCC))
avg_accuracy3 <- mean(sapply(nb_results3, function(res) res$Metric[,1]))
avg_senstivity3 <- mean(sapply(nb_results3, function(res) res$Metric[,2]))
avg_specificity3 <- mean(sapply(nb_results3, function(res) res$Metric[,3]))
avg_precision3 <- mean(sapply(nb_results3, function(res) res$Metric[,4]))
avg_auc3 <- mean(sapply(nb_results3, function(res) res$Metric[,5]))
avg_mcc3
avg_accuracy3
avg_senstivity3
avg_specificity3
avg_precision3
avg_auc3 

#MCC higher for previous_disc, remove previous_disc

############################# Business Model 2

# Prepare for 5-fold cross-validation
data1 <- subset(data1, select = c(job,marital,education,housing,loan,contact,day,month,default,poutcome,y,pdays_disc,previous_disc,age_disc,balance_disc,duration_disc,campaign_disc))
str(data1)

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

nb_results2 <- list()

set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for(i in 1:5) {
  # Split into training and validation set
  train_data1 <- data1[-folds[[i]],]
  test_data1 <- data1[folds[[i]],]
  
  # Check for class imbalance and use ROSE for oversampling if necessary
  
  train_data1 <- subset(train_data1,select = -c(default,poutcome,duration_disc,campaign_disc))
  # test_data1 <- subset(test_data1,select = -c(default,poutcome))
  str(train_data1)
  set.seed(123)
  train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  str(train_data1)
  
  
  #train model
  nb_model2 <- naiveBayes(y~ ., data = train_data1) 
  
  #predict on test data and evaluate, then store 
  nb_prediction2 <- predict(nb_model2, newdata = test_data1)
  
  cm <- confusionMatrix(nb_prediction2, mode = "everything", reference = test_data1$y, positive = "yes")
  cm
  #create MCC function and find MCC value, store in m. 
  m <- matthews_correlation_coefficient(cm$table)
  m
  pred2_prob <- predict(nb_model2, newdata = test_data1,type = "raw")
  test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(test_data1$y,pred2_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
  metrics_cal
  
  #store confusion matrix and MCC
  nb_results2[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}

nb_results2

avg_mcc2 <- mean(sapply(nb_results2, function(res) res$MCC))
avg_mcc2

avg_accuracy2 <- mean(sapply(nb_results2, function(res) res$Metric[,1]))
avg_senstivity2 <- mean(sapply(nb_results2, function(res) res$Metric[,2]))
avg_specificity2 <- mean(sapply(nb_results2, function(res) res$Metric[,3]))
avg_precision2 <- mean(sapply(nb_results2, function(res) res$Metric[,4]))
avg_auc2 <- mean(sapply(nb_results2, function(res) res$Metric[,5]))
avg_mcc2
avg_accuracy2
avg_senstivity2
avg_specificity2
avg_precision2
avg_auc2

# avg_mcc2
# [1] 0.2616939
# > avg_accuracy2
# [1] 0.7175245
# > avg_senstivity2
# [1] 0.6532427
# > avg_specificity2
# [1] 0.7260409
# > avg_precision2
# [1] 0.2400552
# > avg_auc2
# [1] 0.7497377

####################MODEL 3: TUNING USING LAPLACE + removing correlated features ################################

#tuning laplace value based on MCC

nb_results3 <- list()
nb_results3tune <- list()

set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for (k in 1:5){
  
  for(i in 1:5) {
    # Split into training and validation set
    train_data1 <- data1[-folds[[i]],]
    test_data1 <- data1[folds[[i]],]
    
    # Check for class imbalance and use ROSE for oversampling if necessary
    
    train_data1 <- subset(train_data1,select = -c(default,poutcome,duration_disc,campaign_disc))
    str(train_data1)
    set.seed(123)
    train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
    str(train_data1)
    
    laplace <- list(1,2,3,4,5)
    
    #train model
    nb_model3 <- naiveBayes(y~ ., data = train_data1, laplace = laplace[[k]]) 
    
    #predict on test data and evaluate, then store 
    nb_prediction3 <- predict(nb_model3, newdata = test_data1)
    
    cm <- confusionMatrix(nb_prediction3, mode = "everything", reference = test_data1$y, positive = "yes")
    cm
    #create MCC function and find MCC value, store in m. 
    m <- matthews_correlation_coefficient(cm$table)
    m
    pred3_prob <- predict(nb_model3, newdata = test_data1,type = "raw")
    test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
    roc_auc <- Metrics::auc(test_data1$y,pred3_prob[,2])
    metrics_cal = rbind(
      c(cm$overall["Accuracy"], 
        cm$byClass["Sensitivity"],
        cm$byClass["Specificity"],
        cm$byClass["Precision"],
        roc_auc))
    colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
    metrics_cal
    
    #store confusion matrix and MCC
    nb_results3[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
  }
  avg_mcc3 <- mean(sapply(nb_results3, function(res) res$MCC))
  avg_mcc3
  nb_results3tune[[k]] <- avg_mcc3
}
nb_results3
nb_results3tune

#avg_mcc before tuning: 0.4221034
#avg_mcc after tuning:
# [[1]]
# [1] 0.2617164
# 
# [[2]]
# [1] 0.2618271
# 
# [[3]]
# [1] 0.2619164
# 
# [[4]]
# [1] 0.2619834
# 
# [[5]]
# [1] 0.2619899

#optimal laplace value = 5

################# REMOVING CORRELATED FEATURES ########################################


#use laplace = 5, then tune by removing feature previous_disc as per business model 1

matthews_correlation_coefficient <- function(cm) {
  tp <- as.numeric(cm[2, 2])
  tn <- as.numeric(cm[1, 1])
  fp <- as.numeric(cm[2, 1])
  fn <- as.numeric(cm[1, 2])
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

nb_results3 <- list()

str(data1)
set.seed(123)
folds <- createFolds(data1$y, k=5)

library("ROSE")

for(i in 1:5) {
  # Split into training and validation set
  train_data1 <- data1[-folds[[i]],]
  test_data1 <- data1[folds[[i]],]
  
  
  #
  train_data1 <- subset(train_data1,select = -c(default,poutcome,duration_disc,campaign_disc,previous_disc))
  str(train_data1)
  #oversample training data
  set.seed(123)
  train_data1 <- ovun.sample(y ~ .,data = train_data1, method = "over", N = 2*sum(train_data1$y == "no"))$data
  str(train_data1)
  
  
  #train model
  nb_model3 <- naiveBayes(y~ ., data = train_data1, laplace=5) 
  
  #predict on test data and evaluate, then store 
  nb_prediction3 <- predict(nb_model3, newdata = test_data1)
  
  cm <- confusionMatrix(nb_prediction3, mode = "everything", reference = test_data1$y, positive = "yes")
  cm
  #create MCC function and find MCC value, store in m. 
  m <- matthews_correlation_coefficient(cm$table)
  m
  pred3_prob <- predict(nb_model3, newdata = test_data1,type = "raw")
  test_data1$y <- ifelse(test_data1$y == "yes", 1, 0)
  roc_auc <- Metrics::auc(test_data1$y,pred3_prob[,2])
  metrics_cal = rbind(
    c(cm$overall["Accuracy"], 
      cm$byClass["Sensitivity"],
      cm$byClass["Specificity"],
      cm$byClass["Precision"],
      roc_auc))
  colnames(metrics_cal) = c("Accuracy", "Sensitivity", "Specificity","Precision","AUC")
  metrics_cal
  
  #store confusion matrix and MCC
  nb_results3[[i]] <- list(confusion = cm$table,MCC = m,Metric = metrics_cal)
}

nb_results3
avg_mcc3 <- mean(sapply(nb_results3, function(res) res$MCC))
avg_accuracy3 <- mean(sapply(nb_results3, function(res) res$Metric[,1]))
avg_senstivity3 <- mean(sapply(nb_results3, function(res) res$Metric[,2]))
avg_specificity3 <- mean(sapply(nb_results3, function(res) res$Metric[,3]))
avg_precision3 <- mean(sapply(nb_results3, function(res) res$Metric[,4]))
avg_auc3 <- mean(sapply(nb_results3, function(res) res$Metric[,5]))
avg_mcc3
avg_accuracy3
avg_senstivity3
avg_specificity3
avg_precision3
avg_auc3 
