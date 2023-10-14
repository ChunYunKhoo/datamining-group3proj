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

#age vs y
ggplot(data, aes(x_column)) + 
  geom_histogram(bins = 15)

#breakdown of y across education levels 
ggplot(data = data, aes(x=y, fill=education)) +
  geom_bar() +
  xlab(" Education level") + ylab("no. of clients")
#clients with secondary and tertiary education contribute to bulk of subscription
  
# prop. of y across education levels 
ggplot(data = data, aes(x=education, fill= y)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", 
              aes(label = stat(count)),
              position=position_fill(vjust=0.5), colour="black")
  xlab(" Education level") + ylab("no. of clients")
#clients w tertiary education have highest proportion of subscribing to term deposit
#but on an absolute level, highest no. of clients who subscribe have sec lvl of education.

#breakdown of y across job type
ggplot(data = data, aes(x=y, fill=job)) +
  geom_bar(position = "fill") +
xlab("term deposit subsription") + ylab("no. of clients")
guides(fill=guide_legend(title="Has the client subscribed to Term Deposit?"))
#clients with management jobs contribute to the highest proportion of yes
#clients with blue-collar jobs contribute to the highest proportion of no

#age distribution for each class value of y
# create mean by class
mean <- data %>% group_by(y)%>%summarise(mean_val=mean(age))
mean
#those who say yes and no have similar mean

#histogram
ggplot(data, aes(x=age)) + 
  geom_histogram(binwidth = 5) + 
  facet_grid(vars(y)) + 
  scale_x_continuous(breaks =seq(0,100,5)) + 
  geom_vline(mean, mapping = aes(xintercept=mean_val),color="blue")

#mean age for both target class values are similar.
#no: mode age range is between 32.5 to 35.5
#yes:mode age range id between 27.5 to 37.5
#boxplot
ggplot(data = data, aes(x=y, y=age, fill = y)) +
  geom_boxplot(alpha = 0.5) +
  guides(fill=guide_legend(title="Has the client subscribed to Term Deposit?"))
#yes and no roughly have similar age range,corresponds to age range of clients


#marital status, y
ggplot(data = data, aes(x=marital, fill=y)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="black")
  xlab("term deposit subscription") + ylab("no. of clients")
#singles have highest proportion of subscribing.


#check duration with y since when duration = 0, likely y = no.
num_y <- as.numeric(data$y)
cor.test(data$duration, num_y) #some weak-moderate correlation at 0.39
filter(data, duration == 0 & y == 'no')
#even if the correlation is significant, it is occuring in 3 out of 45k+ records, can reasonably keep duration
  
#duration, y
m <- data %>% group_by(y)%>% summarise(m_d=mean(duration))
m
summary(data$duration)
#the mean call duration is longer for those who subscribed
ggplot(data, aes(x=duration)) + 
  geom_histogram(bins = 300) + 
  facet_grid(vars(y)) +
  scale_x_continuous(breaks =seq(0,5000,300), limits = c(0,4918))
#mean call duration for those who subscribed is around 5 mins. 
#check cdf to see where the proportion stabilize
plot(ecdf(data[,"duration"]),
     xlim=c(0,6000),
     col="blue")


#campaigns, duration, y
ggplot(data = data, aes(campaign, duration, colour = y)) + geom_point()
#number of people subscribing is higher when number of contacts performed are low and duration of call is longer (>5 min)

#previous,y
summary(data$previous)
#strange outlier of previous = 275. this means the bank called them 275 times previously! quite implausible
#check top 10 highest values in previous
top10_previous <- data %>%                                     
  arrange(desc(data$previous)) %>% 
  slice(1:10)
top10_previous #top 1 is 275, & the rest are between 30-50+.
p_y <- data %>%group_by(y) %>% summarise(mean(previous)) 
p_y #higher mean number of contacts performed for those who subscribed, could be skewed by outlier?
ggplot(data = data, aes(x = previous)) +geom_histogram() + xlim(-2,10)
CrossTable(data$previous, data$y, prop.c = F, prop.t = F, prop.chisq = F)
#the no. of records for each value beyond previous >9 is <100,quite small
#possible to group records above a certain value into 1 group.
#seems like cdf curve starts flattening after 10, proportion of previous seem to stabilize
plot(ecdf(data[,"previous"]),
     xlim=c(0,50),
     col="blue")
#to take a closer look, limit x to 20
plot(ecdf(data[,"previous"]),
     xlim=c(0,10),
     col="blue")
#can group all those > 7 as one group


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

#pairplot for numerical variables to check for multicollinearity between numerical variables
num_var <- data[,c("age","balance","duration", "campaign", "pdays", "previous")]
corrplot(cor(num_var), method = "number")
#pdays and previous have a corr = 0.45, but still not that high that it is concerning

str(data)
#job, marital, education, default, housing, loan, contact, month, poutcome,y are categorical variables
#do chi-squared test to check for multicollinearity between categorical variables


# #shows warning: In chisq.test(tbl) : Chi-squared approximation may be incorrect
# install.packages("Hmisc")
# library("Hmisc")
# categorical_columns <- names(data)[sapply(data, is.factor)]
# # Initialize a matrix to store p-values
# p_value_matrix <- matrix(NA, nrow=length(categorical_columns), ncol=length(categorical_columns),
#                          dimnames=list(categorical_columns, categorical_columns))
# 
# # Loop through all combinations of categorical columns
# for (i in 1:(length(categorical_columns) - 1)) {
#   for (j in (i + 1):length(categorical_columns)) {
#     # Extract column names
#     col1 <- categorical_columns[i]
#     col2 <- categorical_columns[j]
#     
#     # Create contingency table
#     tbl <- table(data[, col1], data[, col2])
#     
#     # Conduct chi-squared test if all expected counts are > 5 
#     if (all(chisq.test(tbl)$expected > 5)) {
#       test <- chisq.test(tbl)
#       p_value_matrix[i, j] <- test$p.value
#     }
#   }
# }
# p_value_matrix
# 
# chisq.test(data$education, data$job, correct=FALSE)
# chisq.test(data$loan, data$y, correct=FALSE)
# #all of categorical variables are correlated as p-values are extremely small

str(data)
#discretizing pdays
#zoom into range where most positive values lie in order to gauge how to bin
ggplot(data = data, aes(x = pdays)) +geom_histogram(binwidth = 5) + xlim(0,500) 
filter(data, pdays == -1) #36954 customers
filter(data, pdays > -1 & pdays <= 90) #718 customers were contacted previously within 3 months back
filter(data, pdays > 90 & pdays <= 180) #2480 customers were contacted 3-6 months ago
filter(data, pdays > 180 & pdays <= 270) #2082 customers in 6-9 months ago
filter(data, pdays > 270) #2977 customers called more than 9 months ago
#discretizing previous into 5 categories in new variable pdays_disc
categories_pdays <- cut(data$pdays, breaks = c(-2, -1, 90, 180, 270, 871),
                  labels = c("nc", "1_90d", "91_180d", "181_270d", "271d_plus"),
                  right = TRUE)
data$pdays_disc <- data.frame(data$pdays, categories_pdays)$categories_pdays
str(data)
CrossTable(data$pdays_disc, data$y)


#discretizing previous into categories: 1,2,3,4,5,6,7 >7 in new variable previous_disc
categories_previous <- cut(data$previous, breaks = c(-1,0, 1, 2, 3, 4, 5, 6, 7, 275),
                        labels = c("nc", "1c", "2c", "3c", "4c", "5c", "6c", "7c", "8c_plus"),
                        right = TRUE)
data$previous_disc <- data.frame(data$previous, categories_previous)$categories_previous
CrossTable(data$previous_disc, data$y)
str(data)

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

#contact
#come up with 3 versions of of values to fill in missing data in column 2:7
summary(data[,9])
UnknownPercentage <- function(datax) {sum(datax == "unknown")/length(datax)*100} 
apply(data, 2, UnknownPercentage)
#*100 makes the value as percentage. Note the percentage is not big
data$contact_disc <- na_if(data$contact, "unknown")
str(data)
MissingPercentage <- function(datax) {sum(is.na(datax))/length(datax)*100} 
apply(data, 2, MissingPercentage)
library(mice)
impute <- mice(data[,c("job", "marital","education","default","loan","month","contact_disc")], m=3, method= "polyreg", seed = 123)
#from the results, it has repeated 5 iterations. for each iteration, it has repeated 3 times. Imp = 3.
#impute missing values using the imputation model
newDATA <- complete(impute, 1)
print(newDATA)
#add variable into dataframe
data$contact_disc <- newDATA$contact_disc
str(data)
CrossTable(data$contact_disc, data$y)

data <- subset(data, select = -c(pdays, previous, contact))
str(data)

set.seed(123)
split <- sample(1:nrow(data),size = round(0.7 * nrow(data)))

# Create training and testing sets
train_data <- data[split, ]
test_data <- data[-split, ]

#create upsampling training set
train_up <- upSample(x = train_data %>% select(-y),
                     y = as.factor(train_data$y),
                     yname = "y")
str(data)
#check proportions
print("Train Dataset")
prop.table(table(train_data$y))
prop.table(table(train_up$y))
print("Test Dataset")
prop.table(table(test_data$y))


#feature selection using boruta algorithm
set.seed(123)
library(Boruta)
boruta <- Boruta(y ~ ., data = train_data, doTrace = 2, maxRuns = 20)
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
results <- rfe(train_data[,c(1:13,15:17)], train_data[,14], sizes=c(6:13,15:17), rfeControl=control)
# summarize the results
print(results)
#top 5 variables (out of 15): duration, month, housing, day, contact
#top 5 variables after discretisation: duration, poutcome, month, housing, age
# list the chosen features
predictors(results)
#before discretisation: #all except 'default'
#after discretisation: "duration" "month"    "housing"  "day"      "poutcome" "age" based on accuracy
#but if based on highest kappa its actlly keeping the first 10 variables

# plot the results
ggplot(data = results , metric = "Accuracy") 
ggplot(data = results, metric = "Kappa")

predictions <- predict(results, test_data[, results$optVariables])
identical(levels(predictions),levels(test_data$y))
levels <- levels(predictions[, 1])
levels <- levels[order(levels)]  
confusionMatrix(table(ordered(predictions[,1],levels), ordered(test_data$y, levels)), mode = "everything", positive = 'yes')
#precision: 0.64722, recall: 0.45338
#after discretisation: precision: 0.6412, recall: 0.45275

# set.seed(123)
# data_rf <- ranger(, data = train)
# credit_ranger
# set.seed(123)
# credit_prediction_ranger <- predict(credit_ranger, test)
# print(credit_ranger)
# confusionMatrix(credit_prediction_ranger$predictions, test$default)
# toc()


#####################################NAIVE BAYES CLASSFIER#############################

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

categories_age <- cut(data$age, breaks = c(15,30, 45, 60, 75, 100),
                           labels = c("16_30y", "31_45y", "46y_60", "60_75y", "75y_plus"),
                           right = TRUE)
data$age_disc <- data.frame(data$age, categories_age)$categories_age
CrossTable(data$age_disc, data$y)
str(data)

#discretize balance
summary(data$balance)
#take a closer look at where most of the values of the records lie to gauge how to bin
ggplot(data = data, aes(x = balance)) +geom_histogram() + xlim(-3000,8000)
#mostly 0 and then having a long right tail 
filter(data, balance < 0) 
filter(data, balance == 0)
filter(data, balance > 0) 
categories_balance <- cut(data$balance, breaks = c(-9000,-1,0, 102128),
                      labels = c("negative", "zero", "positive"),
                      right = TRUE)
data$balance_disc <- data.frame(data$balance, categories_balance)$categories_balance
CrossTable(data$balance_disc, data$y)
str(data)

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
categories_duration <- cut(data$duration, breaks = c(-1,100,200,300,400,500,600,5000),
                          labels = c("100s", "101_200s", "201_300s","301_400s","401_500s","501_600s","601s_plus"),
                          right = TRUE)
data$duration_disc <- data.frame(data$duration, categories_duration)$categories_duration
CrossTable(data$duration_disc, data$y)
str(data)

#discretize day
data$day <- as.factor(data$day)
str(data)

#discretize campaign
summary(data$campaign)
ggplot(data = data, aes(x = campaign)) +geom_histogram() + xlim(0,20)
filter(data, campaign == 1)
filter(data, campaign == 2)
filter(data,campaign == 3)
filter(data,campaign > 3)
categories_campaign <- cut(data$campaign, breaks = c(0,1,2,3,65),
                           labels = c("1call", "2call", "3call","4call_plus"),
                           right = TRUE)
data$campaign_disc <- data.frame(data$campaign, categories_campaign)$categories_campaign
CrossTable(data$campaign_disc, data$y)
str(data)

# Prepare for 5-fold cross-validation
data <- subset(data, select = c(job,marital,education,housing,loan,day,month,poutcome,y,pdays_disc,previous_disc,contact_disc,age_disc,balance_disc,duration_disc,campaign_disc))
str(data)

set.seed(12345)
folds <- createFolds(data$y, k=5)

for(i in 1:5) {
  # Split into training and validation set
  train_data <- data[-folds[[i]],]
  valid_data <- data[folds[[i]],]
}

install.packages("ROSE")
library("ROSE")
# Check for class imbalance and use ROSE for oversampling if necessary
if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
train_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
}
summary(train_data)

#train model
base_model_nb  <- naiveBayes(y~ ., data = train_data) 
base_model_nb 
summary(base_model_nb)

#predict using valid data
y_pred <- predict(base_model_nb, valid_data)   

conf_mat <- table(valid_data$y,y_pred)
print(conf_mat)
confusionMatrix(conf_mat, mode = "everything", positive = "yes")

#manual calculation MCC = 0.423
MCC = (TP*TN – FP*FN) / √(TP+FP)(TP+FN)(TN+FP)(TN+FN)
MCC_manual (791*6541-1443*266) / sqrt((791+1443)*(791+266)*(6541+1443)*(6541+266))
MCC_manual

install.packages("mltools")
library(mltools)

#Matthews correlation coefficient for confusion matrix = 0.423
conf_matrix <- matrix(c(791, 266, 1443, 6541), nrow=2)
#view confusion matrix
conf_matrix
mcc(confusionM = conf_matrix)

###############model without poutcome##############################
data <- subset(data, select = c(job,marital,education,housing,loan,day,month,y,pdays_disc,previous_disc,contact_disc,age_disc,balance_disc,duration_disc,campaign_disc))
str(data)

set.seed(12345)
folds <- createFolds(data_1$y, k=5)

for(i in 1:5) {
  # Split into training and validation set
  train_data <- data[-folds[[i]],]
  valid_data <- data[folds[[i]],]
}

install.packages("ROSE")
library("ROSE")
# Check for class imbalance and use ROSE for oversampling if necessary
if(sum(train_data$y == "yes") < sum(train_data$y == "no")) {
  train_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = 2*sum(train_data$y == "no"))$data
}
summary(train_data)

str(train_data)
x <- train_data[,-8]
str(x)
y <- train_data$y
str(y)

#train model
library(klaR)
library(e1071)
# base_model_nb_f <- train(x,y,'nb',trControl=trainControl(method='cv',number=5)) ##doesnt work
library(caret)
# base_model_nb_f  <- train(y ~ ., data = train_data, method = "nb")
#train_control <- trainControl(method="cv", number=5) ##doesnt work

install.packages("Rfast2")
# library(Rfast2)
# nb.cv(x, y, type = "laplace", folds = NULL, nfolds = 5, 
#       stratified = TRUE, seed = TRUE, pred.ret = TRUE)

base_model_nb_f  <- naiveBayes(y~ ., data = train_data) 
base_model_nb_f
summary(base_model_nb_f)

#predict using valid data
y_pred_f <- predict(base_model_nb_f, valid_data)   

conf_mat <- table(valid_data$y,y_pred_f)
print(conf_mat)
confusionMatrix(conf_mat, mode = "everything", positive = "yes")

#manual calculation
MCC = (TP*TN – FP*FN) / √(TP+FP)(TP+FN)(TN+FP)(TN+FN)
MCC_manual <- (832*6439-1545*225)/sqrt((832+1545)*(832+225)*(6439+1545)*(6439+225))
MCC_manual

install.packages("mltools")
library(mltools)

#Matthews correlation coefficient for confusion matrix
conf_matrix <- matrix(c(832, 225, 1545, 6439), nrow=2)
#view confusion matrix
conf_matrix
mcc(confusionM = conf_matrix) #MCC = 0.433
