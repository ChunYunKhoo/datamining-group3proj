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

#import data
data <- read.csv("C:/Users/chuny/Documents/GitHub/datamining-group3proj/bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
str(data)
str('testing commit')

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

#check duration with y since when duration = 0, likely y = no.
filter(data, duration == 0)
#indeed the case, but it is only 3 records so can keep duration

#campaigns, duration, y
ggplot(data = data, aes(campaign, duration, colour = y)) + geom_point()
#number of people subscribing is higher when number of contacts performed are low and duration of call is longer (>5 min)

#previous,y
p_y <- data %>%group_by(y) %>% summarise(mean(previous)) 
p_y
#higher mean number of contacts performed for those who subscribed

#previous, poutcome and y
ggplot(data = data, aes(previous, poutcome, colour = y)) + geom_point()
#clients who were previously contacted with failure also tend to not subscribe to term deposit.

########################DATA PREPROCESSING#########################################


#pairplot for numerical variables to check for multicollinearity between numerical variables
num_var <- data[,c("age","balance","duration", "campaign", "pdays", "previous")]
corrplot(cor(num_var), method = "number")
#pdays and previous have a corr = 0.45, but still not that high that it is concerning


#do chi-squared test to check for multicollinearity between categorical variables
#shows warning: In chisq.test(tbl) : Chi-squared approximation may be incorrect
install.packages("Hmisc")
library("Hmisc")
categorical_columns <- names(data)[sapply(data, is.factor)]
# Initialize a matrix to store p-values
p_value_matrix <- matrix(NA, nrow=length(categorical_columns), ncol=length(categorical_columns),
                         dimnames=list(categorical_columns, categorical_columns))

# Loop through all combinations of categorical columns
for (i in 1:(length(categorical_columns) - 1)) {
  for (j in (i + 1):length(categorical_columns)) {
    # Extract column names
    col1 <- categorical_columns[i]
    col2 <- categorical_columns[j]
    
    # Create contingency table
    tbl <- table(data[, col1], data[, col2])
    
    # Conduct chi-squared test if all expected counts are > 5 
    if (all(chisq.test(tbl)$expected > 5)) {
      test <- chisq.test(tbl)
      p_value_matrix[i, j] <- test$p.value
    }
  }
}
p_value_matrix

chisq.test(data$education, data$job, correct=FALSE)
chisq.test(data$loan, data$y, correct=FALSE)
#none of categorical variables are correlated as p-values are extremely small

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
boruta <- Boruta(y ~ ., data = data, doTrace = 2, maxRuns = 20)
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
control <- rfeControl(functions=rfFuncs, method="cv", repeats=2, number=3,verbose=TRUE)
# run the RFE algorithm
results <- rfe(train_data[,1:16], train_data[,17], sizes=c(1:16), rfeControl=control)
# summarize the results
print(results)
#top 5 variables (out of 15): duration, month, housing, day, contact
# list the chosen features
predictors(results)

# plot the results
ggplot(data = results , metric = "Accuracy") 
ggplot(data = results, metric = "Kappa")
#all confirmed except 'default'

predictions <- predict(results, test_data[, results$optVariables])
identical(levels(predictions),levels(test_data$y))
levels <- levels(predictions[, 1])
levels <- levels[order(levels)]  
confusionMatrix(table(ordered(predictions[,1],levels), ordered(test_data$y, levels)), mode = "everything", positive = 'yes')
#precision: 0.64722, recall: 0.45338

#use all variables except 'default'