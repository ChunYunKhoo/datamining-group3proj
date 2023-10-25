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

data1 <- subset(data, select = -c(pdays, previous))
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
