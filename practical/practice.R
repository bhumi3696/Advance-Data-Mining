setwd("D:/SEM 2/ADM/practical/1") #change this to where you downloaded the .csv 
data<-airquality
summary(data)
############################################Question 1######################
#F1
#remove NA
#check null value
sapply(data,function(x) sum(is.na(x)))
#check index of NA value
which(!complete.cases(data))
#replace Na value with mean 
summary(data)
data$Ozone[is.na(data$Ozone)] = 42
#replace special character
a <- gsub("+$","",Google_playstore_data$Installs)
#remove NA value 
x <- na.omit(data)
data_1 <- data[complete.cases(data),] 

#outliers
boxplot(subset(my_dataset, select=c(1,6,18,20,21:24))) #nothing too obvious here
boxplot(subset(my_dataset, select=c(26:29))) #May be something in years at company
boxplot(my_dataset$DailyRate)
boxplot(my_dataset$HourlyRate)
boxplot(my_dataset$MonthlyIncome) #possibly some here too
boxplot(my_dataset$MonthlyRate)

#F2
my_dataset$Attrition <- factor(my_dataset$Attrition, labels=c(0,1), levels=c("No", "Yes"))
table(my_dataset$Attrition)

#F3
# Data Partition(train and test)
library(caret)
sample <- createDataPartition(my_dataset$Attrition, p = .75, list = FALSE) 
train <- my_dataset[sample, ]
test <- my_dataset[-sample, ]

#F4: benchmarks
#1 everyone stays:
set.se
str(test$Attrition) # remain (Y) is 0
b1 <- rep(0, dim(test)[1])
(accuracyB1 <- 1 - mean(b1 != test$Attrition))


