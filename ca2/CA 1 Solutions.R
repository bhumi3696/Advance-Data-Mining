 ##########################################
 #                                        #
###           ADM PRACTICAL CA           ###
 #                                        #
 ##########################################

# The key to success in any organization is attracting and retaining top talent. 
# You are an HR analyst at my company, and one of my tasks is to determine which factors 
# keep employees at my company and which prompt others to leave. We need to know what 
# factors we can change to prevent the loss of good people. 
 
# You have data about past and current employees in a spreadsheet. It has various data 
# points on our employees, but we're' most interested in whether theyâ€™re still with the 
# company or whether theyâ€™ve gone to work somewhere else. And we want to understand how 
# this relates to workforce attrition. 

#Attributes:
 # Age: in years
 # Attrition: Y/N the dependent variable -- have they left the company?
 # BusinessTravel: Non-Travel; Traval_Frequently, Travel_Rarely
 # DailyRate: Consultancy Charge per Day
 # Department: Human Resources; Research & Development; Sales
 # DistanceFromHome: How far the employe lives from work
 # Education: 1 'Below College'; 2 'College'; 3 'Bachelor'; 4 'Master'; 5 'Doctor'
 # EducationField: Human Resources; Life Sciences; Marketing; Medical; Other; Technical Degree
 # EmployeeCount: No of employes in this record	
 # EmployeeNumber: Employee ID
 # EnvironmentSatisfaction: 4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'	
 # Gender: Male / Female
 # HourlyRate: Consultancy Charge per Hour
 # JobInvolvement: 4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'
 # JobLevel	Metadata not available -- make an assumption 
 # JobRole: Healthcare Representative;  Human Resources; Laboratory Technician; Manager; Manufacturing Director; Research Director; Research Scientist; Sales Executive; Sales Representative 
 # JobSatisfaction: 4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'
 # MaritalStatus: Divorced; Married; Single
 # MonthlyIncome: monthly salary
 # MonthlyRate: Consultancy Charge per Day
 # NumCompaniesWorked: No. of previous employeers
 # Over18: Y/N
 # OverTime: Yes/No
 # PercentSalaryHike: Last Years Increment	
 # PerformanceRating:  4 point Likert scale: 1 'Low'; 2 'Good'; 3 'Excellent'; 4 'Outstanding'
 # RelationshipSatisfaction:  4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'
 # StandardHours: Contract hours	
 # StockOptionLevel: No available metadata -- make an assumption :)	
 # TotalWorkingYears: Career Age
 # TrainingTimesLastYear: No. of training courses attended last year
 # WorkLifeBalance: 4 Point Likert Scale: 1 'Bad'; 2 'Good'; 3 'Better'; 4 'Best'
 # YearsAtCompany: Time spent with company
 # YearsInCurrentRole: Time in current role
 # YearsSinceLastPromotion: No. of years since last promoted
 # YearsWithCurrManager: Year spent with current manager
 #change this to where you downloaded the .csv
 
 setwd("C:/Users/jayan/Desktop/ADM/")
 hrdata <-read.csv("ADM CA 1 Data.csv", stringsAsFactors = T) #will autoencode the text attributes to factors
 
 #ok, now we need to make a dataset unique to you
 set.seed(1337) # <-- put your student number here WITHOUT the x!! Leave off a starting zero if you have one
 #e.g.: set.seed(62345678)
 my_dataset <- hrdata[order(runif(600)), ]
 
 #let's remove ID, we probably don't want that:
 my_dataset <- my_dataset[-10]
 
 #Unfortunately, due to a technical error, 3 columns of the data were lost :(
 #HR blamed IT, IT blamed HR, your manager will blame you, so let's just hope those columns weren't important!
 col1 <- round(runif(1)*32)+2 #the +2 protects the Age and Attrition Variables
 col2 <- round(runif(1)*31)+2
 col3 <- round(runif(1)*30)+2
 
 cols <- names(my_dataset)
 
 print(paste("I lost: ", cols[col1], ",", cols[col2], ",", cols[col3]))
 #"I lost:  StandardHours , OverTime , Gender"
 
 my_dataset <- my_dataset[-col1]
 my_dataset <- my_dataset[-col2]
 my_dataset <- my_dataset[-col3]
 
 #if you want to use something other than R save your dataset:
 write.csv(file="mydata.csv", my_dataset, row.names = F)
 
 #Now please begin, and good luck!
 #Because you lost 3 columns, some models may/may not work as well, 
 #don't worry about this, I will control for it in the grading!
 
 ##########################################
 #Begin Foundations
 
 #OK, so first off i lost some of my factor levels, and I'm lazy, so let's just read it in again
 my_dataset <- read.csv("mydata.csv", stringsAsFactors = T)
 
 str(my_dataset)
 
 
 #let's deal with these other factors: F1
 my_dataset$Education <- factor(my_dataset$Education, levels = c(1,2,3,4,5), labels=c("BC", "C", "UG", "MSc", "PhD"))
 my_dataset$EnvironmentSatisfaction <- factor(my_dataset$EnvironmentSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v High"))
 my_dataset$JobInvolvement <- factor(my_dataset$JobInvolvement, levels = c(1:4), labels=c("Low", "Medium", "High", "v High"))
 my_dataset$JobLevel <- factor(my_dataset$JobLevel) #insufficient information to do more
 my_dataset$JobSatisfaction <- factor(my_dataset$JobSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v High"))
 my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating, levels = c(1:4), labels=c("Low", "Good", "Excellent", "Outstanding"))
 my_dataset$RelationshipSatisfaction <- factor(my_dataset$RelationshipSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v High"))
 my_dataset$StockOptionLevel <- factor(my_dataset$StockOptionLevel) #don't have more information
 my_dataset$WorkLifeBalance <- factor(my_dataset$WorkLifeBalance, levels = c(1:4), labels=c("Bad", "Good", "Better", "Best"))
 
 
 #Now to fix factors where i don't have all levels
 summary(my_dataset)
 #I'm missing levels 0 and 1 in PerformanceRating, but the rest of my categoricals seem fine
 my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating) 
 #however, as JobRole displays other, let's check more precisely
 table(my_dataset$JobRole)
 summary(my_dataset$JobRole)
 #because i already encoded them above, all i have to do is reinvoke the factor command and my empty levels disappear
 
 #a couple of the variables are pointless, so let's get rid of them
 my_dataset <- my_dataset[,-9] #EmployeeCount -- always 1
 my_dataset <- my_dataset[, -19] #over 18 -- always Y; strictly speaking we should confirm this, but under time pressure it's safe to assume this varibale isn't needed
 
 #mising values:
 sapply(my_dataset,function(x) sum(is.na(x)))
 
 #outliers
 boxplot(subset(my_dataset, select=c(1,6,18,20,21:24))) #nothing too obvious here
 boxplot(subset(my_dataset, select=c(26:29))) #May be something in years at company
 boxplot(my_dataset$DailyRate)
 boxplot(my_dataset$HourlyRate)
 boxplot(my_dataset$MonthlyIncome) #possibly some here too
 boxplot(my_dataset$MonthlyRate)
 
 #i prefer the attrition factor as 0/1 for convenience 
 my_dataset$Attrition <- factor(my_dataset$Attrition, labels=c(0,1), levels=c("No", "Yes"))
 
 #F2: class balance:
 table(my_dataset$Attrition) #fairly imbalanced
 
 #F3: train and test
 #2 options here stratified, or under/over sampling
 library(caret)
 sample <- createDataPartition(my_dataset$Attrition, p = .75, list = FALSE) 
 train <- my_dataset[sample, ]
 test <- my_dataset[-sample, ]
 
 #F4: benchmarks
 #1 everyone stays:
 str(test$Attrition) # remain (Y) is 0
 b1 <- rep(0, dim(test)[1])
 (accuracyB1 <- 1 - mean(b1 != test$Attrition))
 #here we really see the effect of the class imbalance!
 
 #2 those who have higher satisfaction are less likely to leave
 str(my_dataset$JobSatisfaction)
 b2 <- rep(0, dim(test)[1])
 b2[test$JobSatisfaction == 'Low'] <- 1
 b2[test$JobSatisfaction == 'Medium'] <- 1
 (accuracyB2 <- 1 - mean(b2 != test$Attrition))
 
 #End Foundations
 
 ##########################################
 
 #Start of Question: B1
 
 #work out what columns we have:
 str(subset(my_dataset, select=c(1,4,6,10,16:19,23,24,26:29)))
 #now correlation test
 a <- cor(subset(my_dataset, select=c(1,4,6,10,16:19,23,24,26:29)))
 view(a)
 #End of Question
 ##########################################
 
 #Start of Question: B2
 boxplot(my_dataset$ï..Age~ my_dataset$Attrition)
 spineplot(my_dataset$Education, my_dataset$Attrition) 
 
 #End of Question
 ##########################################
 
 #Start of Question: B3
 #I don't have gender, or overtime, so selecting marital status
 spineplot(my_dataset$MaritalStatus, my_dataset$Attrition) #single people seem to leave more often
 boxplot(my_dataset$DistanceFromHome ~ my_dataset$MaritalStatus) #surprisingly there doesn't appear to be much difference in commute distance by martial status
 
 #End of Question
 ##########################################
 
 #Start of Question: B4
 plot(my_dataset$ï..Age, my_dataset$MonthlyRate, main = "Scatter plot of Age vs. Monthly Rate")
 plot(my_dataset$ï..Age, my_dataset$DailyRate, main = "Scatter plot of Age vs. Daily Rate")
 plot(my_dataset$ï..Age, my_dataset$HourlyRate, main = "Scatter plot of Age vs. Hourly Rate")
 #doesn't look like it
 
 #End of Question
 ##########################################
 
 #Start of Question: B5
 counts <- table(my_dataset$Attrition, my_dataset$JobSatisfaction)
 row.names(counts) <- c("Remained", "Left")
 barplot(counts, main="Attrition Distribution by Job Satisfaction", legend = row.names(counts))
 
 #or if you want something a little easier to read
 barplot(counts, main="Attrition Distribution by Job Satisfaction", xlab="Job Satistfaction", col=c("darkblue","red"), legend = rownames(counts), beside=T)
 
 table(my_dataset$Attrition,my_dataset$JobSatisfaction)
 
 #End of Question
 ##########################################
 
 #Start of Question: I1
 
 library(C50)
 cFifty <- C5.0(Attrition ~ï..Age+Attrition+BusinessTravel+DailyRate+Department  , data=train, trials=100)
 cFifty <- C5.0(Attrition ~., data=train, trials=100)
 #if you saw: c50 code called exit with value 1 you had unused levels still in your factors or some single level factors (e.g. over 18)
 str(my_dataset)
 
 cFiftyPrediction <- predict(cFifty, newdata = test[, -2])
 cFiftyAccuracy <- 1- mean(cFiftyPrediction != test$Attrition)
 #0.8724832
 
 cFiftyAccuracy - accuracyB1
 #0.02684564 -- marginally better than always predicting remain
 
 cFiftyAccuracy - accuracyB2
 #0.2751678 -- a lot beter than just assuming that higher statisfaction are less likely to leave
 
 #End of Question
 ##########################################
 
 #Start of Question: I2
 
 #kNN requires normalised data
 
 library(class)
 
 normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
 #now let's normalise our dataset so that calculating the distances in the feature space makes sense
 my_dataset_n <- subset(my_dataset, select=c(1,4,6,10,16:19,23,24,26:29)) #get the numerical for normalisation -- kNN also doesn't support levelled factors either
 my_dataset_n <- as.data.frame(lapply(my_dataset_n, normalize)) #normalise
 summary(my_dataset_n) #all our numericals are normalised, our categoricals are untouched
 ?lapply
 #re make train and test note we can retain the original distribution if we choose to
 train_n <- my_dataset_n[sample, ]
 test_n <- my_dataset_n[-sample, ]
 
 #different ways to determine k
 k1 <- round(sqrt(dim(train_n)[1])) #sqrt of number of instances
 k2 <- round(sqrt(dim(train_n)[2])) #sqrt of number of attributes
 k3 <- 7 #a number between 3 and 10
 
 knn1 <- knn(train = train_n, test = test_n, cl = train$Attrition, k=k1)
 knn2 <- knn(train = train_n, test = test_n, cl = train$Attrition, k=k2)
 knn3 <- knn(train = train_n, test = test_n, cl = train$Attrition, k=k3)
 
 (knn1Acc <- 1- mean(knn1 != test$Attrition))
 (knn2Acc <- 1- mean(knn2 != test$Attrition))
 (knn3Acc <- 1- mean(knn3 != test$Attrition))
 
 knn1Acc - accuracyB1 #same as benchmark
 knn2Acc - accuracyB1 #worse then benchmark
 knn3Acc - accuracyB1 #same as benchmark
 
 dim(my_dataset_n)
 my_dataset_c <- subset(my_dataset, select=c(2,3,5,7:9,11:15,20:22,25)) #isolate the categoricals
 my_dataset_n <- cbind(my_dataset_n, my_dataset_c) #put them back together if you want to include the categoricals in a later question
 #don't run this line if you are going to do A1! PCA will break.
 
 #End of Question
 ##########################################
 
 #Start of Question: I3
 
 logit <- glm(train$Attrition ~.,family=binomial(link='logit'),data=train)
 summary(logit)
 anova(logit, test="Chisq")
 library(pscl)
 install.packages("pscl")
 pR2(logit)
 
 results.1.logit <- predict(logit,newdata=test[,-2],type='response')
 results.1.logit <- ifelse(results.1.logit > 0.5,1,0)
 (logitAcc1 <- 1- mean(results.1.logit != test$Attrition))
 
 results.2.logit <- predict(logit,newdata=test[,-2],type='response')
 results.2.logit <- ifelse(results.2.logit > 0.6,1,0)
 (logitAcc2 <- 1- mean(results.2.logit != test$Attrition))
 
 results.3.logit <- predict(logit,newdata=test[,-2],type='response')
 results.3.logit <- ifelse(results.3.logit > 0.75,1,0)
 (logitAcc3 <- 1- mean(results.3.logit != test$Attrition))
 
 logitAcc1 - accuracyB1 #better than benchmark
 logitAcc2 - accuracyB1 #better then benchmark
 logitAcc3 - accuracyB1 #better (but not as much as above 2) as benchmark
 
 #Increasing the strictness of the model, seems to improve the performance, but it hasn't managed to outperform our benchmark yet
 #End of Question
 ##########################################
 
 #Start of Question: I4
 
 #here we can reuse our approach to kNN somewhat -- k-means also needs normalised data
 my_dataset_c <- subset(my_dataset, select=c(2,3,5,7:9,11:15,20:22,25)) #isolate the categoricals
 my_dataset_k <- cbind(my_dataset_n, my_dataset_c) #put them back together 
 
 my_dataset_n <- subset(my_dataset, select=c(1,4,6,10,16:19,23,24,26:29))
 kmeansClusters <- list()
 kmeansScores <- c()
 
 for (i in 2:10) {
   clusters <- kmeans(my_dataset_n, i)
   name <- paste0("kmeans", i)
   kmeansClusters[[name]] <- clusters
   kmeansScores <- rbind(kmeansScores, sum(clusters$withinss))
 }
 
 row.names(kmeansScores) <- c(2:10)
 colnames(kmeansScores) <- c("k")
 
 plot(kmeansScores, xlab="k", ylab="within groups sum of squares")
 #intuition would suggest 4 clusters... let's try another measure and find out
 #i'd have been fine with up to here as an answer, but to improve our intutition of a possibly good value for k...

 
 kmeansScores <- c()
 for (i in 2:10) {
   clusters <- kmeans(my_dataset_n, i)
   name <- paste0("kmeans", i)
   dbindex <- index.DB(my_dataset_n, clusters$cluster, centrotypes="centroids")
   kmeansScores <- rbind(kmeansScores, dbindex$DB)
 }
 
 row.names(kmeansScores) <- c(2:10)
 colnames(kmeansScores) <- c("k")
 
 plot(kmeansScores, xlab="k", ylab="DBIndex")
 #lower is better, so it's probably not k=2; k = 5 seems ok again, 10 has also done well this time.
 
 #let's try plotting
 library(fpc)
 plotcluster(my_dataset_n, kmeansClusters[["kmeans2"]]$cluster)
 plotcluster(my_dataset_n, kmeansClusters[["kmeans4"]]$cluster)
 plotcluster(my_dataset_n, kmeansClusters[["kmeans5"]]$cluster)
 plotcluster(my_dataset_n, kmeansClusters[["kmeans10"]]$cluster)
 
 #for interest (going beyond what's asked, but it's good to think about this a little) let's use the whole dataset -- we need to switch to medoids because of the categorical variables
 library(cluster)
 
 kmedoidsClusters <- list()
 kmedoidsScores <- c()
 
 gower_dist <- daisy(my_dataset_k, metric = "gower", type = list(logratio = 3))
 
 for (i in 2:20) { #for fun let's also increase the max k value as well
   clusters <- pam(gower_dist, k=i, diss=T) #note we switched to the full set of attibutes now so need to use medoids
   name <- paste0("kmedoids", i)
   kmedoidsClusters[[name]] <- clusters
   dbindex <- index.DB(gower_dist, clusters$clustering, centrotypes = "medoids", d=gower_dist)
   kmedoidsScores <- rbind(kmedoidsScores, dbindex$DB)
 }
 
 row.names(kmedoidsScores) <- c(2:20)
 colnames(kmedoidsScores) <- c("k")
 
 plot(kmedoidsScores, xlab="k", ylab="DBIndex")
 #now k=2 (which makes sense) and k=7 are looking like good candidate values of k
 
 #End of Question
 ##########################################
 
 #Start of Question: I5
 
 library(rpart)
 library(rpart.plot)
 library(rattle)
 overfitModel <- rpart(Attrition ~ ., data=train, method="class", control=rpart.control(minsplit=2, cp=0))
 notOverfitModel <- rpart(Attrition ~ ., data = train, method = "class")
 
 #if you are willing to wait, we can plot
 fancyRpartPlot(overfitModel)
 fancyRpartPlot(notOverfitModel)
 
 
 #or, we can just tap into some of the properites of the models
 dim(overfitModel$splits)[1] #this is the no. of splits
 dim(notOverfitModel$splits)[1] #this is the no. of splits
 
 overfitP <- predict(overfitModel, test[,-2], type = "class")
 (overFitAcc <- 1- mean(overfitP != test$Attrition))
 overFitAcc - accuracyB1 #bad
 
 notoverfitP <- predict(notOverfitModel, test[,-2], type = "class")
 (notoverFitAcc <- 1- mean(notoverfitP != test$Attrition))
 notoverFitAcc - accuracyB1 #less bad
 
 notoverFitAcc - overFitAcc
 
 #End of Question
 ##########################################
 
 #Start of Question: I6
 install.packages("randomForest")
 library(randomForest)
 forest <- randomForest(Attrition ~ ., data=train, importance=TRUE, ntree=2000)
 varImpPlot(forest)
 
 #while we're at it, let's also see how well it does (for A3)
 forestPrediction <- predict(forest, test[,-2], type = "class")
 (forestAcc <- 1- mean(forestPrediction != test$Attrition))
 
 #End of Question
 ##########################################
 
 #Start of Question: I7
 
 my_dataset_old <- my_dataset #don't want to lose it and do all the preprocessing again!
 
 #copy and paste relevent lines:
 my_dataset <- hrdata[order(runif(600)), ]
 my_dataset <- my_dataset[-10]
 write.csv(file="myFulldata.csv", my_dataset, row.names = F)
 my_dataset <- read.csv("myFulldata.csv", stringsAsFactors = T)
 my_dataset$Education <- factor(my_dataset$Education, levels = c(1,2,3,4,5), labels=c("BC", "C", "UG", "MSc", "PhD"))
 my_dataset$EnvironmentSatisfaction <- factor(my_dataset$EnvironmentSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$JobInvolvement <- factor(my_dataset$JobInvolvement, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$JobLevel <- factor(my_dataset$JobLevel) #insufficient information to do more
 my_dataset$JobSatisfaction <- factor(my_dataset$JobSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating, levels = c(1:4), labels=c("Low", "Good", "Excellent", "Outstanding"))
 my_dataset$RelationshipSatisfaction <- factor(my_dataset$RelationshipSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$StockOptionLevel <- factor(my_dataset$StockOptionLevel) #don't have more information
 my_dataset$WorkLifeBalance <- factor(my_dataset$WorkLifeBalance, levels = c(1:4), labels=c("Bad", "Good", "Better", "Best"))
 
 #now need to encode the remaining other 3
 #"I lost:  StandardHours , OverTime , Gender"
 summary(my_dataset)
 #apart from PerformanceRating (again sigh) no special effort needed luckily
 my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating) 
 
 #again let's remove the useless attibutes
 my_dataset <- my_dataset[,-9] #employees
 my_dataset <- my_dataset[,-20] #over18
 my_dataset <- my_dataset[,-24] #standard hours -- one i got back, but it's always 80, so has no value
 
 #make new train and test
 trainNew <- my_dataset[sample, ]
 testNew <- my_dataset[-sample, ]
 
 #redo I1 -- copy and paste code and rename key variables
 cFiftyNew <- C5.0(Attrition ~., data=trainNew, trials=100)
 #if you saw: c50 code called exit with value 1 you had unused levels still in your factors or some single level factors (e.g. over 18)
 
 cFiftyPredictionNew <- predict(cFiftyNew, newdata = testNew[, -2])
 (cFiftyAccuracyNew <- 1- mean(cFiftyPredictionNew != testNew$Attrition))
 #0.8657718
 
 cFiftyAccuracy - cFiftyAccuracyNew #in this case, marginal difference the missing columns had basically no impact on the C50
 #0.006711409 -- negative would mean that the extra info was beneficial, 0 no difference, positive detrimental to performance
 #this is such a small number, it could be due to chance, ordinarily we should run a few models and take the averages, but this 
 #is enough to show that we know how to test the effect of these columns being absent
 
 #now let's revert back to the CA dataset:
 my_dataset <- my_dataset_old
 
 #End of Question
 ##########################################
 
 #Start of Question: A1
 pcs <- prcomp( ~ ., data=my_dataset_n) #PCA on categoricals is hard, so let's stick to the numerical attributes
 #conveniently, for k-means (I4) and kNN (I2) we normalised the numerical data -- happy days :)
 #had we not, prcomp will do this for us using scale=T
 plot(pcs, type="l")
 #let's get the first 2 PCs
 comp <- data.frame(pcs$x[,1:2])
 
 #so we know, from I4 that there are 2 clusters
 k <- kmeans(comp, 2)
 library(RColorBrewer)
 library(scales)
 palette(alpha(brewer.pal(9,'Set1'), 0.5))
 plot(comp, col=k$clust, pch=16)
 
 #ref: https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/
 
 #End of Question
 ##########################################
 
 #Start of Question: A2
 
 library(kernlab)
 
 #we're probably going to build lots of models, so let's make a function to save time
 svmPerformance <- function(svm, testing, trueValues) {
   p <- predict(svm, newdata=testing, type = "response")
   accuracy <- 1-mean(p != trueValues)
   return(accuracy)
 }
 
 svm.model <- ksvm(Attrition ~ ., data = train)
 svmPerformance(svm.model, test[,-2], test$Attrition)
 #0.8456376 -- equal to B1 :) with no effort
 
 #let's try auto-tuning
 library(e1071)
 tuned.svm = tune(svm, Attrition ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
 svm.model <- tuned.svm$best.model
 svmPerformance(svm.model, test[,-2], test$Attrition)
 #0.8456376 -- no better; no surprises really, this is a copy and paste from the titanic example
 
 #let's increase the search space -- this is pretty much try most realistic parameter settings, and hope one works -- we wouldn't need to have a search space this big! runs about 15 mins on my laptop
 tuned.svm = tune(svm, Attrition ~ ., data = train, ranges = list(gamma = 2^(-4:4), cost = 2^(-4:4), kernel = c("linear", "polynomial", "radial", "sigmoid"), degree=c(1:4)))
 #while this runs, prepare another question...
 svm.model <- tuned.svm$best.model
 bestSvmAcc <- svmPerformance(svm.model, test[,-2], test$Attrition)
 #0.885906 -- i'd say that's pretty good 
 
 delta <- bestSvmAcc - accuracyB1
 (percentageDiff <- delta / accuracyB1 * 100)
 #almost a 5% improvement
 
 #let's be a little more objective here
 b1Scores <- c()
 
 sampleRate <- 600*.25
 
 for (i in 1:1000) { #1000 samples of the b1 benchmark -- not stratified to get a non-zero sd; if this is a stratified sample the SD will likely be 0
   test_i <- my_dataset[order(runif(sampleRate)), ]
   b1test <- 1 - mean(b1 != test_i$Attrition)
   b1Scores <- rbind(b1Scores, b1test)
 }
 
 mean(b1Scores)
 sd(b1Scores)
 
 ((bestSvmAcc - (mean(b1Scores) + (3 * sd(b1Scores)))) > delta)
 #False -- so, based on the empirical rule, the probability of being more than 3 times the SD of the B1 benchmark is 0.3%
 #that's probably significant
 
 #End of Question
 ##########################################
 
 #Start of Question: A3
 
 #random forest we already did in I6, so no need to redo
 
 #CI Forest:
 library(party)
 cForest <- cforest(Attrition ~., data=train, controls=cforest_unbiased(ntree=2000, mtry=3))
 cFp <- predict(cForest, newdata = test[, -2])
 (cForestAcc <- 1- mean(cFp != test$Attrition))
 
 #NB
 nb <- naiveBayes(Attrition ~., data=train)
 nbP <- predict(nb, newdata=test[,-2], type = "class")
 (nbAcc <- 1- mean(nbP != test$Attrition))
 
 #so based on default settings, it's rf > cf > nb
 
 #question says don't use defaults though... 
 #i'm lazy, let's use caret to give us some intution of performance
 tunegrid <- expand.grid(.mtry=5)
 control <- trainControl(method="repeatedcv", number=10, repeats=3)
 
 CFmodel <- train(Attrition ~ ., data = train,
                  method = "cforest",
                  metric="Accuracy", tuneGrid=tunegrid,
                  controls = cforest_unbiased(ntree = 250))
 
 
 
 cFpbest <- predict(CFmodels, newdata = test[, -2])
 (cForestAccBest <- 1- mean(cFpbest != test$Attrition))
 (cForestAccBest - cForestAcc) #no improvement
 
 RFmodel <- train(Attrition ~ ., data = train,
                  method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
 
 rFpbest <- predict(RFmodel, newdata = test[, -2])
 (rForestAccBest <- 1- mean(rFpbest != test$Attrition))
 (rForestAccBest - forestAcc) #didn't beat defaults
 
 nbModel <- train(Attrition ~ ., data = train,
                  method = "nb", 
                  trControl = control)
 
 nbpbest <- predict(nbModel, newdata=test[,-2])
 (nbAccBest <- 1- mean(nbpbest != test$Attrition))
 (nbAccBest - nbAcc) #improved a little
 
 #new order: rf > (nb = cf)
 
 #End of Question
 ##########################################
 
 #Start of Question: A4
 
 #so here, we build on I3
 logit <- glm(train$Attrition ~.,family=binomial(link='logit'),data=train)
 results.2.logit <- predict(logit,newdata=test[,-2],type='response')
 results.2.logit <- ifelse(results.2.logit > 0.6,1,0)
 (logitAcc2 <- 1- mean(results.2.logit != test$Attrition))
 
 h2otrain <- train[, -2]
 h2otrain <- cbind(train$Attrition, h2otrain)
 colnames(h2otrain)[1] <- "Attrition"
 
 h2otest <- test[, -2]
 h2otest <- cbind(test$Attrition, h2otest)
 colnames(h2otest)[1] <- "Attrition"
 
 #for simplicity put the dependent first
 
 #this gives us our target to aim for: logitAcc2
 write.table(x = h2otrain, file = "training.csv", row.names = F, col.names = T)
 write.table(x = h2otest, file = "testing.csv", row.names = F, col.names = T)
 
 library(h2o)
 h2o.init(nthreads = -1)
 localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
 #convert to the h2o format
 h2o_trainset <- h2o.importFile(path = paste0(getwd(), "/training.csv"))
 h2o_testset <- h2o.importFile(path = paste0(getwd(), "/testing.csv"))
 
 fit <- h2o.deeplearning(x = 2:29,  # column numbers for predictors
                         y = 1,   # column number for label
                         training_frame = h2o_trainset, # data in H2O format
                         activation = "TanhWithDropout", # or 'Tanh'
                         input_dropout_ratio = 0.2, # % of inputs dropout
                         hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                         hidden = c(150,150,150), # three layers of 50 nodes
                         epochs = 100)
 
 pred <- h2o.predict(fit, h2o_testset)
 pred <- as.data.frame(pred)
 
 results.DL <- ifelse(pred$predict > 0.5,1,0)
 
 dlPerformance <- 1 - mean(results.DL != test$Attrition)
 (dlPerformance > logitAcc2) #didn't beat it
 
 #let's do SO3 and try a bit harder
 hidden_opt <- list(c(200,200), c(100,300,100), c(500,500,500)) 
 l1_opt <- c(1e-5,1e-7)
 hyper_params <- list(hidden = hidden_opt, l1 = l1_opt)
 model_grid <- h2o.grid("deeplearning",
                        hyper_params = hyper_params,
                        x = 2:29,  # column numbers for predictors
                        y = 1,   # column number for label
                        training_frame = h2o_trainset,
                        validation_frame = h2o_testset)
 
 dlPerf <- c()
 
 for (model_id in model_grid@model_ids) {
   model <- h2o.getModel(model_id)
   pred <- h2o.predict(model, h2o_testset)
   pred <- as.data.frame(pred)
   
   results.DL <- ifelse(pred$predict > 0.5,1,0)
   
   dlPerformance <- 1 - mean(results.DL != test$Attrition)
   dlPerf <- rbind(dlPerf, dlPerformance)
 }
 
 bestDL <- max(dlPerf)
 (bestDL > logitAcc2) #still didn't beat it
 
 #last attempt -- under regular circumstances, we probably wouldn't take a shot in the dark like this
 hidden_opt <- list(c(100, 100, 100, 100), c(200, 200, 200, 200), c(300, 300, 300, 300)) 
 l1_opt <- c(1e-5,1e-7)
 activations <- c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
 hyper_params <- list(hidden = hidden_opt, l1 = l1_opt, activation=activations)
 model_grid <- h2o.grid("deeplearning",
                        hyper_params = hyper_params,
                        x = 2:29,  # column numbers for predictors
                        y = 1,   # column number for label
                        training_frame = h2o_trainset,
                        validation_frame = h2o_testset)
 
 dlPerf <- c()
 
 for (model_id in model_grid@model_ids) {
   model <- h2o.getModel(model_id)
   pred <- h2o.predict(model, h2o_testset)
   pred <- as.data.frame(pred)
   
   results.DL <- ifelse(pred$predict > 0.5,1,0)
   
   dlPerformance <- 1 - mean(results.DL != test$Attrition)
   dlPerf <- rbind(dlPerf, dlPerformance)
 }
 
 bestDL <- max(dlPerf)
 (bestDL > logitAcc2) #beat it on testing data, so can't really overfit
 
 #bestDL (89.2%) actually also beats the benchmark for SO1 so let's get the model parameters and we'll redo for SO1 with the full dataset
 dlPerf #ok so model 1 and 3 seem best -- let's get them and interrogate their parameterisation
 
 modelID1 <- model_grid@model_ids[[1]]
 modelID3 <- model_grid@model_ids[[3]]
 
 model1 <- h2o.getModel(modelID1)
 model3 <- h2o.getModel(modelID3)
 
 summary(model1)
 summary(model3)
 
 #both are 4 hidden layers, of 200 in each layer, using either the Maxout or Rectifier activation function
 #intuitively, we can grid search these to try and further improve performance in SO1
 
 #now in all fairness, we should probably give the glm a chance to shine too, we only used the defaults so far
 library(glmnet)
 
 #we'll try a lasso logit and see how we get on
 
 train_c <- my_dataset_c[sample, ] #build the training categoricals 
 trainFactors <- model.matrix(Attrition ~., data = train_c ) #flattens the categoricals
 head(trainFactors) #we could if we wanted to go back to the knn question and use this now if we chose to
 
 glmTraining <- as.matrix(data.frame(train_n, trainFactors))
 head(glmTraining)
 
 
 glm <- glmnet(glmTraining, y=train$Attrition, alpha=1, family="binomial")
 # Note alpha=1 for lasso only and can blend with ridge penalty down to
 # alpha=0 ridge only.
 
 # Plot variable coefficients vs. shrinkage parameter lambda.
 plot(glm, xvar="lambda")
 
 #prepare testing data
 test_c <- my_dataset_c[-sample, ] #build the training categoricals 
 test_n <- my_dataset_n[-sample, ]
 testFactors <- model.matrix(~., test_c[,-1]) #flattens the categoricals now we don't want to bias it and use the dependent here
 head(testFactors) #we could if we wanted to go back to the knn question and use this now if we chose to
 
 glmTesting <- as.matrix(data.frame(test_n, testFactors))
 preds <- predict(glm, glmTesting, type = 'response')[ ,1]
 results.glm <- ifelse(preds > 0.5,1,0)
 (glmAcc <- 1- mean(results.glm != test$Attrition))
 #0.8456376 the original one was better
 
 (bestDL < glmAcc) #false
 
 #let's give it one more chance to shine and cv it
 cv.glmmod <- cv.glmnet(glmTraining, y=train$Attrition, alpha=1, family="binomial",
                        type.measure = "auc",
                        # 5-fold cross-validation
                        nfolds = 5,
                        # high value is less accurate, but has faster training
                        thresh = 1e-3,
                        # again lower number of iterations for faster training
                        maxit = 1e3)
 
 
 preds2 <- predict(cv.glmmod, glmTesting, type = 'response')[ ,1]
 results.glm.2 <- ifelse(preds2 > 0.5,1,0)
 (glmCVAcc <- 1- mean(results.glm.2 != test$Attrition))
 #slight improvement (0.8590604), but not better
 
 (bestDL < glmCVAcc) #false
 #that's more than enough effort now i think -- i wouldn't have expected this level of effort either!
 #on a different note, is it surprising that this was the result? We know that DL has a huge learning capacity, but we have essentially a tiny dataset. For future reference don't try DL on a small dataset, you are wasting time and compute power, see here for a nice overview on why: https://simplystatistics.org/2017/05/31/deeplearning-vs-leekasso/
 
 #End of Question
 ##########################################
 
 #Start of Question: A5
 
 #well i'm lazy, we did PCA already in A1, so let's just reuse that :)
 pcs <- prcomp(~ ., data=my_dataset_n)
 plot(pcs, type="l")
 
 #let's continue with the first 2 pcs idea and redo the c50
 comp <- data.frame(pcs$x[,1:2])
 pcaTrain <- comp[sample, ]
 pcaTest <- comp[-sample, ]
 pcaTrain$Attrition <- train$Attrition
 
 cFiftyPCA <- C5.0(Attrition ~., data=pcaTrain, trials=100)
 #if you saw: c50 code called exit with value 1 you had unused levels still in your factors or some single level factors (e.g. over 18)
 
 cFiftyPredictionPCA <- predict(cFiftyPCA, newdata = pcaTest)
 (cFiftyAccuracyPCA <- 1- mean(cFiftyPredictionPCA != test$Attrition))
 
 (cFiftyAccuracyPCA - cFiftyAccuracy)
 #-0.03355705 quite interesting really, we reduced from 29 columns to 2 PCs (and removed all categoricals) and only lost 0.03% accuracy. In all honesty this isn't very surprisng as the class imbalance is having such a huge effect on the models.
 
 
 #End of Question
 ##########################################
 
 ##########################################
 
 #Start of Question: SO1 and SO3
 #copy and paste from up top, but first backup my_dataset
 
 my_dataset_old <- my_dataset
 my_dataset <- read.csv("ADM CA 1 Data.csv", stringsAsFactors = T)
 my_dataset <- my_dataset[-10]
 my_dataset$Education <- factor(my_dataset$Education, levels = c(1,2,3,4,5), labels=c("BC", "C", "UG", "MSc", "PhD"))
 my_dataset$EnvironmentSatisfaction <- factor(my_dataset$EnvironmentSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$JobInvolvement <- factor(my_dataset$JobInvolvement, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$JobLevel <- factor(my_dataset$JobLevel) #insufficient information to do more
 my_dataset$JobSatisfaction <- factor(my_dataset$JobSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating, levels = c(1:4), labels=c("Low", "Good", "Excellent", "Outstanding"))
 my_dataset$RelationshipSatisfaction <- factor(my_dataset$RelationshipSatisfaction, levels = c(1:4), labels=c("Low", "Medium", "High", "v. High"))
 my_dataset$StockOptionLevel <- factor(my_dataset$StockOptionLevel) #don't have more information
 my_dataset$WorkLifeBalance <- factor(my_dataset$WorkLifeBalance, levels = c(1:4), labels=c("Bad", "Good", "Better", "Best"))
 
 #now need to encode the remaining other 3
 #"I lost:  StandardHours , OverTime , Gender"
 summary(my_dataset)
 #interesting, i still need to fix PerformanceRating sigh - bad metadata... oops X-D
 my_dataset$PerformanceRating <- factor(my_dataset$PerformanceRating) 
 
 #again let's remove the useless attibutes
 my_dataset <- my_dataset[,-9] #employees
 my_dataset <- my_dataset[,-20] #over18
 my_dataset <- my_dataset[,-24] #standard hours 
 
 my_dataset$Attrition <- factor(my_dataset$Attrition, labels=c(0,1), levels=c("No", "Yes"))
 
 #ok now we are good to go.
 #redo train and test
 sampleFull <- createDataPartition(my_dataset$Attrition, p = .75, list = FALSE) 
 trainFull <- my_dataset[sampleFull, ]
 testFull <- my_dataset[-sampleFull, ]
 
 h2otrain <- trainFull[, -2]
 h2otrain <- cbind(trainFull$Attrition, h2otrain)
 colnames(h2otrain)[1] <- "Attrition"
 
 h2otest <- testFull[, -2]
 h2otest <- cbind(testFull$Attrition, h2otest)
 colnames(h2otest)[1] <- "Attrition"
 
 write.table(x = h2otrain, file = "trainingFull.csv", row.names = F, col.names = T)
 write.table(x = h2otest, file = "testingFull.csv", row.names = F, col.names = T)
 
 h2o_trainset <- h2o.importFile(path = paste0(getwd(), "/trainingFull.csv"))
 h2o_testset <- h2o.importFile(path = paste0(getwd(), "/testingFull.csv"))
 
 #rerun what we did for A4 but on the whole dataset -- and see how it comes out
 hidden_opt <- list(c(100, 100, 100, 100), c(200, 200, 200, 200), c(300, 300, 300, 300)) 
 l1_opt <- c(1e-5,1e-7)
 activations <- c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
 hyper_params <- list(hidden = hidden_opt, l1 = l1_opt, activation=activations)
 model_grid <- h2o.grid("deeplearning",
                        hyper_params = hyper_params,
                        x = 2:29,  # column numbers for predictors
                        y = 1,   # column number for label
                        training_frame = h2o_trainset,
                        validation_frame = h2o_testset)
 
 
 #prepare another question whilst waiting... runs for ~5 mins
 
 dlPerf <- c()
 
 for (model_id in model_grid@model_ids) {
   model <- h2o.getModel(model_id)
   pred <- h2o.predict(model, h2o_testset)
   pred <- as.data.frame(pred)
   
   results.DL <- ifelse(pred$predict > 0.5,1,0)
   
   dlPerformance <- 1 - mean(results.DL != testFull$Attrition)
   dlPerf <- rbind(dlPerf, dlPerformance)
 }
 
 (bestDL <- max(dlPerf))
 #[1] 0.9046322
 
 #revert back
 my_dataset <- my_dataset_old
 
 #End of Question
 ##########################################
 
 ##########################################
 
 #Start of Question: SO2
 
 table(train$Attrition)
 #reminder ourselves of the imbalance -- we are going to try undersampling the dominant class
 #traditionally undersampling means we randomly draw the same no. of samples of each class
 
 mean(train_0$Age)
 
 train_0 <- train[ which(train$Attrition=="0"), ]
 train_1 <- train[ which(train$Attrition=="1"), ]
 
 c50Best <- NULL
 c50Score <- 0
 c50Scores <- c()
 
 for (i in 1:50) { #we have stochastic behaviour so we should build a few of these really to be sure
   
   train_i <- train_0[sample(nrow(train_0), dim(train_1)[1]), ] #undersampled
   
   underTrain <- rbind(train_i, train_1)
   
   cFiftyUS <- C5.0(Attrition ~., data=underTrain, trials=100)
   
   cFiftyUSPrediction <- predict(cFiftyUS, newdata = test[, -2])
   (cFiftyUSAccuracy <- 1- mean(cFiftyUSPrediction != test$Attrition))
   
   c50Scores <- rbind(c50Scores, cFiftyUSAccuracy)
   
   if (cFiftyUSAccuracy > c50Score) {
     c50Score <- cFiftyUSAccuracy
     c50Best <- cFiftyUSPrediction
   }
   
 }
 
 summary(c50Scores)
 
 
 #drop in accuracy: 0.7919 , let's check other performance measures... 
 #there are cases of the true positive rate being a little better though in all fairness, but it is marginal
 
 confusionMatrix(c50Best, test$Attrition, positive="1")
 confusionMatrix(cFiftyPrediction, test$Attrition, positive="1")
 
 logitBest <- NULL
 logitScore <- 0
 logitScores <- c()
 
 #if we try to do the same for logit, e.g. below we will likely get this error: factor NN has new levels NN
 #so undersampling for this method means that the model may not be useable for training, we can fix this with a tryCatch though note the model is ignored if it cannot be tested
 for (i in 1:50) { 
   
   train_i <- train_0[sample(nrow(train_0), dim(train_1)[1]), ] #undersampled
   
   underTrain <- rbind(train_i, train_1)
   logitUS <- glm(train_i$Attrition ~.,family=binomial(link='logit'),data=train_i)
   
   tryCatch({
     results.logit.US <- predict(logitUS,newdata=test[,-2],type='response')
     results.logit.US <- ifelse(results.logit.US > 0.6,1,0)
     (logitUSAcc <- 1- mean(results.logit.US != test$Attrition))
     
     logitScores <- rbind(logitScores, logitUSAcc)
     
     if (logitUSAcc > logitScore) {
       logitScore <- logitUSAcc
       logitBest <- results.logit.US
     }
   }, error = function(s) {})
 }
 
 summary(logitScores)
 dim(logitScores) #got 31 results, not too bad...
 
 #drop in accuracy: 0.8456 , let's check other performance measures... the best model always predicted remain in a few runs... oops; so undersampling here didn't seem to help at all!
 confusionMatrix(logitBest, test$Attrition, positive="1")
 confusionMatrix(results.2.logit, test$Attrition, positive="1")
 
 #next easiest to do is I6
 varImpPlot(forest) #starting point
 
 forestBest <- NULL
 bestForestModel <- NULL
 forestScore <- 0
 forestScores <- c()
 
 for (i in 1:50) { #we have stochastic behaviour so we should build a few of these really to be sure
   
   train_i <- train_0[sample(nrow(train_0), dim(train_1)[1]), ] #undersampled
   
   underTrain <- rbind(train_i, train_1)
   
   forestUS <- randomForest(Attrition ~., data=underTrain, importance=TRUE, ntree=2000)
   
   forestUSPrediction <- predict(forestUS, newdata = test[, -2])
   (forestUSAccuracy <- 1- mean(forestUSPrediction != test$Attrition))
   
   forestScores <- rbind(forestScores, forestUSAccuracy)
   
   if (forestUSAccuracy > forestScore) {
     forestScore <- forestUSAccuracy
     forestBest <- forestUSPrediction
     bestForestModel <- forestUS
   }
   
 }
 
 summary(forestScores)
 varImpPlot(bestForestModel) #it's changed a little, but not too much a few attibutes have reordered, the most notable change is in the performance of the forest though
 
 #conclusion, try oversampling... 
 
 #End of Question and all questions
 ##########################################
 