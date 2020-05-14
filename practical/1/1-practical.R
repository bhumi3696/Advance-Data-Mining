#KDD 
setwd("D:/SEM 2/ADM/practical/1")
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
y <- titanicData$Survived
table(y)
y <- factor(y, levels = c(0,1), labels = c("No", "Yes"))
table(y)
prop.table(table(y))
barplot(table(y), main = "Distribution of Titanic Surivial", ylab="Frequency")

#25% sample data
set.seed(1337)
index <- sample(1:length(y), length(y) * .25, replace=FALSE)
testing <- y[index]

#create first model
perishModel <- rep("No", length(testing))
perishModel <- factor(perishModel, levels = c("No", "Yes"), labels = c("No", "Yes"))
#create second model
coinModel <- round(runif(length(testing), min=0, max=1))
coinModel <- factor(coinModel, levels =c(0,1), labels = c("No", "Yes"))

table(testing, perishModel)
table(testing, coinModel)

(coinAccuracy <- 1 - mean(coinModel != testing))
(perishAccuracy <- 1 - mean(perishModel != testing))
prop.table(table(testing))
#create two vactor
perish <- c()
coin <- c()
for (i in 1:1000) {
  index <- sample(1:length(y), length(y) * .25, replace=FALSE)
  testing <- y[index]
  coinModel <- round(runif(length(testing), min=0, max=1))
  coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))
  coin[i] <- 1 - mean(coinModel != testing)
  perish[i] <- 1 - mean(perishModel != testing)
}
results <- data.frame(coin, perish)
names(results) <- c("Coin Toss Accuracy", "Everyone Perishes Accuracy")
summary(results)

library(ggplot2)

library(reshape)
ggplot(melt(results), mapping = aes (fill = variable, x = value)) + geom_density (alpha = .5)

#sex
df <- titanicData[, c("Survived", "Sex")]
df$Survived <- factor(df$Survived, levels = c(0,1), labels = c("No", "Yes"))
index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
training <- df[index, ]
testing <- df[-index, ]
predictSurvival <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$Sex == 'female'] <- "Yes"
  return(model)
}
women <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
  testing <- df[-index, ]
  womenModel <- predictSurvival(testing)
  women[i] <- 1 - mean(womenModel != testing$Survived)
}
results$`Women Accuracy` <- women
names(results) <- c("Coin", "All Perish", "Women")
boxplot(results)


#age
df1 <- titanicData[, c("Survived", "Age")]
df1$Survived <- factor(df1$Survived, levels = c(0,1), labels = c("No", "Yes"))
df1$child <- ifelse(df1$Age>=18,0,1)
df1$child <- factor(df1$child, levels = c(0,1), labels = c("A", "C"))
index <- sample(1:dim(df1)[1], dim(df1)[1] * .75, replace=FALSE)
training1 <- df1[index, ]
testing1 <- df1[-index, ]
predictSurvival1 <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$child == 'C'] <- "Yes"
  return(model)
}
child <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df1)[1], dim(df1)[1] * .75, replace=FALSE)
  testing1 <- df1[-index, ]
  ageModel <- predictSurvival1(testing1)
  child[i] <- 1 - mean(ageModel != testing1$Survived)
}
results$`age Accuracy` <- child
names(results) <- c("Coin", "All Perish", "women","child")
boxplot(results)

#Passenger Class
df2 <- titanicData[, c("Survived", "Pclass")]
df2$Survived <- factor(df2$Survived, levels = c(0,1), labels = c("No", "Yes"))
#df1$child <- ifelse(df1$Age>=18,0,1)
#df1$child <- factor(df1$child, levels = c(0,1), labels = c("A", "C"))
index <- sample(1:dim(df2)[1], dim(df2)[1] * .75, replace=FALSE)
training2 <- df2[index, ]
testing2 <- df2[-index, ]
predictSurvival2 <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$Pclass == '1'] <- "Yes"
  return(model)
}
Fclass <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df2)[1], dim(df2)[1] * .75, replace=FALSE)
  testing2 <- df2[-index, ]
  PclassModel <- predictSurvival2(testing2)
  Pclass[i] <- 1 - mean(PclassModel != testing2$Survived)
}
results$`First class Accuracy` <- Pclass
names(results) <- c("Coin", "All Perish", "women","child","First class")
boxplot(results)


#lab 2
library(gmodels)
CrossTable(testing$Survived, womenModel)
CrossTable(testing$Survived, womenModel, prop.chisq = F, prop.c = F, prop.r = F)
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
confusionMatrix(table(womenModel, testing$Survived), positive = "Yes")

library(ModelMetrics)
auc(womenModel, testing$Survived)
#install.packages("ROCR")
library(ROCR)
data(ROCR.testing)
womenModel <- as.factor(womenModel)
pWomenModel <- prediction(as.numeric(womenModel), testing$Survived)
perfWomenModel <- performance(pWomenModel, measure = "tpr", x.measure = "fpr")
plot(perfWomenModel)
auc <- performance(pWomenModel, measure = "auc")
auc <- auc@y.values[[1]]
auc
#install.packages("mice")
library(mice)
titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData <- titanicData[, -c(1,11)]
titanicData$Embarked[c(62, 830)] <- 'C'
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age
#feature engineering: make a feature to represent a passenger is a child
titanicData$Child[titanicData$Age < 18] <- "Yes"
titanicData$Child[titanicData$Age >= 18] <- "No"
titanicData$Child <- factor(titanicData$Child)
#feature engineer a title feature
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)
titanicData$Title[titanicData$Title == 'Mlle'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Ms'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme'] <- 'Mrs'
titanicData$Title[titanicData$Title %in% rare_title] <- 'Rare Title'
titanicData$Title <- as.factor(titanicData$Title)
#feature engineer a few more things using the passenger name
titanicData$Name <- as.character(titanicData$Name)
titanicData$Surname <- sapply(titanicData$Name,
                              FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1
#remove features 3, 7, and 11
titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL
# feature engineer a family size categorical variable
titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)
contrasts(titanicData$Sex)
contrasts(titanicData$Pclass)
#TASK 1
set.seed(1337)
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .8, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
logit <- glm(Survived ~.,family=binomial(link='logit'),data=training)

summary(logit)
plot(logit)



#lab 3
setwd("D:/SEM 2/ADM/practical/1")
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
str(titanicData)
table(titanicData$SibSp)
table(titanicData$Parch)
titanicData$SibSp = as.factor(titanicData$SibSp)
titanicData$Parch = as.factor(titanicData$Parch)
par(mfrow=c(1,2))
hist(titanicData$Fare, breaks = 30)
hist(titanicData$Age)
titanicData$FareBinned <- cut(titanicData$Fare,
                              breaks = c(0,10,50,max(titanicData$Fare)),
                              labels=c("low", "middle", "high"))
table(titanicData$FareBinned, titanicData$Pclass)
aggregate(Fare ~ Pclass, data=titanicData, FUN=summary)

titanicData$Age[is.na(titanicData$Age)] <- median(titanicData$Age,na.rm = "T")
titanicData$AgeBinned <- cut(titanicData$Age,
                             breaks = c(0,10,20,30,40,50,60,70,max(titanicData$Age)),
                             labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(titanicData$AgeBinned)
AgeBinned = as.factor(titanicData$AgeBinned)
str(titanicData)
titanicData[c(1,3,6,10)] <- NULL

#install.packages("e1071")
library(e1071)
titanicData$Survived = factor(titanicData$Survived, levels=c(0,1), labels=c("No","Yes"))
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
nb <- naiveBayes(training, training$Survived, type = 'class')
nbPredict <- predict(nb, testing[,-1], type='class')
length(testing$Survived)

#Naive_Bayes_Model=naiveBayes(Survived ~., data=training, type = 'class')
#NB_Predictions=predict(Naive_Bayes_Model,testing[,-1], type='class')
#Confusion matrix to check accuracy
table(nbPredict,testing$Survived)
1-mean(nbPredict != testing$Survived)



#Lab4(C5.0)
setwd("D:/SEM 2/ADM/practical/1")
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
save(titanicData, file="titanticData.RData")
set.seed(1337)
#install.packages("C50")
library(C50)
cFifty <- C5.0(Survived ~., data=training, trials=10)
titanicData$Survived <- as.factor(titanicData$Survived)
titanicData = na.omit(titanicData)
str(titanicData)
#Task 1
library(C50)
library(caret)
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
cFifty <- C5.0(Survived ~ PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Cabin+Embarked, data=training)
c <- predict(cFifty, testing[, c(-4,-2,-9)])
caret::confusionMatrix(c, testing$Survived)
#winnowed
cFiftyWinnow <- C5.0(Survived ~ PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Cabin+Embarked, data = training, control = C5.0Control(winnow = TRUE))
c <- predict(cFiftyWinnow, testing[, c(-4,-2,-9)])
caret::confusionMatrix(c, testing$Survived)
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=5) #5 x 10-fold cv
metric <- "Kappa"
a <-train(Survived~., data=titanicData, method="C5.0", metric=metric, trControl=control)
#task 2
#install.packages("dummies")
library(dummies)
## dummies-1.5.6 provided by Decision Patterns
dummyDF <- dummy.data.frame(training)
dummyDF$Survived <- training$Survived
dummyDFTesting <- dummy.data.frame(testing)
cFiftyDummy <- C5.0(Survived ~ PassengerId+Pclass+Age+SibSp+Parch+Fare, data = dummyDF)
pred <- predict(cFiftyDummy, dummyDFTesting)
caret::confusionMatrix(pred, testing$Survived)
#Binning
titanicData$FareBinned <- cut(titanicData$Fare,
                              breaks = c(0,10,50,max(titanicData$Fare)),
                              labels=c("low", "middle", "high"))
titanicData$AgeBinned <- cut(titanicData$Age,
                             breaks = c(-1,10,20,30,40,50,60,70,max(titanicData$Age)),
                             labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
titanicData$SibSp <- factor(titanicData$SibSp)
titanicData$Parch <- factor(titanicData$Parch)
titanicData[c(4,7,11)] <- NULL
trainBinned <- titanicData[index, ]
testBinned <- titanicData[-index, ]
cFiftyBinned <- C5.0(Survived ~ PassengerId+Pclass+Age+Parch+Fare+Embarked, data = trainBinned)
pred <- predict(cFiftyBinned, testBinned)
caret::confusionMatrix(pred, testing$Survived)


#lab4(Decision Trees - beyond C50)
set.wd("D:/SEM 2/ADM/practical/1")
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
save(titanicData, file="titanticData.RData")
set.seed(1337)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
library(RColorBrewer)
index <- sample(1:dim(titanicData)[1], dim(titanicData)[1] * .75, replace=FALSE)
training <- titanicData[index, ]
testing <- titanicData[-index, ]
regressionTree <- rpart(Survived ~ PassengerId+Pclass+Sex+Age+Parch+Fare+Embarked, data=training, method="class")
tree <- rpart(Survived ~ PassengerId+Pclass+Fare+Embarked+Sex+Age+Parch, data=training, method="class")
plot(regressionTree)
text(regressionTree)
plot(tree)
text(tree)
summary(regressionTree)
library(rattle)
#install.packages("rattle")
fancyRpartPlot(regressionTree)
Prediction <- predict(regressionTree, training, type = "class")
confusionMatrix(table(Prediction, training$Survived))
newRpart <- rpart(Survived ~ PassengerId+Pclass+Sex+Age+Parch+Fare+Embarked, data=training, method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(newRpart)
rpartPrediction <- predict(newRpart, training, type = "class")
confusionMatrix(table(rpartPrediction, training$Survived))
treeToPrune <- rpart(Survived ~ PassengerId+Pclass+Sex+Age+Parch+Fare+Embarked, data=training, method="class", control=rpart.control(minsplit=2, cp=0))
prunedTree <- prp(treeToPrune,snip=TRUE)$obj
fancyRpartPlot(prunedTree)
#Random Forest
install.packages("randomForest")
library(randomForest)
forest <- randomForest(Survived ~ PassengerId+Pclass+Sex+Parch+Fare+Age, data=training, importance=TRUE, ntree=2000)

varImpPlot(forest)
rf <- predict(forest, testing, type = "class")
#install.packages("caret")
training$Age[is.na(training$Age)] = 0
confusionMatrix(table(rf, testing$Survived))
#Conditional Inference Trees0
library(partykit)
cTree <- ctree(Survived ~ PassengerId+Pclass+Sex+Parch+Fare+Age+Embarked, data=training)
print(cTree)
plot(cTree, type="simple")
cForest <- cforest(Survived ~PassengerId+Pclass+Sex+Parch+Fare+Age+Embarked, data=training)

#Clustering(lab 5)
library(ggfortify) #for plots
df <- iris[c(1:4)] # removes the class label: feature 5
autoplot(prcomp(df, scale. = T, center = T), data = iris, colour = 'Species')
screeplot(prcomp(df, scale. = T, center = T), type="lines")
library(factoextra)
library(FactoMineR)
pca <- PCA(iris[, -5], scale.unit = T, ncp=3)
fviz_screeplot(pca, addlabels = TRUE)
fviz_pca_ind(pca, habillage = iris$Species, label="none", addEllipses = T)
#k-mean
table(iris$Species)
set.seed(1337)
myIris <- iris[, c(1:4)] #removing the dependent
myIris <- sapply(myIris, FUN=function(x) { scale(x, scale = T, center=T)})
res.1 <- kmeans(myIris, 3)
str(res.1)
df <- data.frame(cluster = res.1$cluster, species = iris$Species)
table (factor(df$cluster), df$species)
myIris <- iris[, c(1:4)] #removing the dependent
myIris <- sapply(myIris, FUN=function(x) { scale(x, scale = T, center=F)})
res.2 <- kmeans(myIris, 3)
df <- data.frame(cluster = res.2$cluster, species = iris$Species)
table (factor(df$cluster), df$species)
pca <- prcomp(iris[, c(1:4)], scale. = T, center = T)
res.3 <- kmeans(pca$x[, 1:2], 3)
df <- data.frame(cluster = res.3$cluster, species = iris$Species)
table (factor(df$cluster), df$species)
pca <- prcomp(iris[, c(1:4)], scale. = T, center = F)
res.4 <- kmeans(pca$x[, 1:2], 3)
df <- data.frame(cluster = res.4$cluster, species = iris$Species)
table (factor(df$cluster), df$species)
totss <- c(res.1[[3]], res.2[[3]], res.3[[3]], res.4[[3]])
tos.withinss <- c(res.1[[5]], res.2[[5]], res.3[[5]], res.4[[5]])
betweenss <- c(res.1[[6]], res.2[[6]], res.3[[6]], res.4[[6]])
performance <- data.frame(totss, tos.withinss, betweenss)
row.names(performance) <- c("w/o pca +c", "w/o pca", "w pca +c", "w pca")
performance
plot(iris[,c(1:4)], col=res.1$cluster)
plot(iris[,c(1:4)], col=res.4$cluster)
library(clusterSim) #for the dbindex
library(cluster) #for the silhouette
results <- list()
tot.withinss <- c()
betweenss <- c()
dbindex <- c()
silhouettes <- c()
for (k in 2:10) {
  results[[k]] <- kmeans(myIris, k)
  tot.withinss[k] <- results[[k]]$tot.withinss
  betweenss[k] <- results[[k]]$betweenss
  #dbindex[k] <- dbindex(myIris, results[[k]]$cluster, centrotypes="centroids")$DB
  s <- silhouette(results[[k]]$cluster, daisy(myIris))
  silhouettes[k] <- mean(s[,3])
}
par(mfrow=c(2,2))
plot(tot.withinss, xlab="k")
plot(betweenss, xlab="k")
plot(dbindex, xlab="k")
plot(silhouettes, xlab="k")
plot(silhouette(results[[3]]$cluster, daisy(myIris)))
library(fpc)
plotcluster(myIris, res.2$cluster)
res.2$centers #coords of centroids
centers <- res.2$centers[res.2$cluster, ] #vector of all centers for each point
distances <- sqrt(rowSums((myIris - centers)^2)) #Euclidean pair-wise distances
summary(distances)
sd(distances)
# pick n largest distances: boring!
outliers <- order(distances, decreasing=T)[1:5]
# pick those more than 2 standard deviations from the mean
outliers <- (distances > (mean(distances) + (2 * sd(distances))) |
               distances < (mean(distances) - (2 * sd(distances))))
print(myIris[outliers,])
# plot clusters (choose 2 dimensions for ease of display)
plot(myIris[,c("Sepal.Length", "Sepal.Width")], pch="o", col=res.2$cluster, cex=0.3)
# plot cluster centers
points(res.2$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)
# plot outliers
points(myIris[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)
#Hierarchical Agglomerative
d <- dist(myIris, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red")
#Density based clustering
ds <- dbscan(myIris, eps=.25, MinPts=5)
table(ds$cluster, iris$Species)
plot(ds, myIris)
plotcluster(myIris, ds$cluster)
results <- list()
d <- dist(myIris)
for (i in lowerBound:upperBound) {
  ds <- dbscan(myIris, eps=i/100, MinPts = 5)
  results[[i]] <- cluster.stats(d, ds$cluster)
  results[[i]]$eps <- i/100
}
#prune the NULLS
results[sapply(results, is.null)] <- NULL
#pick what you want to plot, e.g. average silhouette width
avg.silwidth <- lapply(results, FUN = function(x) {
  return (x$avg.silwidth)
})
eps <- lapply(results, FUN = function(x) {
  return (x$eps)
})
plot(y=avg.silwidth, x=eps, xlab="eps")
library(dbscan)
db <- dbscan(myIris, 0.4, 5)
hullplot(myIris, db$cluster)
#Model Based Clustering
library(mclust)
fit <- Mclust(myIris)
plot(fit, what = "classification") # plot results
summary(fit)
fit$classification


#Ensemble Learning(lab 6)
wine <- read.csv(
  url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),
  header=F)
names(wine) <- c(
  "Alcohol",
  "MalicAcid",
  "Ash",
  "Height",
  "Alcalinity",
  "Magnesium",
  "TotalPhenols",
  "Flavanoids",
  "NonflavanoidPhenols",
  "Proanthocyanins",
  "ColorIntensity",
  "Hue",
  "OD280OD315",
  "Proline"
)
wine$Alcohol <- factor(wine$Alcohol) #this should be categorical
barplot(table(wine$Alcohol))
boxplot(wine[, c(2:13)])
sapply(wine, FUN=function(x) {sum(is.na(x))})
#kNN
wineKNN <- wine
wineKNN[, c(2:14)] <- sapply(wineKNN[, 2:14],
                             FUN = function(x) { return ((x - min(x)) / (max(x) - min(x))) })

summary(wineKNN)
set.seed(1337)
library(caret)
index <- createDataPartition(wineKNN$Alcohol, p = .75, list = FALSE)
trainKNN <- wineKNN[index,]
validationKNN <- wineKNN[-index,]
library(randomForest)
training <- wine[index, ]
rf <- randomForest(Alcohol ~ ., data=training, importance=TRUE, ntree=100)
varImpPlotData <- varImpPlot(rf)
#grab the data for mean decrease accuracy
meanDecAcc <- varImpPlotData[, 1]
# order it descending (most important first)
meanDecAcc <- meanDecAcc[order(-meanDecAcc)]
#make a vector of numbers
a <- c(1:length(meanDecAcc))
# derive those that are odd
a <- a %% 2 == 1
# get the odd colNames
knnFeatures <- names(meanDecAcc[a])
knnFeatures
tuneParams <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final')
knn <- train(trainKNN[,knnFeatures],trainKNN$Alcohol,method='knn',trControl=tuneParams,tuneLength=10)
knn.pred <- predict(knn, newdata = validationKNN[,knnFeatures])
confusionMatrix(knn.pred, validationKNN$Alcohol)
library(C50)
train <- wine[index,]
validation <- wine[-index,]
c50Features <- names(meanDecAcc[!a])
c50Features
c50Tree <- train(train[,c50Features], train$Alcohol, method="C5.0", trControl=tuneParams, tuneLength=3)
c50.pred <- predict(c50Tree, newdata = validation[,c50Features])
confusionMatrix(c50.pred, validation$Alcohol)
#CART
#col 2 is GINI
meanDecGini <- varImpPlotData[, 2]
meanDecGini <- meanDecGini[order(-meanDecGini)]
b <- c(1:length(meanDecGini))
b <- b %% 2 == 1
cartFeatures <- names(meanDecGini[b])
cartFeatures
## [1] "Proline" "Flavanoids" "OD280OD315" "TotalPhenols"
## [5] "Alcalinity" "Magnesium" "Height"
rpartTree <- train(train[,cartFeatures], train$Alcohol, method="rpart", trControl=tuneParams, tuneLength=3)
rpart.pred <- predict(rpartTree, newdata = validation[,cartFeatures])
confusionMatrix(rpart.pred, validation$Alcohol)
#Instrumenting the voting
validation$pred_rpart_prob<-predict(object = rpartTree,validation[, cartFeatures],type='prob')
validation$pred_c50_prob<-predict(object = c50Tree,validation[,c50Features],type='prob')
validation$pred_knn_prob<-predict(object = knn,validationKNN[,knnFeatures],type='prob')
validation$pred_avg<-(validation$pred_rpart_prob+validation$pred_knn_prob+validation$pred_c50_prob)/3
validation$preds <- apply(validation$pred_avg, 1, FUN=function(x) {which.max(x)})
validation$preds <- factor(validation$preds, levels=c(1:3), labels=c(1:3))
confusionMatrix(validation$Alcohol, validation$preds)
dfPred <- cbind(validation$pred_rpart_prob, validation$pred_c50_prob, validation$pred_knn_prob)
validation$maxConfidence <- apply(dfPred, 1, FUN=function(x) {which.max(x)})
summary(validation$maxConfidence)
validation$maxConfidence[validation$maxConfidence > 6] <-
  validation$maxConfidence[validation$maxConfidence > 6] - 6
validation$maxConfidence[validation$maxConfidence > 3] <-
  validation$maxConfidence[validation$maxConfidence > 3] - 3
summary(validation$maxConfidence)
validation$maxConfidence <- factor(validation$maxConfidence, levels=c(1:3), labels=c(1:3))
confusionMatrix(validation$Alcohol, validation$maxConfidence)
train$pred_knn<-factor(knn$pred$pred[order(knn$pred$rowIndex)])
train$pred_c50<-factor(c50Tree$pred$pred[order(c50Tree$pred$rowIndex)])
train$pred_cart<-factor(rpartTree$pred$pred[order(rpartTree$pred$rowIndex)])
predictors <- c("pred_knn", "pred_c50", "pred_cart")
gbm <- train(train[, predictors],train$Alcohol,method='gbm',trControl=tuneParams,tuneLength=3)
validation$pred_knn <- factor(knn.pred)
validation$pred_c50 <- factor(c50.pred)
validation$pred_cart <- factor(rpart.pred)
gbm.pred <- predict(gbm, validation[, predictors])
confusionMatrix(validation$Alcohol, gbm.pred)
#Boosting
boosted <- train(train, train$Alcohol,method='ada',trControl=tuneParams,tuneLength=3)
#or
boosted <- train(train, train$Alcohol,method='adaboost',trControl=tuneParams,tuneLength=3)
#Appendix
knn <- train(trainKNN[,-1],trainKNN$Alcohol,method='knn',trControl=tuneParams,tuneLength=10)
knn.pred <- predict(knn, newdata = validationKNN[,-1])
confusionMatrix(knn.pred, validationKNN$Alcohol)
c50.pred <- predict(c50Tree, newdata = validation[,-1])
confusionMatrix(c50.pred, validation$Alcohol)

#Support Vector Machines(lab 7)










