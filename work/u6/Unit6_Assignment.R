kos = read.csv("dailykos.csv")

distances = dist(kos, method = "euclidean")
clusterKos= hclust(distances, method = "ward.D") 
plot(clusterKos)

clusterGroups = cutree(clusterKos, k = 7)

#http://cran.r-project.org/doc/manuals/r-release/R-intro.html#Lists-and-data-frames
kos_cl <- list()
for (i in 1:7) {
  kos_cl[[i]] = subset(kos, clusterGroups == i)
}

for (i in 1:length(kos_cl)) {
  cat(i, ":", nrow(kos_cl[[i]]), "\n")
  
  #This computes the mean frequency values of each of the words in cluster 1, 
  #and then outputs the 6 words that occur the most frequently.
  print(tail(sort(colMeans(kos_cl[[i]]))))
}


set.seed(1000)
k = 7
kos_kmeans = kmeans(kos, centers = k) #, iter.max = 1000)

kos_cl_kmeans <- list()
for (i in 1:7) {
  kos_cl_kmeans[[i]] = subset(kos, kos_kmeans$cluster == i)
}

for (i in 1:length(kos_cl_kmeans)) {
  cat(i, ":", nrow(kos_cl_kmeans[[i]]), "\n")
  print(tail(sort(colMeans(kos_cl_kmeans[[i]]))))
}

lapply(kos_cl_kmeans,nrow)
which.max(sapply(kos_cl_kmeans,nrow))
which.min(sapply(kos_cl_kmeans,nrow))


#======================================

#Balance = number of miles eligible for award travel
#QualMiles = number of miles qualifying for TopFlight status
#BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
#BonusTrans = number of non-flight bonus transactions in the past 12 months
#FlightMiles = number of flight miles in the past 12 months
#FlightTrans = number of flight transactions in the past 12 months
#DaysSinceEnroll = number of days since enrolled in the frequent flyer program

airlines = read.csv("AirlinesCluster.csv") 

#normalization

#install.packages("Rcpp")
#library("Rcpp")
#install.packages("RcppEigen")
#library("RcppEigen")
install.packages("caret")
library("caret")

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

distances = dist(airlinesNorm, method = "euclidean")
clusterAirlines= hclust(distances, method = "ward.D") 
plot(clusterAirlines)

clusterGroups = cutree(clusterAirlines, k = 5)

air_cl <- list()
for (i in 1:5) {
  air_cl[[i]] = subset(airlinesNorm, clusterGroups == i)
  cat(i, "# of obs: ", nrow(air_cl[[i]]), "\n")
}

for (i in 1:length(airlines)) {
  cat(colnames(airlines)[i], " ", which.max(tapply(airlines[[i]], clusterGroups, mean)), "\n")
}
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

set.seed(88)
k = 5
air_kmeans = kmeans(airlinesNorm, centers = k, iter.max = 1000)

air_kmeans_cl <- list()
for (i in 1:k) {
  air_kmeans_cl[[i]] = subset(airlinesNorm, air_kmeans$cluster == i)
  cat(i, "# of obs: ", nrow(air_kmeans_cl[[i]]), "\n")
}

#================================================================

stock = read.csv("StocksCluster.csv")
nrow(stock)
table(stock$PositiveDec)
cor(stock)
colMeans(stock)
which.max(colMeans(stock))

install.packages("caTools")
library(caTools)
set.seed(144)
spl = sample.split(stock$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stock, spl == TRUE)
stocksTest = subset(stock, spl == FALSE)

stock_log_reg = glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
stock_log_reg_pred_train = predict(stock_log_reg, type = "response")
table(stocksTrain$PositiveDec, stock_log_reg_pred_train > 0.5)

stock_log_reg_pred_test = predict(stock_log_reg, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, stock_log_reg_pred_test > 0.5)
table(stocksTest$PositiveDec)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain)
summary(normTest)

set.seed(144)
k = 3
km = kmeans(normTrain, centers = k, iter.max = 1000)

km_cl <- list()
for (i in 1:k) {
  km_cl[[i]] = subset(normTrain, km$cluster == i)
  cat(i, "# of obs: ", nrow(km_cl[[i]]), "\n")
}

install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain_cl <- list()
stocksTest_cl <- list()
for (i in 1:k) {
  stocksTrain_cl[[i]] = subset(stocksTrain, km$cluster == i)
  stocksTest_cl[[i]] = subset(stocksTest, clusterTest == i)
  cat(i, "avg of dependent var in a training set: ", mean(stocksTrain_cl[[i]]$PositiveDec), "\n")
}

stocksModel <- list()
stockModel_predict_test <-list()
for (i in 1:k) {
  stocksModel[[i]] = glm(PositiveDec ~ ., data = stocksTrain_cl[[i]], family = "binomial") 
  stockModel_predict_test[[i]] = predict(stocksModel[[i]], newdata = stocksTest_cl[[i]], type = "response")
  x = as.matrix(table(stocksTest_cl[[i]]$PositiveDec, stockModel_predict_test[[i]] > 0.5))
  accuracy = (x[1,1] + x[2,2])/nrow(stocksTest_cl[[i]])
  cat("Cluster ", i, ": test accuracy = ", accuracy, "\n")
}

for (i in 1:k) {
  summary(stocksModel[[i]])$coefficients[,1]
}