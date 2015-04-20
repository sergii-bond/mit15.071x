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
install.packages("Rcpp")
library("Rcpp")
install.packages("RcppEigen")
library("RcppEigen")
install.packages("caret")
library("caret")
