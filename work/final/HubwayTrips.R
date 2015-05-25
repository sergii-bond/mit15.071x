
htrips = read.csv("HubwayTrips.csv")
nrow(htrips)
library(caret)
preproc = preProcess(htrips)
htrips_norm = predict(preproc, htrips)

set.seed(8000)

k = 20 
htrips_kmeans = kmeans(htrips_norm, centers = k) #, iter.max = 1000)

htrips_cl <- list()
for (i in 1:k) {
  htrips_cl[[i]] = subset(htrips, htrips_kmeans$cluster == i)
}

for (i in 1:length(htrips_cl)) {
  if (mean(htrips_cl[[i]]$Duration) < mean(htrips$Duration) && 
       mean(htrips_cl[[i]]$Weekday) > 0.4 && 
        mean(htrips_cl[[i]]$Evening) > 0.4) {
    cat(i, ":", nrow(htrips_cl[[i]]), "\n")
    print(tail(sort(colMeans(htrips_cl[[i]]))))
  }
}

library(ggplot2)

#p1 = ggplot(htrips, aes(x = htrips_kmeans$cluster, y = Age))
p1 = ggplot(htrips, aes(y = htrips_kmeans$cluster, x = Age))
#p1 = ggplot(x = htrips_kmeans$cluster, y = htrips$Age)
#p1 + geom_histogram()
p1 + geom_point()

boxplot(htrips$Age ~ as.factor(htrips_kmeans$cluster))
