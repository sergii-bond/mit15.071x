usda = read.csv('/home/sergius/work/mit15.071x/data/USDA.csv')
names(usda)
match("CAVIAR", usda$Description)
sd(usda$Sodium, na.rm=TRUE)
plot(usda$Protein, usda$TotalFat, xlab="Protein",
     ylab="Fat", main="Protein vs. Fat", col="red")

hist(usda$VitaminC, xlab='Vitamin C', 
     main = "Histogram of vitamin C levels", 
     xlim = c(0, 100), breaks = 2000)

boxplot(usda$Sugar, ylab='Sugar, g')

hsod = as.numeric(usda$Sodium > mean(usda$Sodium, na.rm=TRUE))
usda$hsod = as.numeric(usda$Sodium > mean(usda$Sodium, na.rm=TRUE))

table(usda$hsod, usda$HighFat)
tapply(usda$Iron, usda$HighProtein, mean, na.rm=TRUE)
