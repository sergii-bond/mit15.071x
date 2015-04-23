# View variables in the current environment
ls()

# Create a vector with data type 'number'
x = c(1, 2, 3)
x

# Create a vector with data type 'string'
y = c("A", "B", "C")
y

# Note that data types can't be combined together

# Create a sequence with step 2
seq(0, 10, 2)

# Combine two vectors
d = data.frame(y, x)
d

# Add a column to the data frame
d$z = c("Yes", "No", "Maybe")
d

u = c(4,5,6)

# Add a new column in a different way
d = cbind(d, u)
d

# Create another data frame
d2 = data.frame(y=c("D", "E"), x=c(8.1,5.2), z=c("Ha", "Ha"), u=c(9, 4))
d2

# Combine two data frames
d3 = rbind(d, d2)
d3

# Change the working directory
setwd("~/work/mit15.071x/data")

# Check the current directory
getwd()
WHO = read.csv("WHO.csv")

# See the structure of the data
str(WHO)

# See numerical summary
summary(WHO)

# Create a subset of data
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)

# Save data to csv
write.csv(WHO_Europe, "WHO_Europe.csv")

# Remove the dataset from the env
ls()
rm(WHO_Europe)

# Get the column from a data frame as a vector
WHO$Under15

# Calculate the mean and standard deviation of the vector
mean(WHO$Under15)
sd(WHO$Under)

# Get the numerical summary of the vector
summary(WHO$Under15)

# Find who has the minimum value
x = which.min(WHO$Under15)
WHO$Country[x]
WHO$Country[which.max(WHO$Under15)]

# Scatter plot
plot(WHO$GNI, WHO$FertilityRate)

# Define a subset of outliers
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)

# See how many rows
nrow(Outliers)

# See the information of interest only
Outliers[c("Country", "GNI", "FertilityRate")]

hist(WHO$CellularSubscribers)

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy", 
        main = "Life Expectancy of countries by region")

table(WHO$Region)

tapply(WHO$Over60, WHO$Region, mean)

tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)

tapply(WHO$ChildMortality, WHO$Region, mean)

x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
y = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = T)
y

x = rnorm(5)
x

y = rnorm(5, mean = 1, sd = 2)
y

cor(x, y)

x = rnorm(1000, mean = 3, sd = 2)
E_X_est = mean(x)
VAR_X_est = mean((x - E_X_est)^2)
hist(x)

x = rnorm(100)
y = rnorm(100)
plot(x, y, xlab = "X", ylab = "Y")

# plot to file
pdf("fig.pdf")
plot(x, y, col = "green")
dev.off()

jpeg("fig.jpeg")
hist(x)
dev.off()

#equaly spaced points
x = seq(-pi, pi, length = 50)

#countour plot
y = x
# outer product of arrays, creates a matrix
f = outer(x, y, function(x,y) cos(y) / (1 + x^2))
contour(x, y, f)
# 'add' - if true, add to current plot
contour(x, y, f, nlevels = 45, add = T)
# t() - matrix transpose
fa = (f - t(f)) / 2
contour(x, y, fa, nlevels = 45)

# heatmap
image(x, y, fa)

# 3D plot
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

A = matrix(1:16, 4, 4)
A[2, 3]
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[ , 1:2]
A[1, ]
#exclude rows
A[-c(1, 3), ]
dim(A)

b = read.csv("test.csv")
fix(b)

# remove empty rows
b = na.omit(b)
fix(b)

# see column names
names(b)

# let colum names be available in the current namespace
attach(b)
plot(col1, col2)

# convert a variables from quantitative into qualitative 
col1 = as.factor(col1)

plot(col1, col2)

# scatterplot matrix
pairs(b)
pairs(~ col1 + col2 + col3, b)

# identify the value of a point in the plot
plot(col2, col3)
identify(col2, col3, col1)

x = rnorm(10)
y = rnorm(10)
plot(x, y)
identify(x, y, x)

# savehistory()
# loadhistory()

wd = getwd()
setwd(paste(wd, "/stat", sep = ""))
