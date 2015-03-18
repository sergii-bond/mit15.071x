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
