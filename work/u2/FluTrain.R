
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)

#DateConvert = as.Date(strptime(FluTrain$Week, "%y-%m-%d"))

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
i = which(FluTest$Week == '2012-03-11 - 2012-03-17')
x = PredTest1[i]
1 - x/FluTest$ILI[i]
SSE = sum((PredTest1 - FluTest$ILI)^2)
sqrt(SSE/nrow(FluTest))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

ILILag2_test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2_test)
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1] 
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)] 

PredTest2 = exp(predict(FluTrend2, FluTest))
sqrt(mean((PredTest2 - FluTest$ILI)^2))

#In this problem, we used a simple time series model with a single lag term. 
#ARIMA models are a more general form of the model we built, which can include 
#multiple lag terms as well as more complicated combinations of previous values 
#of the dependent variable. If you're interested in learning more, check out ?arima 
#or the available online tutorials for these sorts of models.