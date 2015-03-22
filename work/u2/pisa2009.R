
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

SSE = sum(residuals(lmScore)^2)
SST = sum((pisaTrain$readingScore - mean(pisaTrain$readingScore))^2)
1 - SSE/SST
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

predTest = predict(lmScore, newdata = pisaTest)
SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE
m = mean(pisaTrain$readingScore)
m
SST = sum((pisaTest$readingScore - m)^2)
SST
1 - SSE/SST
