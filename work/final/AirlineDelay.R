Airlines = read.csv("AirlineDelay.csv")
set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]

model = lm(TotalDelay ~., AirlinesTrain)
predictions = predict(model, AirlinesTrain)
SSE = sum((predictions - AirlinesTrain$TotalDelay)^2)
SST = sum((AirlinesTrain$TotalDelay - mean(AirlinesTrain$TotalDelay))^2)
1 - SSE/SST

predictions = predict(model, newdata = AirlinesTest)
SSE = sum((predictions - AirlinesTest$TotalDelay)^2)
SST = sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2)
1 - SSE/SST

Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, 
                                    "No Delay", ifelse(Airlines$TotalDelay >= 30, 
                                                       "Major Delay", "Minor Delay")))
Airlines$TotalDelay = NULL
#only for categorical data
library(caTools)
set.seed(15071)
split = sample.split(Airlines$DelayClass, SplitRatio = 0.7)
AirlinesTrain = subset(Airlines, split == TRUE)
AirlinesTest = subset(Airlines, split == FALSE)

#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
tree_model = rpart(DelayClass ~., data = AirlinesTrain, method = "class")
prp(tree_model)

pred_train = predict(tree_model, AirlinesTrain, type = "class")
table(AirlinesTrain$DelayClass, pred_train)
(361+3094)/(314+804+361+1806+188+3094)

#baseline
(188+3094)/(314+804+361+1806+188+3094)

pred = predict(tree_model, newdata = AirlinesTest, type = "class")
table(AirlinesTest$DelayClass, pred)
(153+1301)/(141+338+153+776+105+1301)
