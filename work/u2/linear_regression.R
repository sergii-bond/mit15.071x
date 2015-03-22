wine = read.csv("wine.csv")
str(wine)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)
model1$residuals
SSE = sum((model1$residuals)^2)
SSE
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2 )
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

model4 = lm(Price ~ HarvestRain + WinterRain + Age + AGST, data = wine)
summary(model4)


x = wine$FrancePop
y = wine$Price
mean((x - mean(x))*(y-mean(y)))/sd(x)/sd(y)
cor(x, y)

cor(wine)

wineTest = read.csv("wine_test.csv")
  
predictTest = predict(model4, newdata=wineTest)
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST



