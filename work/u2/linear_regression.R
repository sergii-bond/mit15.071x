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

model4 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)
