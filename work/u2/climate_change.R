
x = read.csv("climate_change.csv")
climTrain = subset(x, Year <= 2006)
climTest= subset(x, Year > 2006)

climReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climTrain)
summary(climReg)

climReg_2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climTrain)
summary(climReg_2)

#We have many variables in this problem, and as we have seen above, 
#dropping some from the model does not decrease model quality. 
#R provides a function, step, that will automate the procedure of trying 
#different combinations of variables to find a good compromise of model simplicity and R2. 
#This trade-off is formalized by the Akaike information criterion (AIC) - it can be informally 
#thought of as the quality of the model with a penalty for the number of variables in the model.
#(http://en.wikipedia.org/wiki/Akaike_information_criterion)

climReg_AIC = step(climReg)
summary(climReg_AIC)

climP = predict(climReg_AIC, climTrain)
SSE = sum((climP-climTrain$Temp)^2)
SST = sum((climTrain$Temp-mean(climTrain$Temp))^2)
1 - SSE/SST

climP = predict(climReg_AIC, newdata = climTest)
SSE = sum((climP-climTest$Temp)^2)
SST = sum((climTest$Temp-mean(climTrain$Temp))^2)
1 - SSE/SST
