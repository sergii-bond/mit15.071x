baseball = read.csv("baseball.csv")
moneyball = subset(baseball, Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA

plot(moneyball$RD, moneyball$W)

WinsReq = lm(W ~ RD, data = moneyball)

RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
RunsReg = lm(RS ~ OBP + SLG , data=moneyball)
predict(RunsReg, data.frame(OBP=0.311, SLG=0.405))
AllowReg = lm(RA ~ OOBP + OSLG, data=moneyball)
predict(AllowReg, data.frame(OOBP=0.297, OSLG=0.370))

d = data.frame(name="Eric Chavez", OBP=0.338, SLG=0.540, Salary=1400000)
d = rbind(d, data.frame(name = "Jeremy Giambi", OBP = 0.391, SLG = 0.450, Salary = 1065000))
d = rbind(d, data.frame(name = "Frank Menechino", OBP = 0.369, SLG = 0.374, Salary = 295000))
d = rbind(d, data.frame(name = "Greg Myers", 	OBP = 0.313, SLG = 0.447, Salary = 800000 ))
d = rbind(d, data.frame(name = "Carlos Pena", OBP = 0.361, SLG = 0.500, Salary = 300000 ))

