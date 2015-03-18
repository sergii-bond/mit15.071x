mvt = read.csv("/home/sergius/work/mit15.071x/data/mvtWeek1.csv")

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
boxplot(mvt$Date ~ mvt$Arrest)

# get year from Date object
format(mvt$Date[5], "%Y")

table(format(mvt$Date, "%Y"), mvt$Arrest)

sort(table(mvt$LocationDescription))
top5 = subset(mvt, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL")

# get rid of 0 categories
top5$LocationDescription = factor(top5$LocationDescription)

setwd("/home/sergius/work/mit15.071x/data/")
IBM = read.csv("IBMStock.csv")
Boeing = read.csv("BoeingStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")

#for (i in c(IBM, Boeing, ProcterGamble, GE, CocaCola)) { 
#  names(i) 
#}

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

# plot line
plot(CocaCola$Date, CocaCola$StockPrice, col="green", type='l')

# vertical line
abline(v=as.Date("2000-03-01"), lwd=2)

# limit the y axis in the plot
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type='l',
     col='red', ylim=c(0,210), ylab = 'Stock Price', main='CocaCola (red), P&G (blue),
     GE (purple), Boeing (orange), IBM(green)')

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type='l',
      col='blue')

lines(GE$Date[301:432], GE$StockPrice[301:432], 
      type='l', col='purple')

lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], 
      type='l', col='orange')

lines(IBM$Date[301:432], IBM$StockPrice[301:432], 
      type='l', col='green')

setwd("~/work/mit15.071x/data/CPS")
CPS = read.csv("CPSData.csv") 

# returns a vector of TRUE/FALSE, criteria = NA
is.na(CPS$Married)

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

# left outer join
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", 
            all.x=TRUE)

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
