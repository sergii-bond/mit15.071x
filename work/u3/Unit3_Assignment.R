#Task 1
songs = read.csv("songs.csv")
str(songs)
nrow(subset(songs, year == 2010))
nrow(subset(songs, artistname == 'Michael Jackson'))
x = subset(songs, artistname == 'Michael Jackson' & Top10 == 1)
nrow(x)
x[c("songtitle")]
table(songs$timesignature)
songs[which.max(songs$tempo), c("songtitle")]
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
nrow(SongsTrain)
nrow(SongsTest)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
model1 = glm(Top10~ ., data=SongsTrain, family=binomial)
summary(model1)
cor(SongsTrain$loudness, SongsTrain$energy)

#can subtract numerical variables only
model2 = glm(Top10~ . -loudness, data=SongsTrain, family=binomial)
summary(model2)

model3 = glm(Top10~ . -energy, data=SongsTrain, family=binomial)
summary(model3)

pred3 = predict(model3, newdata=SongsTest, type="response")
table(SongsTest$Top10, pred3 > 0.45)
#accuracy
(309+19)/(309+19+5+40)
#sensitivity
19/(40+19)
#specificity
309/(309+5)

table(SongsTest$Top10)
314/(314+59)


#Task 2

parole = read.csv("parole.csv")
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
install.packages("caTools")
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1 = glm(violator ~ ., data=train, family='binomial')
#Consider a parolee who is male, of white race, aged 50 years 
#at prison release, from the state of Maryland, served 3 months, 
#had a maximum sentence of 12 months, did not commit multiple offenses, 
#and committed a larceny.
#According to the model, what are the odds this individual is a violator?
odds = exp(-4.24115+0.8867*1-0.0001756*50+0.3869*1-0.1238*3+0.08*12+0.6837*1)
#According to the model, what is the probability this individual is a violator?
p = odds/(1+odds)

pred = predict(model1, newdata=test, type = "response")
table(test$violator, pred > 0.5)
12/(11+12)
167/(167+12)
(167+12)/(167+12+12+11)

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function
ROCRpred = prediction(pred, test$violator)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# Plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

#Test3
loans = read.csv("loans.csv")

loans_na = subset(loans, is.na(pub.rec) | is.na(delinq.2yrs) | 
                    is.na(inq.last.6mths) | is.na(revol.util) |
                    is.na(days.with.cr.line) | is.na(log.annual.inc))

install.packages("mice")
library(mice)

set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
#Note that to do this imputation, we set vars.for.imputation to all 
#variables in the data frame except for not.fully.paid, to impute the values 
#using all of the other independent variables.

loans = read.csv("loans_imputed.csv")

set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
model1 = glm(not.fully.paid ~ ., data=train, family="binomial")
predicted.risk = predict(model1, newdata=test, type="response")
