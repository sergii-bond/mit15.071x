
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)
table(wiki$Vandal)

library(tm)
library(SnowballC)

#1) Create the corpus for the Added column, and call it "corpusAdded".
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusRemoved = Corpus(VectorSource(wiki$Removed))
#corpusAdded[[1]]

#2) Remove the English-language stopwords.
length(stopwords("english")) 
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

#3) Stem the words.
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusRemoved = tm_map(corpusRemoved, stemDocument)

#4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmRemoved = DocumentTermMatrix(corpusRemoved)

dtmAdded
dtmRemoved

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

sparseAdded
sparseRemoved

wordsAdded = as.data.frame(as.matrix(sparseAdded))
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

ncol(wordsAdded)
ncol(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved) 
ncol(wikiWords)

wikiWords$Vandal = wiki$Vandal
ncol(wikiWords)

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainWikiWords = subset(wikiWords, split == TRUE)
testWikiWords = subset(wikiWords, split == FALSE)

# Baseline accuracy
table(trainWikiWords$Vandal)
1443/nrow(trainWikiWords)


# Build CART model
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data=trainWikiWords, method="class")

prp(wikiCART)

predictWiki = predict(wikiCART, newdata=testWikiWords, type="class")
table(testWikiWords$Vandal, predictWiki)
(618+12)/(618+533+12)

#see how it predict on a training set
predictWiki_train = predict(wikiCART, newdata=trainWikiWords, type="class")
table(trainWikiWords$Vandal, predictWiki_train)

#We have stripped all punctuation so links to websites appear in the data as one word, 
#e.g. "httpwwwgooglecom". We hypothesize that given that a lot of vandalism seems 
#be adding links to promotional or irrelevant websites, the presence of a web address 
#is a sign of vandalism.

#We can search for the presence of a web address in the words added by searching for "http" in the Added column. The grepl function returns TRUE if a string is found in another string, e.g.

grepl("cat","dogs and cats",fixed=TRUE) # TRUE

grepl("cat","dogs and rats",fixed=TRUE) # FALSE

#Create a copy of your dataframe from the previous question:
  
wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
  
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

#Based on this new column, how many revisions added a link?
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

#Then create a new CART model using this new variable as one of the independent variables.
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART2)
(609+57)/(609+9+57+488)
#Another possibility is that the number of words added and removed is predictive, 
#perhaps more so than the actual words themselves. 
#We already have a word count available in the form of the document-term matrices (DTMs).

#Sum the rows of dtmAdded and dtmRemoved and add them as new variables 
#in your data frame wikiWords2 (called NumWordsAdded and NumWordsRemoved) 
#by using the following commands:
  
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

#What is the average number of words added?
mean(wikiWords2$NumWordsAdded)
mean(wikiWords2$NumWordsRemoved)

wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)

#Then create a new CART model using this new variable as one of the independent variables.
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
predictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predictCART3)
(514+248)/(514+104+297+248)

wikiWords3 = wikiWords2

#Then add the two original variables Minor and Loggedin to this new data frame:
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)

#Then create a new CART model using this new variable as one of the independent variables.
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictCART4)
(595+241)/(595+23+304+241)
prp(wikiCART4)

#===================================================================

trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
#The nchar() function counts the number of characters in a piece of text.
str(trials)

max(nchar(trials$abstract))
max(nchar(trials$title))
nrow(subset(trials, nchar(abstract) == 0))
trials$title[which.min(nchar(trials$title))]

trials$trial = as.factor(trials$trial)

#Because we have both title and abstract information for trials, 
#we need to build two corpora instead of one. Name them corpusTitle and corpusAbstract.
library(tm)
library(SnowballC)

#1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
                                                                  
#2) Convert corpusTitle and corpusAbstract to lowercase. 

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

#After performing this step, remember to run the lines:
                                                                  
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
                                                                  
#3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
                                                                  
#4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

#5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
                                                                  
#6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
                                                                  
#7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)
                                                                  
#8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))
                                                                  
#How many terms remain in dtmTitle after removing sparse terms 
#(aka how many columns does it have)?
sparseTitle
ncol(sparseAbstract)
ncol(dtmAbstract)

#What is the most frequent word stem across all the abstracts? 
#Hint: you can use colSums() to compute the frequency of a word across all the abstracts.

which.max(colSums(dtmAbstract))

#We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions.
#However, some of the variables in these data frames have the same names. 
#To fix this issue, run the following commands:
#(paste0 doesn't insert a space as paste does)  
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
dtmTrain = subset(dtm, split == TRUE)
dtmTest = subset(dtm, split == FALSE)

x = table(dtmTest$trial)

#accuracy of the basline model
max(x)/sum(x)

#build CART model
dtmCART = rpart(trial ~ ., data = dtmTrain, method="class")
prp(dtmCART)

#Obtain the training set predictions for the model (do not yet predict on the test set). 
#Extract the predicted probability of a result being a trial (recall that this involves 
#not setting a type argument, and keeping only the second column of the predict output). 
#What is the maximum predicted probability for any result?
predictCART = predict(dtmCART)
str(predictCART)
summary(predictCART)
ncol(predictCART)
max(predictCART[,2])

predictCART = predict(dtmCART, newdata = dtmTest)
max(predictCART[,2])

predictCART = predict(dtmCART, newdata = dtmTrain, type = "class")
x = as.matrix(table(dtmTrain$trial, predictCART))
#accuracy
(x[1,1] + x[2,2]) / sum(x)
(631+441)/(631+99+131+441)
#specificity
x[1,1] / (x[1,1] + x[1,2])

#sensitivity
x[2,2] / (x[2,1] + x[2,2])

predictCART = predict(dtmCART, newdata = dtmTest, type = "class")
x = as.matrix(table(dtmTest$trial, predictCART))
#accuracy
(x[1,1] + x[2,2]) / sum(x)


# ROC curve
library(ROCR)

PredictROC = predict(dtmCART, newdata = dtmTest)
pred = prediction(PredictROC[,2], dtmTest$trial)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

#====================================================================

emails = read.csv("emails.csv", stringsAsFactors = FALSE)
nrow(emails)
table(emails$spam )
str(emails)

#How many characters are in the longest email in the dataset 
#(where longest is measured in terms of the maximum number of characters)?

max(nchar(emails$text))

#Which row contains the shortest email in the dataset? 
#(Just like in the previous problem, shortest is measured in terms of the fewest number of characters.)
which.min(nchar(emails$text))

#Follow the standard steps to build and pre-process the corpus:
  
#1) Build a new corpus variable called corpus.
corpus = Corpus(VectorSource(emails$text))

#2) Using tm_map, convert the text to lowercase.
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

#3) Using tm_map, remove all punctuation from the corpus.
corpus = tm_map(corpus, removePunctuation)

#4) Using tm_map, remove all English stopwords from the corpus.
corpus= tm_map(corpus, removeWords, stopwords("english"))

#5) Using tm_map, stem the words in the corpus.
corpus = tm_map(corpus, stemDocument)

#6) Build a document term matrix from the corpus, called dtm.
dtm = DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm, 0.95) 
spdtm

#Build a data frame called emailsSparse from spdtm, and use the make.names function 
#to make the variable names of emailsSparse valid.
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

#colSums() is an R function that returns the sum of values for each variable in our 
#data frame. Our data frame contains the number of times each word stem (columns) 
#appeared in each email (rows). Therefore, colSums(emailsSparse) returns the number 
#of times a word stem appeared across all the emails in the dataset. 
#What is the word stem that shows up most frequently across all the emails in the dataset?
#Hint: think about how you can use sort() or which.max() to pick out the maximum frequency.
which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam
#How many word stems appear at least 5000 times in the ham emails in the dataset? 
#Hint: in this and the next question, remember not to count the dependent variable 
#we just added.
sum(as.integer(colSums(subset(emailsSparse, spam == 0)) >= 5000))

sum(as.integer(colSums(subset(emailsSparse, spam == 1)) >= 1000)) 
#and subtract this:
as.integer(sum(emailsSparse$spam) >= 1000)

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

spamLog = glm(spam ~ ., data=train, family="binomial")
spamCART = rpart(spam ~ ., data=train, method="class")
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train) 

probLog = predict(spamLog)
probCART = predict(spamCART)[,2]
probRF = predict(spamRF, type="prob")[,2]
#How many of the training set predicted probabilities from spamLog are less than 0.00001?
sum(as.integer(probLog < 0.00001))
    
#How many of the training set predicted probabilities from spamLog are more than 0.99999?
sum(as.integer(probLog > 0.99999))

#How many of the training set predicted probabilities from spamLog are between 
#0.00001 and 0.99999?
sum(as.integer(probLog < 0.00001 && probLog > 0.99999))

#grepl("***", summary(spamLog), fixed=TRUE)
summary(spamLog)

#How many of the word stems "enron", "hou", "vinc", and "kaminski" appear 
#in the CART tree? Recall that we suspect these word stems are specific 
#to Vincent Kaminski and might affect the generalizability of a spam filter 
#built with his ham data.
prp(spamCART)

x = as.matrix(table(train$spam, probLog > 0.5))
(x[1,1] + x[2,2]) / sum(x)

library(ROCR)

pred = prediction(probLog, train$spam)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

predictCART = predict(spamCART, type="class")
x = as.matrix(table(train$spam, predictCART))
(x[1,1] + x[2,2]) / sum(x)

pred = prediction(probCART, train$spam)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

predictRF = predict(spamRF, type="class")
x = as.matrix(table(train$spam, predictRF))
(x[1,1] + x[2,2]) / sum(x)

pred = prediction(probRF, train$spam)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

# Now determine testing accuracies
test_probLog = predict(spamLog, newdata=test)
test_probCART = predict(spamCART, newdata=test)[,2]
test_probRF = predict(spamRF, newdata=test, type="prob")[,2]

x = as.matrix(table(test$spam, test_probLog > 0.5))
(x[1,1] + x[2,2]) / sum(x)
pred = prediction(test_probLog, test$spam)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

x = as.matrix(table(test$spam, test_probCART > 0.5))
(x[1,1] + x[2,2]) / sum(x)
pred = prediction(test_probCART, test$spam)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

x = as.matrix(table(test$spam, test_probRF > 0.5))
(x[1,1] + x[2,2]) / sum(x)
pred = prediction(test_probRF, test$spam)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)
