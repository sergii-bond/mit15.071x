eBay = read.csv("ebay.csv", stringsAsFactors=FALSE)
eBay$sold = as.factor(eBay$sold)
eBay$condition = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color = as.factor(eBay$color)
eBay$material = as.factor(eBay$material)

set.seed(144)
library(caTools)
spl = sample.split(eBay$sold, 0.7)
training = subset(eBay, spl == TRUE)
testing = subset(eBay, spl == FALSE)

model = glm(sold ~ biddable + startprice + condition + heel + style +
              color + material, data = training, family = binomial)

pred = predict(model, newdata = testing, type = "response")
table(testing$sold, pred > 0.5)

#install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(pred, testing$sold)
#pred_train = predict(model, type = "response")
#ROCRpred = prediction(pred_train, training$sold)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred, "auc")@y.values)

plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

predTrain = predict(model, type = "response")
ROCRpred = prediction(predTrain, training$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

set.seed(144)
library(caret)
install.packages("e1071")
library("e1071")

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 

# Perform the cross validation
train(sold ~ biddable + startprice + condition + heel + style +
              color + material, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

# Create a new CART model
model_cart = rpart(sold ~ biddable + startprice + condition + heel + style +
              color + material, data = training, method = "class", cp = 0.007)
install.packages("rpart.plot")
library("rpart.plot")
prp(model_cart)
# Make predictions
predict_cart = predict(model_cart, newdata = testing, type = "class")

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus
 
corpus = Corpus(VectorSource(eBay$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

#inspect(dtm[1000:1005,505:515])

# Check for sparsity
#findFreqTerms(dtm, lowfreq=20)

# Remove sparse terms
# Keep only those terms that exist in 10% of the tweets
spdtm = removeSparseTerms(dtm, 0.9)
descriptionText = as.data.frame(as.matrix(spdtm))

# Make all variable names R-friendly
colnames(descriptionText) = make.names(colnames(descriptionText))

names(descriptionText) = paste0("D", names(descriptionText)) 
descriptionText$sold = eBay$sold
descriptionText$biddable = eBay$biddable
descriptionText$startprice = eBay$startprice
descriptionText$condition = eBay$condition
descriptionText$heel = eBay$heel
descriptionText$style = eBay$style
descriptionText$color = eBay$color
descriptionText$material = eBay$material

trainText = subset(descriptionText, spl == TRUE)
testText = subset(descriptionText, spl == FALSE)

glmText = glm(sold ~ ., data = trainText, family = binomial)

predText = predict(glmText, type = "response")
ROCRpred = prediction(predText, trainText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)

predText = predict(glmText, newdata = testText, type = "response")
ROCRpred = prediction(predText, testText$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)