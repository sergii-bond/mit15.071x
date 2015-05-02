# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.

#The dependent variable in this problem is the variable Popular, 
#which labels if an article had 25 or more comments in its online 
#comment section (equal to 1 if it did, and 0 if it did not). 
#The dependent variable is provided in the training data set, 
#but not the testing dataset. 
#This is an important difference from what you are used to - 
#you will not be able to see how well your model does on the test set 
#until you make a submission on Kaggle.

#The independent variables consist of 8 pieces of article data available 
#at the time of publication, and a unique identifier:
  
#NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
#SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
#SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
#Headline = the title of the article
#Snippet = a small portion of the article text
#Abstract = a summary of the blog article, written by the New York Times
#WordCount = the number of words in the article
#PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
#UniqueID = a unique identifier for each article



# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer 

# We are adding in the argument stringsAsFactors=FALSE, since we have some text fields

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsValid = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

install.packages("ggplot2")
library(ggplot2)

scatterplot = ggplot(NewsTrain, aes(x = WordCount, y = Popular))
scatterplot + geom_point()
boxplot(NewsTrain$Popular, NewsTrain$WordCount)
pop = subset(NewsTrain, Popular == 1)
non_pop = subset(NewsTrain, Popular == 0)
summary(pop$WordCount)
summary(non_pop$WordCount)

df_list <- list(train = NewsTrain, valid = NewsValid)
df_list <- lapply(df_list, function(df) {
  df$NewsDesk = as.factor(df$NewsDesk)  
  df$SectionName = as.factor(df$SectionName)  
  df$SubsectionName = as.factor(df$SubsectionName)  
  df$PubDate = strptime(df$PubDate, "%Y-%m-%d %H:%M:%S")
  df$Weekday = df$PubDate$wday
  rownames(df) = df$UniqueID
  df
})



# We will just create a simple logistic regression model, to predict Popular using WordCount:

###SimpleMod = glm(Popular ~ WordCount, data=NewsTrain, family=binomial)

# And then make predictions on the test set:

###PredTest = predict(SimpleMod, newdata=NewsTest, type="response")

# We can't compute the accuracy or AUC on the test set ourselves, 
#since we don't have the dependent variable on the test set 
#(you can compute it on the training set though!). 
# However, you can submit the file on Kaggle to see how well the model performs. 
#You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

####MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

####write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!

# One more helpful hint:
# This dataset has a date/time field (PubDate). You might remember dealing with date and time data in some of the Unit 1 homework problems. 
# In this dataset, the following commands might be useful to you when trying to get date and time variables.

# To convert the date/time to something R will understand, you can use the following commands:

#NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
#NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

# The second argument tells the strptime function how the data is formatted. 
# If you opened the file in Excel or another spreadsheet software before loading it into R, you might have to adjust the format. 
# See the help page ?strptime for more information.

# Now that R understands this field, there are many different attributes of the date and time that you can extract.
# For example, you can add a variable to your datasets called "Weekday" that contains the day of the week 
#that the article was published (0 = Sunday, 1 = Monday, etc.), by using the following commands:

#NewsTrain$Weekday = NewsTrain$PubDate$wday
#NewsTest$Weekday = NewsTest$PubDate$wday

# Weekday could now be used as an independent variable in your predictive models.

# For more fields that you can extract from a date/time object in R, see the help page ?POSIXlt

#tf = tf_frame(c(df_list$train$Abstract, df_list$train$Snippet), 0.995)
tf = tf_frame(df_list$train$Abstract, 0.995)
#tf = tf_frame(df_list$train$Snippet, 0.995)
rownames(tf) = rownames(df_list$train)

library(caTools)
#set.seed(123)
split = sample.split(df_list$train$Popular, SplitRatio = 0.7)
tf_train = subset(tf, split == TRUE)
tf_test = subset(tf, split == FALSE)
news_train = subset(df_list$train, split == TRUE)
news_test = subset(df_list$train, split == FALSE)

library(flexclust)
k = 5
#tf_kmeans = kmeans(tf_train, centers = k) #, iter.max = 1000)
km.kcca = kcca(tf_train, k, family = kccaFamily("ejaccard"), 
     control = list(initcent = "kmeanspp"))
# predict clusters on test data
#km.kcca = as.kcca(tf_kmeans, tf_train)
#returns a vector of cluster numbers
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=tf_test)
#table(clusterTest)

#news_train = merge(tf_train, 
#                   data.frame(row.names = rownames(df_list$train), 
#                              Popular = df_list$train$Popular), 
#                   by = 0)
#rownames(news_train) = news_train$Row.names
#news_train$Row.names = NULL

#news_test = merge(tf_test, 
#                   data.frame(row.names = rownames(df_list$train), 
#                              Popular = df_list$train$Popular), 
#                   by = 0)
#rownames(news_test) = news_test$Row.names
#news_test$Row.names = NULL

#news_train_cl = split_df_by_clusters(news_train, tf_kmeans$cluster)
#news_test_cl = split_df_by_clusters(news_test, clusterTest)

#news_train_cl = split_df_by_clusters(news_train, tf_kmeans$cluster)
news_train_cl = split_df_by_clusters(news_train, clusterTrain)
news_test_cl = split_df_by_clusters(news_test, clusterTest)

#for (i in 1:length(tf_train_cl)) {
#  cat(i, ":", nrow(tf_train_cl[[i]]), "\n")
#  print(tail(sort(colMeans(tf_train_cl[[i]]))))
#}

#lapply(tf_cl_kmeans,nrow)
#which.max(sapply(tf_cl_kmeans,nrow))
#which.min(sapply(tf_cl_kmeans,nrow))



library(rpart)
library(rpart.plot)
library(randomForest)
install.packages("lme4")
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10 )
cpGrid = expand.grid(.cp = seq(0.001,0.05,0.001)) 

# Perform the cross validation

newsModel <- list()

for (i in 1:k) {
  best = train(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName, 
               data = news_train_cl[[i]], method = "rpart", 
               trControl = numFolds, tuneGrid = cpGrid)
  #newsModel[[i]] = glm(Popular~ ., data = news_train_cl[[i]], family = "binomial") 
  news_train_cl[[i]]$Popular = as.factor(news_train_cl[[i]]$Popular)
  #newsModel[[i]] = rpart(Popular~ ., data = news_train_cl[[i]], method = "class") 
  newsModel[[i]] = rpart(Popular~ WordCount + Weekday + NewsDesk + SectionName + 
                           SubsectionName, 
                         data = news_train_cl[[i]], method = "class", cp = best$bestTune[[1]]) 
  #newsModel[[i]] = randomForest(Popular~ ., data = news_train_cl[[i]]) 
  
}
  
newsModel_predict_test <-list()
newsModel_predict_test_class <-list()
combined <- data.frame(UniqueID = c(), Probability1 = c())

for (i in 1:k) {
  #newsModel_predict_test[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]], type = "response")
  newsModel_predict_test_class[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]], 
                                              type = "class")
  newsModel_predict_test[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]])
  
  combined = rbind(combined, 
                   data.frame(UniqueID = as.numeric(as.character(rownames(newsModel_predict_test[[i]]))),
                              Probability1 = newsModel_predict_test[[i]][,2]))
  #x = as.matrix(table(news_test_cl[[i]]$Popular, newsModel_predict_test[[i]] > 0.5))
  acc = access_accuracy(news_test_cl[[i]]$Popular, 
                        newsModel_predict_test_class[[i]],
                        newsModel_predict_test[[i]][,2])
                        #newsModel_predict_test[[i]])
  cat("Cluster ", i, ": baseline accuracy =", acc[1], ", test accuracy =", acc[2], "AUC =", acc[3], "\n")
}

#sort
#combined = combined[order(as.numeric(as.character(combined$UniqueID))), ]
combined = combined[order(combined$UniqueID), ]
acc = access_accuracy(news_test$Popular, 
                      combined$Probability1 > 0.5,
                      combined$Probability1)
cat("Overall: baseline accuracy =", acc[1], ", test accuracy =", acc[2], "AUC =", acc[3], "\n")
#prp(tweetCART)