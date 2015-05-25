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

library(rpart)
library(rpart.plot)
library(randomForest)
library(flexclust) 
library("caret")

#install.packages("ggplot2")
#library(ggplot2)

#scatterplot = ggplot(NewsTrain, aes(x = WordCount, y = Popular))
#scatterplot + geom_point()
#boxplot(NewsTrain$Popular, NewsTrain$WordCount)
#pop = subset(NewsTrain, Popular == 1)
#non_pop = subset(NewsTrain, Popular == 0)
#summary(pop$WordCount)
#summary(non_pop$WordCount)
install.packages("babynames")
library("babynames")


# Transform input data
df_list <- list(train = NewsTrain, valid = NewsValid)
df_list <- lapply(df_list, function(df) {
  df$NewsDesk = as.factor(df$NewsDesk)  
  df$SectionName = as.factor(df$SectionName)  
  df$SubsectionName = as.factor(df$SubsectionName)  
  df$PubDate = strptime(df$PubDate, "%Y-%m-%d %H:%M:%S")
  df$Weekday = df$PubDate$wday
  df$Month = df$PubDate$mon
  df$Hour = df$PubDate$hour
  df$HeadlineLength = nchar(df$Headline)
  df$AbstractLength = nchar(df$Abstract)
  rownames(df) = df$UniqueID
  df$PubDate = NULL 
  #df$Popular = as.factor(df$Popular)
  df
})

#names = babynames
names = subset(babynames, n > 1000)

df_list$train$HasName = rep(FALSE, nrow(df_list$train))
for (i in names$name) {
  df_list$train$HasName = df_list$train$HasName | grepl(i, df_list$train$Headline, ignore.case = TRUE)
}


money = c("dollar", "money", "wage", "salary", "euro", "bank", "million", "trillion")
df_list$train$Money = rep(FALSE, nrow(df_list$train))
for (i in money) {
  df_list$train$Money = df_list$train$Money | grepl(i, df_list$train$Headline, ignore.case = TRUE)
}

#Split by month. September and October - train, November - test
split = (df_list$train$Month == 8 | df_list$train$Month == 9)
train = subset(df_list$train, split == TRUE)
test = subset(df_list$train, split == FALSE)
valid = df_list$valid


# Quick predictions
factor_vars = c("NewsDesk", "SectionName", "SubsectionName")
qp <- data.frame()

# set without rows, where we can predict quickly
train_cut = train

for (var in factor_vars) {
  cat("Var:", var, '\n')
  #qp = rbind(qp, quickly_predict(train_cut$UniqueID, train_cut[[var]], train_cut$Popular, 1))
  qp = quickly_predict(train_cut$UniqueID, train_cut[[var]], train_cut$Popular, 1)
  train_cut = subset(train_cut, UniqueID %in% setdiff(train_cut$UniqueID, rownames(qp)))
}

#construct a data frame with variables and their factors that we will exclude from test/valid set
exc_lev_df <- data.frame()
for (var in factor_vars) {
  train_cut[[var]] = factor(train_cut[[var]])
  exc_lev_df = rbind(exc_lev_df, data.frame(var = var, 
                                            lev = setdiff(levels(train[[var]]), levels(train_cut[[var]]))))
}

# construct test/valid sets without rows, where we can predict quickly
test_cut = test
valid_cut = valid

for (i in levels(exc_lev_df$var)) {
  var_i = subset(exc_lev_df, var == i)  
  
  for (j in levels(var_i$lev)) {
    test_cut = subset(test_cut, test_cut[[i]] != j)
    valid_cut = subset(valid_cut, valid_cut[[i]] != j)
  }
}

# need to readjust number of factors
for (var in factor_vars) {
  test_cut[[var]] = factor(test_cut[[var]])
  valid_cut[[var]] = factor(valid_cut[[var]])
}
  
# construct a data frame with quick predictions (0)
train_quick_pred = data.frame(UniqueID = setdiff(train$UniqueID, train_cut$UniqueID))
test_quick_pred = data.frame(UniqueID = setdiff(test$UniqueID, test_cut$UniqueID))
valid_quick_pred = data.frame(UniqueID = setdiff(valid$UniqueID, valid_cut$UniqueID))

test_quick_pred$Probability1 = rep(0, nrow(test_quick_pred)) 
test_quick_pred$Popular = subset(test, UniqueID %in% intersect(test$UniqueID, test_quick_pred$UniqueID))$Popular

train_quick_pred$Probability1 = rep(0, nrow(train_quick_pred)) 
train_quick_pred$Popular = subset(train, UniqueID %in% intersect(train$UniqueID, train_quick_pred$UniqueID))$Popular

valid_quick_pred$Probability1 = rep(0, nrow(valid_quick_pred)) 

# Play with text
#tf = tf_frame(c(paste(train_cut$Headline, sep = " "), 
#                paste(test_cut$Headline, sep = " "), 
#                paste(valid_cut$Headline, sep = " ")), 
#                0.998)
#r = 4
#tf = tf_frame(c(paste(sapply(train_cut$Headline, function(x) paste(x,x,x,x, sep = " ")), train_cut$Abstract, sep = " "), 
#                paste(sapply(test_cut$Headline, function(x) paste(x,x,x,x, sep = " ")), test_cut$Abstract, sep = " "), 
#                paste(sapply(valid_cut$Headline, function(x) paste(x,x,x,x, sep = " ")), valid_cut$Abstract, sep = " ")), 
              #1)
#              0.995)
tf = tf_frame(c(paste(sapply(train_cut$Headline, function(x) paste(x,x, sep = " ")), train_cut$Abstract, sep = " "), 
                paste(sapply(test_cut$Headline, function(x) paste(x,x, sep = " ")), test_cut$Abstract, sep = " "), 
                paste(sapply(valid_cut$Headline, function(x) paste(x,x, sep = " ")), valid_cut$Abstract, sep = " ")), 
              #1)
              0.998)

row.names(tf) = c(train_cut$UniqueID, test_cut$UniqueID, valid_cut$UniqueID)

train_cut_op = train_cut
test_cut_op = test_cut
valid_cut_op = valid_cut


lev = "Opinion"
for (lev in levels(train_cut$SectionName)) {
  cat("SectionName: ", lev, "\n")
  #train_cut_op = subset(train_cut, SectionName == "Opinion")
  #test_cut_op = subset(test_cut, SectionName == "Opinion")
  train_cut_op = subset(train_cut, SectionName == lev)
  test_cut_op = subset(test_cut, SectionName == lev)
  valid_cut_op = subset(valid_cut, SectionName == lev) 
  
  ##tf_op = tf_frame(c(paste(sapply(train_cut_op$Headline, function(x) paste(x,x,x,x, sep = " ")), train_cut_op$Abstract, sep = " "), 
  ##                paste(sapply(test_cut_op$Headline, function(x) paste(x,x,x,x, sep = " ")), test_cut_op$Abstract, sep = " "), 
  ##                paste(sapply(valid_cut_op$Headline, function(x) paste(x,x,x,x, sep = " ")), valid_cut_op$Abstract, sep = " ")), 
                #1)
  ##              0.995)
  ##row.names(tf_op) = c(train_cut_op$UniqueID, test_cut_op$UniqueID, valid_cut_op$UniqueID)
  
  ##tf_train_op = subset(tf_op, row.names(tf_op) %in% train_cut_op$UniqueID)
  ##tf_test_op = subset(tf_op, row.names(tf_op) %in% test_cut_op$UniqueID)
  
  tf_train_op = subset(tf, row.names(tf) %in% train_cut_op$UniqueID)
  tf_test_op = subset(tf, row.names(tf) %in% test_cut_op$UniqueID)
  #train_tf = tf_frame(paste(train_cut$Headline, train_cut$Abstract, sep = " "), 0.995)
  #test_tf = tf_frame(paste(test_cut$Headline, test_cut$Abstract, sep = " "), 0.995)
  #rownames(train_tf) = rownames(train_cut)
  
  
  # Perform modeling on a train_cut and test on test_cut frames
  #train_cut_1 = cbind(train_cut, subset(tf, row.names(tf) %in% train_cut$UniqueID))
  ##train_cut_1 = cbind(train_cut_1, subset(tf_op, row.names(tf_op) %in% train_cut_1$UniqueID))
  train_tf_cut_op = cbind(data.frame(WordCount = train_cut_op$WordCount,
                                 Hour = train_cut_op$Hour,
                                 HeadlineLength = train_cut_op$HeadlineLength,
                                 HasName = train_cut_op$HasName,
                                 Money = train_cut_op$Money,
                                 #AbstractLength = train_cut_op$AbstractLength,
                                 Weekday = train_cut_op$Weekday))
                                 #subset(tf, row.names(tf) %in% train_cut_op$UniqueID))
  
  test_tf_cut_op = cbind(data.frame(WordCount = test_cut_op$WordCount,
                                 Hour = test_cut_op$Hour,
                                 HeadlineLength = test_cut_op$HeadlineLength,
                                 HasName = test_cut_op$HasName,
                                 Money = test_cut_op$Money,
                                 #AbstractLength = test_cut_op$AbstractLength,
                                 Weekday = test_cut_op$Weekday))
                                 #subset(tf, row.names(tf) %in% test_cut_op$UniqueID))
                                 #subset(tf_op, row.names(tf_op) %in% test_cut_op$UniqueID))
  
  
  # Normalization
  #preproc = preProcess(train_tf_cut_op)
  #train_tf_cut_op = predict(preproc, train_tf_cut_op)
  
  #preproc = preProcess(test_tf_cut_op)
  #test_tf_cut_op = predict(preproc, test_tf_cut_op)
  
  #train_cut_op_norm$Popular = train_cut_1$Popular
  #train_cut_1 = train_cut_op_norm 
  
  #train_cut_1 = train_cut
  #train_cut_1 = subset(train_cut_1, SectionName == "Opinion")
  #x = subset(train_cut_1, SectionName == "" & NewsDesk == "Foreign")
  #y = subset(train_cut_1, SectionName == "World" & NewsDesk == "Foreign")
  #mean(as.numeric(x$Popular))
  #mean(as.numeric(y$Popular))
  #x = subset(train_cut_1, SectionName == "Opinion" & NewsDesk != "OpEd")
  #train_cut_1[train_cut_1$UniqueID %in% x$UniqueID, ]$NewsDesk = "OpEd"
  
  #test_cut_1 = cbind(test_cut, subset(tf, row.names(tf) %in% test_cut$UniqueID))
  #test_cut_1 = cbind(test_cut_1, subset(tf_op, row.names(tf_op) %in% test_cut_1$UniqueID))
  #test_cut_1 = test_cut
  #test_cut_1 = subset(test_cut_1, SectionName == "Opinion")
  #y = subset(test_cut_1, SectionName == "Opinion" & NewsDesk != "OpEd")
  #test_cut_1[test_cut_1$UniqueID %in% y$UniqueID, ]$NewsDesk = "OpEd"
  
  preproc = preProcess(tf_train_op)
  tf_train_op = predict(preproc, tf_train_op)
  tf_train_op$Opinion = as.numeric(train_cut$SectionName == "Opinion")
    
  k = 10 
  #tf_kmeans = kmeans(tf_train_op, centers = k)#, iter.max = 1000)
  #tf_train_op$NewsDesk = train_cut_1$NewsDesk
  #km.kcca = kcca(tf_train_op, k, family = kccaFamily("ejaccard"), 
  km.kcca = kcca(tf_train_op, k, family = kccaFamily("angle")) 
  #km.kcca = kcca(tf_train_op, k,
  ##km.kcca = kcca(train_cut_1, k, family = kccaFamily("ejaccard"), 
  #     control = list(initcent = "kmeanspp"))
  # predict clusters on test data
  km.kcca = as.kcca(tf_kmeans, tf_train_op)
  #returns a vector of cluster numbers
  clusterTrain = predict(km.kcca)
  ##clusterTest = predict(km.kcca, newdata=tf_test_op)
  table(clusterTrain, train_cut_op$Popular)
  
  train_cut_cl = train_cut
  train_cut_cl$Cluster = clusterTrain 
  false_pos_train = subset(train_cut_cl, mdf_train$Probability1 > 0.8 & Popular == 0)
  false_neg_train = subset(train_cut_cl, mdf_train$Probability1 < 0.2 & Popular == 1)
  
  table(false_pos_train$Cluster)
  
  
  train_cut_1 = train_tf_cut_op
  test_cut_1 = test_tf_cut_op
  
  #verify if any of the columns has NA
  #unlist(lapply(train_cut_1, function(x) any(is.na(x))))
  
  train_cut_1$Popular = as.factor(train_cut_op$Popular)
  test_cut_1$Popular = as.factor(test_cut_op$Popular)
  
  train_cut_1$NewsDesk = train_cut_op$NewsDesk
  test_cut_1$NewsDesk = test_cut_op$NewsDesk
  train_cut_1$SectionName = train_cut_op$SectionName
  test_cut_1$SectionName = test_cut_op$SectionName
  train_cut_1$SubsectionName = train_cut_op$SubsectionName
  test_cut_1$SubsectionName = test_cut_op$SubsectionName
  
  #keywords = c("Quandary", "Facts & Figures", "Daily Report",
  #             "Daily Clip Report", "Vegeterian", "Word of the Day",
  #             "Recap", "New York Today", "Big Ticket", "Readers Respond",
  #             "Test Yourself", "Reading")
  
  #for (i in keywords) {
  #  col = gsub(" ", "", i)
  #  col = gsub("&", "", col)
  #  train_cut_1[[col]] = grepl(i, train_cut$Headline, ignore.case = TRUE)
  #  test_cut_1[[col]] = grepl(i, test_cut$Headline, ignore.case = TRUE)
  #}
  
  #x = randomForest(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName + Hour +
  #                   HeadlineLength + AbstractLength, 
                   
  #                       data = news_train_cl[[i]], method = "class", cp = best$bestTune[[1]]) 
  #x = randomForest(Popular~ . - UniqueID - Headline - Snippet - Abstract, 
  #x = rpart(Popular~ . - UniqueID - Headline - Snippet - Abstract - NewsDesk - SectionName - 
  #                   HeadlineLength - AbstractLength - 
  #                   SubsectionName - Weekday - Hour - WordCount, data = train_cut_1, method = "class")
  #x = rpart(train_cut_1$Popular~ . + train_cut_1$HeadlineLength + train_cut_1$WordCount + train_cut_1$AbstractLength +
  #            train_cut_1$Weekday + train_cut_1$Hour,
  #x = rpart(Popular~ ., 
  #          data = train_cut_1, cp = 0.001, method = "class")
  #set.seed(2998)
  #numFolds = trainControl(method = "cv", number = 10 )
  #cpGrid = expand.grid(.nodesize = c(1, 3, 5, 7, 10, 15, 25, 35), .ntree = c(500, 1000, 2000, 3000), .mtry = 10) 
  #cpGrid = expand.grid(.mtry = 10) 

  #best = train(Popular~ ., 
  #             data = train_cut_1, method = "rf", 
  #             trControl = numFolds, tuneGrid = cpGrid, nodesize = 5, ntree = 1000)
  
  y = x
  x = randomForest(Popular~ ., 
            data = train_cut_1, nodesize = 5, ntree = 1000)
  #x = randomForest(train_cut_1$Popular~ .,
  #          data = tf_train_op, nodesize = 5, ntree = 1000)
  #prp(x)
  #x = randomForest(Popular~ . - UniqueID - Headline - Snippet - Abstract - NewsDesk - SectionName - 
  #                   SubsectionName - Weekday - Hour - WordCount, 
  #                                data = train_cut_1, nodesize = 5, ntree = 1000) 
  #                                data = train_cut_1, nodesize = 5, ntree = 1000) 
  
  
  pred_test_by_secname  <- data.frame()
  pred_train_by_secname  <- data.frame()
  
  p = predict(x, newdata = test_cut_1, type="prob")
  
  mdf = data.frame(UniqueID = test_cut_op$UniqueID, Probability1 = p[,2], Popular = test_cut_1$Popular)
  # Add previously predicted data
  #fdf = rbind(mdf, test_quick_pred)
  fdf = mdf
  fdf = fdf[order(fdf$UniqueID), ]
  #print(access_accuracy(fdf$Popular,  fdf$Probability1 > 0.5, fdf$Probability1))
  cat("Test AUC: ", auc(fdf$Popular, fdf$Probability1), "\n")
  #print(table(fdf$Popular, fdf$Probability1 > 0.5))
  pred_test_by_secname = rbind(pred_test_by_secname, fdf)
  #pred_test_by_secname = rbind(pred_test_by_secname, test_quick_pred)
  pred_test_by_secname = pred_test_by_secname[order(pred_test_by_secname$UniqueID),] 
  
  p_train = predict(x, type="prob")
  
  mdf_train = data.frame(UniqueID = train_cut_op$UniqueID, Probability1 = p_train[,2], Popular = train_cut_1$Popular)
  # Add previously predicted data
  #fdf_train = rbind(mdf_train, train_quick_pred)
  fdf_train = mdf_train
  fdf_train = fdf_train[order(fdf_train$UniqueID), ]
  #print(access_accuracy(fdf_train$Popular,  fdf_train$Probability1 > 0.5, fdf_train$Probability1))
  cat("Train AUC: ", auc(fdf_train$Popular, fdf_train$Probability1), "\n")
  #print(table(fdf_train$Popular, fdf_train$Probability1 > 0.5))
  pred_train_by_secname = rbind(pred_train_by_secname, fdf_train)

#}


# full prediction by search doesn't work well
#search_pred(article_headline = train_cut_op$Headline, 
#            article_body = train_cut_op$Abstract, 
#            queries = paste(test_cut_op$Headline, test_cut_op$Abstract, sep = " "), 
#            train_judgement = train_cut_op$Popular, 
#            test_judgement = test_cut_op$Popular,
#            other_model_pred = pred_test_by_secname$Probability1, 
#            train = TRUE) 

#partial will do
#tr1 = subset(train, SectionName == "Opinion")
#tst1 = subset(test, SectionName == "Opinion")

#comb_pred = search_pred(article_headline = tr1$Headline, 
#            article_body = tr1$Abstract, 
#            queries = paste(tst1$Headline, tst1$Abstract, sep = " "), 
#            train_judgement = tr1$Popular, 
#            test_judgement = tst1$Popular,
#            other_model_pred = subset(pred_test_by_secname$Probability1, test$SectionName == "Opinion"), 
#            train = TRUE) 

tr1 = subset(train, SectionName == "Opinion")
tst1 = subset(test_cut_op, SectionName == "Opinion")

comb_pred = search_pred(article_headline = tr1$Headline, 
            article_body = tr1$Abstract, 
            queries = paste(tst1$Headline, tst1$Abstract, sep = " "), 
            train_judgement = tr1$Popular, 
            test_judgement = tst1$Popular,
            other_model_pred = subset(pred_test_by_secname$Probability1, test_cut_op$SectionName == "Opinion"), 
            train = TRUE) 

tr1 = train
tst1 = test_cut_op

comb_pred_all = search_pred(article_headline = tr1$Headline, 
            article_body = tr1$Abstract, 
            queries = paste(tst1$Headline, tst1$Abstract, sep = " "), 
            train_judgement = tr1$Popular, 
            test_judgement = tst1$Popular,
            other_model_pred = pred_test_by_secname$Probability1, 
            train = TRUE) 


#tr1 = subset(train, NewsDesk== "Business" & SectionName == "Business Day")
#tr1 = subset(train, NewsDesk== "Business")
tr1 = subset(train, SectionName == "Business Day")
#tr1 = train
tst1 = subset(test_cut_op, NewsDesk== "Business" & SectionName == "Business Day")

comb_pred_1 = search_pred(article_headline = tr1$Headline, 
            article_body = tr1$Abstract, 
            queries = paste(tst1$Headline, tst1$Abstract, sep = " "), 
            train_judgement = tr1$Popular, 
            test_judgement = tst1$Popular,
            other_model_pred = subset(pred_test_by_secname$Probability1, 
                                      test_cut_op$NewsDesk == "Business" &
                                      test_cut_op$SectionName == "Business Day"), 
            train = TRUE) 
  
final_pred = pred_test_by_secname 
final_pred = rbind(final_pred, test_quick_pred)
cat("Before combination AUC = ", auc(final_pred$Popular,final_pred$Probability1), '\n')

final_pred$Probability1[which(test_cut_op$SectionName == "Opinion")] = comb_pred 
final_pred =  final_pred[order(final_pred$UniqueID),] 
cat("Final Combined AUC = ", auc(final_pred$Popular,final_pred$Probability1), '\n')
#pred_train_by_secname = rbind(pred_train_by_secname, train_quick_pred)

#cat("Overall Test AUC: ", auc(pred_test_by_secname$Popular, pred_test_by_secname$Probability1), "\n")
#cat("Overall Train AUC: ", auc(pred_train_by_secname$Popular, pred_train_by_secname$Probability1), "\n")

#################### do the same with a validation set

#valid_cut_1 = cbind(valid_cut, subset(tf, row.names(tf) %in% valid_cut$UniqueID))
valid_cut_1 = valid_cut
p_valid = predict(x, newdata = valid_cut_1, type="prob")

mdf_valid = data.frame(UniqueID = valid_cut_1$UniqueID, Probability1 = p_valid[,2])


tr1 = subset(train, SectionName == "Opinion")
tst1 = subset(valid_cut, SectionName == "Opinion")

comb_pred = search_pred(article_headline = tr1$Headline, 
            article_body = tr1$Abstract, 
            queries = paste(tst1$Headline, tst1$Abstract, sep = " "), 
            train_judgement = tr1$Popular, 
            test_judgement = tst1$Popular,
            other_model_pred = subset(mdf_valid$Probability1, valid_cut$SectionName == "Opinion"), 
            train = FALSE) 




fdf_valid = rbind(mdf_valid, valid_quick_pred)
fdf_valid = fdf_valid[order(fdf_valid$UniqueID), ]
MySubmission = data.frame(UniqueID = fdf_valid$UniqueID, Probability1 = fdf_valid$Probability1)
write.csv(MySubmission, "Submission_Sergius_050315_1.csv", row.names=FALSE)


######################################################

#false_pos_train = subset(train_cut, mdf_train$Probability1 > 0.5 & Popular == 0)
false_pos_test = subset(test_cut, mdf$Probability1 > 0.7 & Popular == 0)
false_neg_test = subset(test_cut, mdf$Probability1 < 0.3 & Popular == 1)
not_sure_test = subset(test_cut, mdf$Probability1 > 0.4 & mdf$Probability1 < 0.6)

opinion_cut = subset(df_list$train, SectionName == "Opinion")
opinion_cut = data.frame(pop = opinion_cut$Popular, head = opinion_cut$Headline, 
                         abstract = opinion_cut$Abstract)

train_cut_cl = train_cut
train_cut_cl$Cluster = clusterTrain 
false_pos_train = subset(train_cut_cl, mdf_train$Probability1 > 0.8 & Popular == 0)
false_neg_train = subset(train_cut_cl, mdf_train$Probability1 < 0.2 & Popular == 1)

table(false_pos_train$Cluster)
#false_pos_test1 = subset(test_cut, mdf$Probability1 > 0.5 & Popular == 0)
#false_neg_test1 = subset(test_cut, mdf$Probability1 < 0.5 & Popular == 1)


table(train_cut_1$NewsDesk, train_cut_1$SectionName)
table(test_cut_1$NewsDesk, test_cut_1$SectionName)

table(df_list$train$NewsDesk, df_list$train$SectionName)
table(valid$NewsDesk, valid$SectionName)
#var = "NewsDesk"
var = "SectionName"

for(lev in levels(test_cut_1[[var]])) {
  x = subset(test_cut_op, test_cut_1[[var]] == lev)  
  y = subset(mdf, UniqueID %in% x$UniqueID)
  cat("Category: ", lev, '\n')
  #levs = sort(union(x$Popular))
  actual = factor(x$Popular)
  prob = y$Probability1
  predicted = factor(as.numeric(prob > 0.5), c(0,1))
  print(table(actual = actual, predicted = predicted))
  print(access_accuracy(actual,  predicted, prob))
  cat('\n')
}


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
#  library(tm)
#  library(SnowballC)

#corpus = Corpus(VectorSource(df_list$train$Abstract))
#corpus = Corpus(VectorSource(df_list$train$Headline))
  #corpus = tm_map(corpus, tolower)
  #corpus = tm_map(corpus, PlainTextDocument)
  #corpus = tm_map(corpus, removePunctuation)
  #corpus = tm_map(corpus, removeWords, stopwords("english"))
  #corpus = tm_map(corpus, stemDocument)
  #tf = DocumentTermMatrix(corpus, control = list(
  #  weighting = function(x)
  #    weightTf(x)))

  #tf_idf = DocumentTermMatrix(corpus, control = list(
  #  weighting = function(x)
  #    weightTfIdf(x, normalize = FALSE)))

  #frequencies = DocumentTermMatrix(corpus, control = list(
  #  weighting = function(x)
      #weightSMART(x, spec = "ntn")))
  #    weightSMART(x, spec = "ltn")))

  #frequencies = DocumentTermMatrix(corpus)
  #very very slow, takes a lot of memory (600 MB)
  #idf = as.matrix(tf_idf) / as.matrix(tf)

  #sparse = removeSparseTerms(frequencies, 0.995)
  #term_score_matrix = as.data.frame(as.matrix(sparse))
  #colnames(term_score_matrix) = make.names(colnames(term_score_matrix))
  
#tf = term_score_matrix 
#tf = tf_frame(c(df_list$train$Abstract, df_list$train$Snippet), 0.995)
#tf = tf_frame(df_list$train$Abstract, 0.995)
#tf = tf_frame(df_list$train$Abstract, 0.95)
tf = tf_frame(paste(df_list$train$Headline, df_list$train$Abstract, sep = " "), 0.995)
tf_valid = tf_frame(df_list$valid$Headline, 0.995)

rownames(tf) = rownames(df_list$train)
rownames(tf_valid) = rownames(df_list$valid)
#tail(sort(colMeans(tf)))

#library(caTools)
#set.seed(123)
#split = sample.split(df_list$train$Popular, SplitRatio = 0.7)
news_tf_train = cbind(tf, data.frame(WordCount = df_list$train$WordCount, 
                                     NewsDesk = df_list$train$NewsDesk, 
                                     SectionName = df_list$train$SectionName,
                                     SubsectionName = df_list$train$SubsectionName, 
                                     Weekday = df_list$train$Weekday,
                                     Month = df_list$train$Month,
                                     Hour = df_list$train$Hour,
                                     Popular = df_list$train$Popular))

news_tf_train$Popular = as.factor(news_tf_train$Popular)

#No 'Popular' here
news_tf_valid = cbind(tf_valid, data.frame(WordCount = df_list$valid$WordCount, 
                                     NewsDesk = df_list$valid$NewsDesk, 
                                     SectionName = df_list$valid$SectionName,
                                     SubsectionName = df_list$valid$SubsectionName, 
                                     Hour = df_list$valid$Hour,
                                     Weekday = df_list$valid$Weekday))

news_tf_valid$SectionName <- factor(news_tf_valid$SectionName,
                                    levels = c(levels(news_tf_valid$SectionName),
                                               "Sports", "Style"))
                                               #"Fashion & Style", "Politics"))

news_tf_valid$SubsectionName <- factor(news_tf_valid$SubsectionName,
                                    levels = c(levels(news_tf_valid$SubsectionName),
                                               "Fashion & Style", "Politics"))

news_tf_valid$NewsDesk <- factor(news_tf_valid$NewsDesk,
                                    levels = c(levels(news_tf_valid$NewsDesk),
                                               "National", "Sports"))

#news_train = subset(df_list$train, split == TRUE)
#news_test = subset(df_list$train, split == FALSE)
news_train = subset(news_tf_train, split == TRUE)
news_test = subset(news_tf_train, split == FALSE)

tf_train = subset(tf, split == TRUE)
tf_test = subset(tf, split == FALSE)

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

#for (i in 1:length(news_train_cl)) {
  #cat(i, ":", nrow(news_train_cl[[i]]), "\n")
  #print(tail(sort(colMeans(news_train_cl[[i]]))))
#}

#lapply(tf_cl_kmeans,nrow)
#which.max(sapply(tf_cl_kmeans,nrow))
#which.min(sapply(tf_cl_kmeans,nrow))



library(rpart)
library(rpart.plot)
library(randomForest)
#install.packages("lme4")
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10 )
cpGrid = expand.grid(.cp = seq(0.001,0.3,0.001)) 

# Perform the cross validation

newsModel <- list()

for (i in 1:k) {
  #best = train(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName, 
  #             data = news_train_cl[[i]], method = "rpart", 
  #             trControl = numFolds, tuneGrid = cpGrid)
  news_train_cl[[i]]$Popular = as.factor(news_train_cl[[i]]$Popular)
  ###newsModel[[i]] = glm(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName, 
  ###                     data = news_train_cl[[i]],  family = "binomial") 
  ##newsModel[[i]] = rpart(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName,
  ##                       data = news_train_cl[[i]], method = "class", cp = best$bestTune[[1]]) 
  ####newsModel[[i]] = randomForest(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName, 
  #newsModel[[i]] = randomForest(Popular~ .-WordCount-Weekday-NewsDesk-SectionName-SubsectionName, 
  newsModel[[i]] = randomForest(Popular~ .,
                                data = news_train_cl[[i]], nodesize = 5, ntree = 1000) 
  
}
  
newsModel_predict_test <-list()
newsModel_predict_test_class <-list()
#combined <- data.frame(UniqueID = c(), Probability1 = c())
combined <- data.frame()

for (i in 1:k) {
  #newsModel_predict_test[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]], type = "response")
  newsModel_predict_test_class[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]], 
                                              type = "class")
  ###                                            type = "response")
  ##newsModel_predict_test[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]])
  newsModel_predict_test[[i]] = predict(newsModel[[i]], newdata = news_test_cl[[i]], type="prob")
  
  combined = rbind(combined, 
                   data.frame(UniqueID = as.numeric(as.character(rownames(newsModel_predict_test[[i]]))),
                              Probability1 = newsModel_predict_test[[i]][,2]))
  
  acc = access_accuracy(news_test_cl[[i]]$Popular, 
                        newsModel_predict_test_class[[i]],
                        newsModel_predict_test[[i]][,2])
                        ###newsModel_predict_test[[i]])
  ###acc = access_accuracy(news_test_cl[[i]]$Popular, 
  ###                      newsModel_predict_test_class[[i]] > 0.5,
  ###                      newsModel_predict_test_class[[i]])
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

#news_train_1 = news_train
#news_train_1$Popular = as.factor(news_train_1$Popular)
#news_test_1 = news_test
#news_test_1$Popular = as.factor(news_test_1$Popular)

#without any clustering!!!
x = randomForest(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName + Hour, 
                                data = news_train, nodesize = 5, ntree = 1000) 
p = predict(x, newdata = news_test, type="prob")
data.frame(News)
access_accuracy(news_test$Popular,  p[,2] > 0.5, p[,2])

#Run model on the complete training set
#Test it on a validation set

full_train_model = randomForest(Popular~ WordCount + Weekday + NewsDesk + SectionName +  SubsectionName + Hour, 
                                data = news_tf_train, nodesize = 5, ntree = 1000) 
valid_prop = predict(full_train_model, newdata = news_tf_valid, type="prob")
#out_data = data.frame(UniqueID = df_list$valid$UniqueID,
#           Probability1 = valid_prop[,2])
MySubmission = data.frame(UniqueID = NewsValid$UniqueID, Probability1 = valid_prop[,2])
write.csv(MySubmission, "Submission_Sergius.csv", row.names=FALSE)
