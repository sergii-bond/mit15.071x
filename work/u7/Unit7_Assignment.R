
#####################################
# Maps
library("ggmap")
library("ggplot2")
library("maps")
statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling = read.csv("PollingImputed.csv")
str(polling)
Train = subset(polling, Year >= 2004 & Year <= 2008)
Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

#TestPrediction gives the predicted probabilities for each state, 
#but let's also create a vector of Republican/Democrat predictions 
#by using the following command:

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

#Now, put the predictions and state labels in a data.frame so that we can use ggplot:

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

#To make sure everything went smoothly, answer the following questions.

#For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
#table(predictionDataFrame$Test.State, predictionDataFrame$TestPredictionBinary) 
nrow(subset(predictionDataFrame, TestPredictionBinary == 1)) 
mean(predictionDataFrame$TestPrediction)

#Now, we need to merge "predictionDataFrame" with the map data "statesMap", 
#like we did in lecture. Before doing so, we need to convert the Test.State variable 
#to lowercase, so that it matches the region variable in statesMap. 
#Do this by typing the following in your R console:
  
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

#Now, merge the two data frames using the following command:
  
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#Lastly, we need to make sure the observations are in order so that the map is drawn 
#properly, by typing the following:
  
predictionMap = predictionMap[order(predictionMap$order),]

#How many observations are there in predictionMap?
nrow(predictionMap)
nrow(statesMap)

ggplot(predictionMap, aes(x = long, 
                          y = lat, 
                          group = group, 
                          fill = TestPredictionBinary)) +  
      geom_polygon(color = "black")

                          #fill = TestPredictionBinary))+ 
ggplot(predictionMap, aes(x = long, 
                          y = lat, 
                          group = group, 
                          fill = TestPrediction))+ 
  geom_polygon(color = "black",
               linetype = 3,
               size = 3,
               alpha = 0.3,
               ) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      #breaks= c(0,1), 
                      #labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")
subset(predictionDataFrame, Test.State == "Florida")

########################################
# Graphs
edges = read.csv("edges.csv")
users = read.csv("users.csv")
nrow(users)
#In our dataset, what is the average number of friends per user?
2 * nrow(edges) / nrow(users)

table(users$locale)
table(users$locale, users$gender)

install.packages("igraph")
library("igraph")
g = graph.data.frame(edges, directed = FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
degree(g)
str(g)
#nrow(subset(g, degree(g) >= 10))
max(degree(g)/2+2)
min(degree(g)/2+2)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

?igraph.plotting

######################################################
#Text Clouds

tweets = read.csv("tweets.csv",  stringsAsFactors=FALSE)

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

#1) Create a corpus using the Tweet variable
corpus = Corpus(VectorSource(tweets$Tweet))

#2) Convert the corpus to lowercase (don't forget to type "corpus = tm_map(corpus, PlainTextDocument)" in your R console right after this step)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

#3) Remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)

#4) Remove all English-language stopwords
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
#corpus = tm_map(corpus, removeWords, stopwords("english"))

#5) Build a document-term matrix out of the corpus
frequencies = DocumentTermMatrix(corpus)

#6) Convert the document-term matrix to a data frame called allTweets
allTweets = as.data.frame(as.matrix(frequencies))
str(allTweets)
ncol(allTweets)

install.packages("wordcloud")
library("wordcloud")
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), c(2, 0.25))
wordcloud(colnames(allTweets), colSums(allTweets), max.words = 100, random.order = FALSE)
wordcloud(colnames(allTweets), 
          colSums(allTweets), 
          max.words = 100, 
          random.order = FALSE,
          random.color = TRUE
          )
install.packages("RColorBrewer")
library("RColorBrewer")
#The function brewer.pal() returns color palettes from the ColorBrewer project 
#when provided with appropriate parameters, 
#and the function display.brewer.all() displays the palettes we can choose from.
#brewer.pal()
display.brewer.all() 
