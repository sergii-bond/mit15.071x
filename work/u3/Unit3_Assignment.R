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