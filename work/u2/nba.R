
nba = read.csv("NBA_train.csv")
nba_test = read.csv("NBA_test.csv")
table(nba$W, nba$Playoffs)
nba$PTSdiff = nba$PTS - nba$oppPTS
plot(nba$PTSdiff, nba$W)

WinsReg = lm(W ~ PTSdiff, data = nba)
