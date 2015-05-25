
# Input:
#   article_headline - vector of  Article Name
#   article_body - vector of Article Abstract
#   queries - vector of Queries (a query can be a combination of Article Name + Article Abstract)
#   train_judgement - vector of training judgements of popularity 
#   test_judgement - vector of testing judgements of popularity (search model tried to predict them)
#   other_model_pred - vector of predictions by another model for comparison
#   train - execute training (TRUE) or run on validation set (FALSE) ?
# Output:
#   Best predictions of the search in combination with the other_model_pred 

search_pred <- function(article_headline, 
                        article_body, 
                        queries, 
                        train_judgement, 
                        test_judgement, 
                        other_model_pred, 
                        train = FALSE) {
  
  #Predict by search, then combine. Leave quick_predictions to the very end - they go straight into the output
  news.dat = data.frame(Contents = paste(article_headline, article_body, sep = " "))
  write.table(news.dat, file = "news.dat", row.names=FALSE, col.names = FALSE)
  
  news.dat.names = data.frame(Contents = article_headline)
  write.table(news.dat.names, file = "news.dat.names", row.names=FALSE, col.names = FALSE)
  
  news.queries = data.frame(Contents = queries)
  write.table(news.queries, file = "news-queries.txt", row.names=FALSE, col.names = FALSE)
  
  system("rm output_ana.csv")
  system("cd /home/sergius/tools/meta/build && rm -rf news-fwd && rm -rf news-inv && ./competition config_ana.toml > test")
  system("cp /home/sergius/tools/meta/build/Assignment2/output_ana.csv ~/work/mit15.071x/work/competition")
  search_df = read.csv("output_ana.csv", header = FALSE)
  # remove last line (for some reasons search 'competition' code writes an extra row)
  search_df = head(search_df, -1)
  
  search_scores = data.frame()
  
  # Trnslate train docID to their popularity
  for(col in colnames(search_df)) {
    search_df[[col]] = sapply(search_df[[col]], function(x) train_judgement[x])
  }
  
  if (train == TRUE) {
    # Find out how many columns to use to calculate an average probability
    best_col_num = 1
    auc_max = 0
    
    for (j in 1:ncol(search_df)) {
      if (j > 1)
        score = rowMeans(search_df[, c(1:j)])
      else
        score = search_df[, c(1:j)]
      #table(test_opinion$Popular, score_opinion > 0.5)
      auc_j = auc(test_judgement, score)
      cat(j, "AUC = ", auc_j, '\n')
      
      if (auc_j > auc_max) {
        auc_max = auc_j
        best_col_num = j 
      }
      
    }
    cat("The number of columns that maximizes AUC is ", best_col_num, '\n')
  } else {
    best_col_num = 2
    cat("Using ", best_col_num, 'columns \n')
  }
  
  combine_prob <- function(v1, v2, a) {
    out = a * v1 + (1 - a) * v2
    out
  }
  
  if (best_col_num > 1)
    search_score = rowMeans(search_df[, c(1:best_col_num)])
  else
    search_score = search_df[, c(1:best_col_num)]
  
  if (train == TRUE) {
    
    #Choose the best num of cols and calc an avr prob
    view_comp = data.frame(Popular = test_judgement,
                           PredForest = other_model_pred,
                           PredSearch = search_score)
    
    cat("Other Model AUC = ", auc(view_comp$Popular, view_comp$PredForest), '\n')
    cat("Search AUC = ", auc(view_comp$Popular, search_score), '\n')
  
    best_a = 0
    auc_max = 0
    
    for (a in seq(0,1,0.1)) {
      combined_pred = combine_prob(search_score, view_comp$PredForest, a)
      auc_a = auc(view_comp$Popular, combined_pred)
      cat("a = ", a, " Combined AUC = ", auc_a, '\n')
      
      if (auc_a > auc_max) {
        auc_max = auc_a
        best_a = a
      }
      
    }
    
    cat("parameter 'a' that maximizes the combined AUC is ", best_a, '\n')
  } else {
    best_a = 0.5
  }
  
  combine_prob(search_score, other_model_pred, best_a)
}