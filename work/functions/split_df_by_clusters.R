
# Splits a data frame 'df' by clusters according to cluster 'cl' vector
# Returns a list of data frames
split_df_by_clusters <- function(df, cl) {
  
  cl_kmeans <- list()
  
  for (i in 1:max(cl)) {
    cl_kmeans[[i]] = subset(df, cl == i)
  }
  
  cl_kmeans
}
  