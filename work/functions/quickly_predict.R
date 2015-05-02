
quickly_predict <- function(id_vec, indep_var_vec, dep_var_vec, threshold) {
  quick_pred_df <- data.frame()
  
  for(lev in levels(indep_var_vec)) {
    x = data.frame(id = id_vec, f = indep_var_vec, t = dep_var_vec)
    z = subset(x, f == lev)
    #y = nrow(subset(z, Popular == 1))
    y = sum(dep_var_vec[indep_var_vec == lev])
    if (y <= threshold) {
      cat(lev, " has", y, "from", nrow(z), "rows with Popular = 1", "(making P = 0)", '\n')
      df_list$train = df_list$train[setdiff(df_list$train$UniqueID, x$UniqueID), ]
      quick_pred_df = rbind(quick_pred_df, data.frame(row.names = z$id, Probability1 = rep(0, nrow(z))))
    }
  }
  
  quick_pred_df 
}