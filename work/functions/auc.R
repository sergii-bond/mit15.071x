
library(ROCR)
# Input: actual, predicted probabilities - vectors
# Returns AUC 
auc <- function(actual, predicted) {
  
  # Prediction function
  #str(predicted)
  #str(actual)
  ROCRpred = prediction(predicted, actual)
  # Performance function
  ROCRperf = performance(ROCRpred, "tpr", "fpr")
  # Plot ROC curve
  #plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
  auc = as.numeric(performance(ROCRpred, "auc")@y.values)
  #auc = 0
  
  auc  
}