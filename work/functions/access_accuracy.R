
# Input: actual, predicted - vectors
# Returns a vector: 
#   1st elem - baseline accuracy
#   2nd elem - test accuracy
#   3nd elem - AUC
access_accuracy <- function(actual, predicted_class, predicted) {
  y = table(actual)
  baseline = max(y[1], y[2]) / length(actual)
  x = table(actual, predicted_class)
  #x = as.matrix(table(actual, predicted))
  #handle rare cases
  #if (length(rownames(x)) == 1) {
  #  if (rownames(x)[1] == "0") {
  #    tn = x[1,1]
  #    tp = 0
  #  } else {
  #    tn = 0
  #    tp = x[1,2]
  #  }
  #    
  #} else {
    tn = x[1,1]
    tp = x[2,2]
  #}
  
  accuracy = (tn + tp)/length(predicted_class)
  
  library(ROCR)
  
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
  
  c(baseline, accuracy, auc)  
}