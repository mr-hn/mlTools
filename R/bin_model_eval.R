#' Binary Model Evaluation
#'
#' @description
#' Evaluates model performance parameters - AUC-ROC, AUC-PR, ROC and PR charts, threshold tuned df
#'
#' @param actual original data values - binary.
#' @param predicted predicted values in probabilities or binary.
#' @return a list of AUC-ROC, AUC-PR, ROC and PR charts, threshold tuned df
#'
#' @import ggplot2
#' @export
#'

bin_model_eval <- function(actual, predicted) {

  j <- 1
  threshold <- 0
  tpr <- 0
  fpr <- 0
  precision <- 0
  recall <- 0
  sensitivity <- 0
  specificity <- 0
  fp <- 0
  fn <- 0
  accuracy <- 0

  # Threshold ranges from 0 to 1 at 0.01 increment
  for (i in seq(1, 0, by = -0.01)) {
    # Call thres_eval for all values of threshold
    thres_eval <- bin_thres_eval(actual, predicted, threshold = i)
    # Calculating tpr, fpr, precision, recall, sensitivity and specificity
    tpr[j] <- thres_eval$tp / (thres_eval$tp + thres_eval$fn)
    fpr[j] <- thres_eval$fp / (thres_eval$fp + thres_eval$tn)
    precision[j] <- thres_eval$tp / (thres_eval$tp + thres_eval$fp)
    recall[j] <- tpr[j]
    sensitivity[j] <- tpr[j]
    specificity[j] <- 1 - fpr[j]
    fp[j] <- thres_eval$fp
    fn[j] <- thres_eval$fn
    accuracy[j] <- (thres_eval$tp + thres_eval$tn) /
      (thres_eval$tp + thres_eval$tn + thres_eval$fp + thres_eval$fn)
    threshold[j] <- i
    j <- j + 1
  }

  #### Model performance at each value of threshold
  thres_df <- data.frame(threshold, accuracy, sensitivity, specificity, fp, fn)
  thres_df <- thres_df[order(threshold),]


  #### Calculating ROC AUC
  # Calculating space between two consecutive points in FPR and TPR
  # A 0 is padded since the difference vector is one position shorter
  dfpr <- c(diff(fpr), 0)
  dtpr <- c(diff(tpr), 0)

  # Adding up the height and width of each full blocks
  roc_area <- sum(tpr * dfpr)
  # Adding to that, area of half blocks at the top of the line
  roc_area <- roc_area + sum(dtpr * dfpr)/2

  # Prepping ROC plot
  roc_df <- data.frame(fpr, tpr)
  roc_curve <- ggplot(roc_df, aes(fpr, tpr)) + geom_line() + theme_minimal() +
    ggtitle(paste("Area under the ROC curve = ", round(roc_area, 3))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                 color = "grey", linetype = "dashed") +
    xlab("False Positive Rate") + ylab("True Positive Rate")


  #### Calculating PR AUC
  pr_area <- bin_calc_auc_pr(actual, predicted)

  # Prepping PR plot
  pr_df <- data.frame(precision, recall)
  pr_curve <- ggplot(pr_df, aes(recall, precision)) + geom_line() + theme_minimal() +
    ggtitle(paste("Area under the PR curve = ", round(pr_area, 3))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Recall") + ylab("Precision") +
    xlim(0, 1) + ylim(0, 1)


  # Return ROC and PR curves, and Threshold table
  return(list(roc_area = roc_area,
              roc_curve = roc_curve,
              pr_area = pr_area,
              pr_curve = pr_curve,
              thres_df = thres_df))
}
