#' Binary Model Evaluation
#'
#' @description
#' Computes confusion matrix along with other evaluation parameters for binary
#' classification problem
#'
#' @param actual original data values - binary.
#' @param predicted predicted values in probabilities or binary.
#' @param threshold threshold at which predicted probabilities are to classified.
#' @param fn_weight cost of false negatives when calculating asymmetric weight. Defaulted to 10
#' @param fp_weight cost of false positives when calculating asymmetric weight. Defaulted at 1.
#' @return Returns a list that contains the confusion matrix as a dataframe, and another dataframe
#' that contains evaluation parameters. Visit \href{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}{Wikipedia}  for definitions
#'
#' @export
#'
conf_matrix <- function(actual, predicted, threshold = 0.5, fn_weight = 10, fp_weight = 1) {

  # Factorizing the data
  actual <- as.factor(actual)

  if (class(predicted) != "factor") {
    predicted <- as.factor(ifelse(predicted > threshold, 1, 0))
  }

  n <- length(actual)
  # Calculating true/false positives/negatives
  tn <- sum(predicted[which(actual == 0)] == 0)
  tp <- sum(predicted[which(actual == 1)] == 1)
  fn <- sum(predicted[which(actual == 1)] == 0)
  fp <- sum(predicted[which(actual == 0)] == 1)

  accuracy <- round((tp + tn) / n, 4)
  sensitivity <- round(tp / (tp + fn), 4)
  specificity <- round(tn / (tn + fp), 4)
  # asym_cost is the weighted average of falsely predicted labels
  asym_cost <- round((fp_weight * fp + fn_weight * fn) / n, 4)
  matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, ncol = 2, byrow = FALSE,
                   dimnames = list(c("predicted_yes", "predicted_no"), c("actual_yes", "actual_no")))
  matrix <- as.data.frame(matrix)

  eval_params <- data.frame(
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    true_positives = tp,
    true_negatives = tn,
    false_positives = fp,
    false_negatives = fn,
    asym_cost = asym_cost )

  return(list(matrix = matrix, eval_params = eval_params))
}
