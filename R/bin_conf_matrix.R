#' Confusion Matrix for a binary model
#'
#' @description
#' Computes confusion matrix along with other evaluation parameters for binary
#' classification problem
#'
#' @param actual original data values - binary.
#' @param predicted predicted values in probabilities or binary.
#' @param threshold threshold at which predicted probabilities are to classified.
#' @return Returns the confusion matrix as a dataframe
#'
#' @export
#'
bin_conf_matrix <- function(actual, predicted, threshold = 0.5) {

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

  matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, ncol = 2, byrow = FALSE,
                   dimnames = list(c("predicted_yes", "predicted_no"), c("actual_yes", "actual_no")))
  matrix <- as.data.frame(matrix)

  return(matrix)
}
