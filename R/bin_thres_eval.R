#' Threshold Evaluation
#'
#' @description
#' Evaluate the model for a given threshold
#'
#' @param actual original data values - binary.
#' @param predicted predicted values in probabilities or binary.
#' @param threshold threshold at which predicted probabilities are to classified.
#' @return Returns True/False Positive/Negatives
#'
#' @export
#'

bin_thres_eval <- function(actual, predicted, threshold = 0.5) {

  # Factorizing the data
  actual <- as.factor(actual)

  # Setting threshold
  if (class(predicted) != "factor") {
    predicted <- as.factor(ifelse(predicted >= threshold, 1, 0))
  }

  n <- length(actual)
  # Calculating true/false positives/negatives
  tn <- sum(predicted[which(actual == 0)] == 0)
  tp <- sum(predicted[which(actual == 1)] == 1)
  fn <- sum(predicted[which(actual == 1)] == 0)
  fp <- sum(predicted[which(actual == 0)] == 1)

  # Return as a list
  eval_params <- list(
    tn = tn,
    tp = tp,
    fn = fn,
    fp = fp)

  return(eval_params = eval_params)
}
