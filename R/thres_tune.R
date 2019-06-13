#' Threshold Tuning
#'
#' @description
#' Tunes through a given range of values to determine model performance
#'
#' @param actual original data values - binary.
#' @param predicted predicted values in probabilities or binary.
#' @param prob_start to indicate the value at which the tuning must start. Defaulted to 0
#' @param prob_end to indicate at what value of probability the tuning must stop. Defaulted to 0.5
#' @param prob_precsn to indicate at what precision each threshold must be incremented. Defaulted to 0.01
#' @param fn_weight cost of false negatives when calculating asymmetric weight. Defaulted to 10
#' @param fp_weight cost of false positives when calculating asymmetric weight. Defaulted at 1.
#' @return Returns a datafram with model evaluation at each value of threshold
#'
#' @export
#'

thres_tune <- function(actual, predicted, prob_start = 0, prob_end = 0.5,
                       prob_precsn = 0.01, fn_weight = 10, fp_weight = 1) {

  thres_grid <- expand.grid(threshold = seq(prob_start, prob_end, by = prob_precsn),
                            accuracy = 0,
                            sensitivity = 0,
                            specificity = 0,
                            true_positives = 0,
                            true_negatives = 0,
                            false_positives = 0,
                            false_negatives = 0,
                            asym_cost = 0)

  for (i in 1:nrow(thres_grid)) {

    thres_conf_matrix <- mlTools::conf_matrix(actual, predicted, fn_weight = 10, fp_weight = 1,
                                              threshold = thres_grid$threshold[i])

    thres_grid$accuracy[i] <- thres_conf_matrix$eval_params["accuracy"][[1]]
    thres_grid$sensitivity[i] <- thres_conf_matrix$eval_params["sensitivity"][[1]]
    thres_grid$specificity[i] <- thres_conf_matrix$eval_params["specificity"][[1]]
    thres_grid$true_positives[i] <- thres_conf_matrix$eval_params["true_positives"][[1]]
    thres_grid$true_negatives[i] <- thres_conf_matrix$eval_params["true_negatives"][[1]]
    thres_grid$false_positives[i] <- thres_conf_matrix$eval_params["false_positives"][[1]]
    thres_grid$false_negatives[i] <- thres_conf_matrix$eval_params["false_negatives"][[1]]
    thres_grid$asym_cost[i] <- thres_conf_matrix$eval_params["asym_cost"][[1]]
  }
  return(thres_grid)
}
