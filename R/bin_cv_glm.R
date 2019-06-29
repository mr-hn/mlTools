#' Cross-Validated Logistic Regression Model
#'
#' @description
#' Apply cross-validation on a logistic regression model and return ROC area and precision-recall curve area
#'
#' @param k for k-fold. Default is 5
#' @param data data that will be cross-validated
#' @param formula ranger formula
#' @param y the target variable
#' @param seed random seed. Default is random
#' @return returns a list containing ROC area and PR area
#'
#' @import ranger
#' @export
#'
bin_cv_glm <- function(k = 5, data, formula, y, seed = runif(1, 0, 9999999)) {

  # Ensure formula is typed right
  if (class(formula) != "formula") {
    formula = as.formula(formula)
  }

  # Shuffle data
  set.seed(seed)
  data <- data[sample(nrow(data)), ]

  # Create K-folds
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)

  # Initialize variables
  model_roc_area <- 0
  model_pr_area <- 0

  # Perform K-fold cross-validation
  for (i in 1:k) {

    # Segement your data by fold using the which() function
    test_index <- which(folds == i, arr.ind = TRUE)
    data_test <- data[test_index, ]
    data_train <- data[-test_index, ]

    # Model
    model <-  glm(formula, data_train, family = binomial)
    # Apply model on test data
    predicted <- predict(model, data_test, type = "response")
    actual <- data_test[[y]]

    model_eval <- bin_model_eval(actual, predicted)

    model_roc_area[i] <- model_eval$roc_area
    model_pr_area[i] <- model_eval$pr_area
  }

  return(list(roc_area = model_roc_area,
              pr_area = model_pr_area))
}
