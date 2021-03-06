% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_cv_glm.R
\name{bin_cv_glm}
\alias{bin_cv_glm}
\title{Cross-Validated Logistic Regression Model}
\usage{
bin_cv_glm(k = 5, data, formula, y, seed = runif(1, 0, 9999999))
}
\arguments{
\item{k}{for k-fold. Default is 5}

\item{data}{data that will be cross-validated}

\item{formula}{ranger formula}

\item{y}{the target variable}

\item{seed}{random seed. Default is random}
}
\value{
returns a list containing ROC area and PR area
}
\description{
Apply cross-validation on a logistic regression model and return ROC area and precision-recall curve area
}
