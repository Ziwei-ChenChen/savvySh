#' Extract Coefficients From a Slab and Shrinkage Linear Regression Estimation Object
#'
#' Extract the estimated coefficients from a fitted Slab and Shrinkage linear regression model object.
#'
#' @param object A fitted model object of class \code{savvySh_model}.
#' @param estimator The specific estimator to use for extracting coefficients. For example, \code{"St"}, \code{"DSh"}, etc.
#'                  This must match one of the estimators in the fitted \code{savvySh_model} object.
#'                  Defaults to the first available estimator if not specified.
#' @param ... Additional arguments passed to the \code{predict} method.
#'
#' @return A named vector of coefficients for the specified estimator.
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @importFrom stats predict
#' @method coef savvySh_model
#' @seealso \code{\link{predict.savvySh_model}}, \code{\link{savvySh}}
#' @export
coef.savvySh_model <- function(object, estimator = NULL, ...) {
  predict(object, type = "coefficients", estimator = estimator, ...)
}


