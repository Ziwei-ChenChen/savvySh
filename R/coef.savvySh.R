#' @title Extract Coefficients for a Slab and Shrinkage Linear Regression Model
#'
#' @description
#' Extracts the regression coefficients from a \code{savvySh_model} object. You may specify one or
#' more shrinkage estimators through the \code{estimator} parameter. If no estimator is specified,
#' the function returns coefficients for all available estimators as a named list.
#'
#' @param object A fitted \code{savvySh_model} object produced by \code{savvySh}.
#' @param estimator A character vector naming one or more estimators from which to extract coefficients.
#'   Valid names are those stored in \code{object$coefficients} (e.g., \code{"St"}, \code{"DSh"}, \code{"Sh"},
#'   \code{"SR"}, \code{"GSR"}, etc.). If \code{NULL}, coefficients for all available estimators are returned.
#' @param ... Additional arguments passed to \code{\link{predict.savvySh_model}}.
#'
#' @details
#' This function internally calls \code{\link{predict.savvySh_model}} with \code{type = "coefficients"}
#' to retrieve the desired coefficient estimates. If multiple estimators are requested (or if none is specified,
#' in which case all are returned), the output is a named list in which each element is a numeric vector of coefficients.
#' The coefficient vectors are named according to whether an intercept is present (for Linear shrinkage, no intercept).
#' If a single estimator is specified, a single named numeric vector is returned.
#'
#' @return
#' A named numeric vector of regression coefficients if a single estimator is specified, or a named list of
#' such vectors if multiple estimators are requested.
#'
#' @seealso
#'   \code{\link{predict.savvySh_model}} for generating predictions,
#'   \code{\link{savvySh}} for fitting slab and shrinkage linear models.
#'
#' @importFrom stats predict
#' @method coef savvySh_model
#' @export
coef.savvySh_model <- function(object, estimator = NULL, ...) {
  predict(object, type = "coefficients", estimator = estimator, ...)
}

