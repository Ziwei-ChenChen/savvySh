#' Predict for Slab and Shrinkage Linear Regression Estimation
#'
#' This function predicts fitted values or coefficients for a fitted Slab and Shrinkage linear regression model
#' created using the \code{savvySh} function.
#'
#' @param object Fitted \code{"savvySh_model"} object returned by \code{savvySh}.
#' @param newx Matrix of new data for which predictions are to be made. Must have the same number of columns as the training data.
#'             This argument is required for \code{type = "response"}.
#' @param type Type of prediction required. Can be \code{"response"} (fitted values) or \code{"coefficients"}.
#'             Defaults to \code{"response"}.
#' @param estimator The specific estimator to use for prediction. For example, \code{"St"}, \code{"DSh"} if \code{model_class = "Multiplicative"}, etc.
#'                  This must match one of the estimators in the fitted \code{savvySh_model} object.
#'                  Defaults to the first available estimator if not specified.
#' @param ... Additional arguments.
#'
#' @return Predicted values for \code{type = "response"} or coefficients for \code{type = "coefficients"}.
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso \code{\link{savvySh}}
#' @method predict savvySh_model
#' @export
predict.savvySh_model <- function(object, newx = NULL, type = c("response", "coefficients"), estimator = NULL, ...) {
  valid_types <- c("response", "coefficients")
  if (!type %in% valid_types) {
    stop("Invalid type specified. Use 'response' or 'coefficients'.")
  }

  type <- match.arg(type, valid_types)

  if (is.null(object$coefficients) || !is.list(object$coefficients)) {
    stop("Invalid 'savvySh_model' object: coefficients are missing or improperly structured.")
  }

  if (is.null(estimator)) {
    estimator <- names(object$coefficients)[1]
    warning("No estimator specified. Defaulting to the first available estimator: ", estimator)
  }

  if (!(estimator %in% names(object$coefficients))) {
    stop("The specified estimator '", estimator, "' is not available. Choose from: ",
         paste(names(object$coefficients), collapse = ", "))
  }

  coefficients <- object$coefficients[[estimator]]
  intercept_present <- ncol(object$model) == length(coefficients)

  if (is.null(coefficients) || length(coefficients) == 0) {
    stop(paste("The coefficients for the estimator", estimator, "are missing or empty."))
  }

  if (type == "coefficients") {
    coef_names <- if (intercept_present) {
      c("(Intercept)", paste0("V", seq_len(ncol(object$model)-1)))
    } else {
      paste0("V", seq_len(length(coefficients)))
    }
    names(coefficients) <- coef_names

    return(coefficients)
  }

  if (type == "response") {
    if (is.null(newx)) {
      stop("You need to supply a matrix of new values for 'newx'.")
    }
    newx <- as.matrix(newx)
    nvars <- length(coefficients) - as.numeric(intercept_present)
    if (ncol(newx) != nvars) {
      stop("The number of variables in 'newx' must match the number of predictors in the model.")
    }

    model_intercept <- if (intercept_present) coefficients[1] else 0
    model_coefs <- if (intercept_present) coefficients[-1] else coefficients
    predictions <- model_intercept + newx %*% model_coefs

    return(predictions)
  }
}


