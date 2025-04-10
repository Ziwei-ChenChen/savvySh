#' @title Predict Method for Slab and Shrinkage Linear Regression Models
#'
#' @description
#' Generate predictions (fitted values) or extract coefficients from a \code{"savvySh_model"}
#' object produced by \code{\link{savvySh}}. This function allows you to specify
#' which shrinkage estimator to use if multiple estimators are stored in the model.
#'
#' @param object A fitted \code{"savvySh_model"} object returned by \code{\link{savvySh}}.
#' @param newx A numeric matrix of new data (with the same number of predictors) for which to generate predictions.
#'   Required if \code{type = "response"}. If \code{type = "coefficients"}, this argument is ignored.
#' @param type A character string specifying whether to return fitted values (\code{"response"})
#'   or regression coefficients (\code{"coefficients"}). Defaults to \code{"response"}.
#' @param estimator A character string naming which shrinkage estimator to use.
#'   Must match one of the estimators in \code{object$coefficients}. If \code{NULL},
#'   defaults to the first available estimator in \code{object$coefficients}.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' The behavior depends on the value of \code{type}:
#' \describe{
#'   \item{\code{"response"}:}{Returns predicted values for new data supplied via \code{newx}.}
#'   \item{\code{"coefficients"}:}{Returns the estimated coefficient vector for the requested \code{estimator}.}
#' }
#' If no \code{estimator} is specified, it defaults to the first one in the model object.
#' For instance, a \code{savvySh_model} fitted with \code{model_class = "Multiplicative"} typically has
#' \code{"St"}, \code{"DSh"}, and possibly \code{"Sh"} if \code{include_Sh = TRUE}.
#'
#' @return
#' If \code{type = "response"}, a numeric vector of predicted values.
#' If \code{type = "coefficients"}, a named numeric vector of regression coefficients for the chosen estimator.
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso
#'   \code{\link{savvySh}} for fitting Slab and Shrinkage linear models,
#'   \code{\link{coef.savvySh_model}} for a direct way to extract coefficients without specifying \code{type}.
#
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


