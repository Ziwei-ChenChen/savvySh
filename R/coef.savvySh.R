#' @title Extract Coefficients for a Slab and Shrinkage Model
#'
#' @description
#' Extracts the estimated regression coefficients from a \code{\link{savvySh_model}} object,
#' specifying which shrinkage estimator to return, if no estimator specified,
#' then return the fist avaliable estimator for the chosen \code{model_class}.
#'
#' @param object A fitted model object of class \code{savvySh_model} (created by \code{\link{savvySh}}).
#' @param estimator A character string naming which estimator's coefficients to extract.
#'   Examples include \code{"St"}, \code{"DSh"}, \code{"Sh"}, \code{"SR"}, \code{"GSR"}, and so on.
#'   If \code{NULL}, the first available estimator is used.
#' @param ... Additional arguments passed to \code{\link{predict.savvySh_model}}.
#'
#' @details
#' This function calls \code{\link{predict.savvySh_model}} internally with \code{type = "coefficients"}
#' to retrieve the requested coefficient vector. If an \code{estimator} is not supplied, it defaults
#' to the first available estimator in the fitted model (for example, \code{"St"}, \code{"DSh"},
#' \code{"SR"}, \code{"GSR"}, etc.). If the requested estimator does not exist in the model object,
#' an error is raised.
#'
#' @return A named numeric vector of regression coefficients. If the model includes an intercept,
#'   it will appear as \code{(Intercept)} in the returned vector.
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso
#'   \code{\link{predict.savvySh_model}} for related predictions,
#'   \code{\link{savvySh}} for fitting a slab and shrinkage model.
#'
#' @importFrom stats predict
#' @method coef savvySh_model
#' @export
coef.savvySh_model <- function(object, estimator = NULL, ...) {
  predict(object, type = "coefficients", estimator = estimator, ...)
}


