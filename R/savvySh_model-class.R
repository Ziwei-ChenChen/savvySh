#' Internal Class for Slab and Shrinkage Models
#'
#' @description
#' This alias documents the S3 class \code{savvySh_model}, which is returned by
#' the \code{\link{savvySh}} function. Instances of this class store the fitted
#' shrinkage estimators, predicted values, model information, etc.
#'
#' @details
#' Typically, you do not construct \code{savvySh_model} objects directly.
#' Instead, they are produced when you run \code{\link{savvySh}(...)}.
#' Several S3 methods such as \code{\link{coef.savvySh_model}},
#' \code{\link{predict.savvySh_model}}, and \code{\link{summary.savvySh_model}}
#' exist for extracting information from or summarizing a \code{savvySh_model}.
#'
#' @name savvySh_model
#' @aliases savvySh_model
#' @keywords internal
NULL
