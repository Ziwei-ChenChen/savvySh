#' @title Print a Slab and Shrinkage Model Summary
#'
#' @description
#' Displays a concise summary of a fitted \code{\link{savvySh_model}} object, including the original
#' function call, the chosen model class, the number of non-zero coefficients per estimator, and
#' the optimal \code{lambda} value (if applicable). Additionally, the coefficients for each estimator
#' are printed with user-specified precision.
#'
#' @param x A fitted \code{"savvySh_model"} object returned by \code{\link{savvySh}}.
#' @param digits An integer specifying how many significant digits to display when printing
#'   coefficients and \code{lambda}. Defaults to \code{max(3, getOption("digits") - 3)}.
#' @param ... Additional arguments passed to \code{\link[base]{print}} (currently unused).
#'
#' @details
#' This print method aims to provide a quick diagnostic of the fitted model, indicating how many
#' coefficients each shrinkage estimator shrank exactly to zero (if any), and what the final
#' \code{lambda} parameter was, in the case of rank-deficient data. It also lists the intercept
#' and predictor names next to their estimates for each estimator. The function invisibly returns
#' a summary \code{data.frame} containing these key metrics.
#'
#' @return
#' Invisibly returns a \code{data.frame} summarizing each estimator's name, number of non-zero
#' coefficients, and the final \code{optimal_lambda} (if any). This can be captured and further
#' processed by the user if desired.
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso
#'   \code{\link{savvySh}} for creating a slab and shrinkage model,
#'   \code{\link{coef.savvySh_model}} and \code{\link{predict.savvySh_model}} for extracting model coefficients
#'   and making predictions.
#'
#' @method print savvySh_model
#' @export
print.savvySh_model <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ", deparse(x$call), "\n\n")
  cat("Model Class: ", x$model_class, "\n\n")

  if (is.null(x$coefficients) || !is.list(x$coefficients)) {
    stop("The 'savvySh_model' object does not contain valid coefficients.")
  }

  summary_list <- lapply(names(x$coefficients), function(estimator) {
    coef <- x$coefficients[[estimator]]
    list(
      "Estimator" = estimator,
      "Non-Zero Coefficients" = sum(coef != 0),
      "Optimal Lambda" = ifelse(is.null(x$optimal_lambda), NA, signif(x$optimal_lambda, digits))
    )
  })

  summary_output <- do.call(rbind, lapply(summary_list, as.data.frame))
  print(summary_output, row.names = FALSE)

  cat("\nCoefficients:\n")
  for (estimator in names(x$coefficients)) {
    cat("\nEstimator: ", estimator, "\n")
    coef <- x$coefficients[[estimator]]
    coef_names <- if (length(coef) == ncol(x$model)) {
      c("(Intercept)", colnames(x$model)[-1])
    } else {
      paste0("V", seq_along(coef))
    }
    coef_df <- data.frame(
      Coefficient = coef_names,
      Estimate = signif(coef, digits)
    )
    print(coef_df, row.names = FALSE)
  }

  invisible(summary_output)
}



