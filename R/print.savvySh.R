#' Print a Slab and Shrinkage Linear Regression Model Object
#'
#' Print a summary of the Slab and Shrinkage linear regression model object.
#'
#' @details
#' The function prints the call that produced the \code{savvySh_model} object and a summary of the model,
#' including the selected model class, the number of non-zero coefficients for each estimator, and
#' if applicable, the optimal lambda value.
#'
#' @param x Fitted \code{"savvySh_model"} object.
#' @param digits Significant digits in the printout.
#' @param ... Additional print arguments.
#'
#' @return Invisibly returns a data frame summarizing the model (number of coefficients and lambda values, if any).
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso \code{\link{savvySh}}
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



