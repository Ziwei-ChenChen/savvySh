#' Summary of a Fitted Slab and Shrinkage Linear Regression Model with Statistics
#'
#' This function provides a summary of the fitted \code{savvySh_model}, displaying
#' the estimated coefficients, residuals, standard errors, R-squared, and F-statistics.
#'
#' @param object A fitted model object of class \code{savvySh_model}.
#' @param ... Additional arguments (currently unused).
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@bayes.city.ac.uk>
#'
#' @seealso \code{\link{savvySh}}
#' @importFrom stats pf predict pt qnorm quantile
#' @method summary savvySh_model
#' @export
summary.savvySh_model <- function(object, ...) {
  cat("Summary of savvySh Model\n")
  cat("===================================================================\n\n")
  cat("Call:\n")
  print(object$call)
  cat("\n")

  x <- as.matrix(object$model[-1])
  y <- object$model[, 1]

  for (estimator in names(object$coefficients)) {
    cat("\nEstimator:", estimator, "\n")
    coefficients <- object$coefficients[[estimator]]
    intercept_present <- length(coefficients) == (ncol(x) + 1)
    stats <- summaryStats_savvySh(x, y, coefficients)

    cat("\nResiduals:\n")
    print(stats$residual_quants)
    signif_codes <- add_significance_codes(stats$p_values)
    formatted_p_values <- format_p_values(stats$p_values)

    coef_names <- if (intercept_present) c("(Intercept)", colnames(x)) else colnames(x)
    coef_table <- data.frame(
      Estimate = round(coefficients, 4),
      `Std. Error` = round(stats$std_err, 4),
      `t value` = round(stats$t_values, 4),
      `Pr(>|t|)` = formatted_p_values,
      `2.5 %` = round(stats$confint_lower, 4),
      `97.5 %` = round(stats$confint_upper, 4),
      `Signif.` = signif_codes,
      check.names = FALSE
    )
    rownames(coef_table) <- coef_names

    cat("\nCoefficients:\n")
    print(coef_table, row.names = TRUE)

    cat("\nResidual standard error:", round(stats$residual_se, 4), "on", stats$df_residual, "degrees of freedom\n")
    cat("Multiple R-squared:", round(stats$r_squared, 4), ", Adjusted R-squared:", round(stats$adj_r_squared, 4), "\n")
    cat("F-statistic:", round(stats$f_statistic, 4), "on", length(coefficients) - 1, "and", stats$df_residual,
        "DF, p-value:", format_p_values(stats$f_p_value), "\n")
    cat("AIC:", round(stats$AIC, 4), ", BIC:", round(stats$BIC, 4), ", Deviance:", round(stats$deviance, 4), "\n")
    cat("===================================================================\n")
  }
}

