#' @title Summarize a Slab and Shrinkage Linear Regression Model
#'
#' @description
#' Provides a comprehensive summary of each estimator within a \code{\link{savvySh_model}} object, including:
#' estimated coefficients, confidence intervals, residual statistics, R-squared measures, F-statistics,
#' and information criteria (AIC, BIC).
#'
#' @param object A fitted model object of class \code{savvySh_model}, produced by \code{\link{savvySh}}.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' For each estimator (e.g., \code{"St"}, \code{"DSh"}, \code{"SR"}, \code{"GSR"}, \code{"Sh"}, etc.) present in the
#' \code{savvySh_model} object, this function computes:
#' \itemize{
#'   \item Residual distribution summary (quantiles).
#'   \item Estimated coefficient table (Estimates, standard errors, t-values, p-values, confidence intervals).
#'   \item Residual standard error and degrees of freedom.
#'   \item R-squared and adjusted R-squared.
#'   \item F-statistic (testing overall regression significance) and p-value.
#'   \item AIC, BIC, and deviance to gauge model fit.
#' }
#'
#' These results are printed to the console for each estimator in turn. Users can compare how each
#' shrinkage approach performs with respect to fit and significance. Internally, it relies on
#' \code{summaryStats_savvySh()} in \code{summaryStats} for key computations.
#'
#' @return
#' Prints a multi-section summary for each estimator within the model.
#' No value is returned, but the summary is printed to the console.
#'
#' @author
#' Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
#' Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
#'
#' @seealso
#'   \code{\link{savvySh}} for fitting slab and shrinkage models,
#'   \code{\link{predict.savvySh_model}} for making predictions,
#'   \code{\link{coef.savvySh_model}} for extracting coefficients directly.
#'
#' @importFrom stats pf predict pt qnorm quantile qt
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

