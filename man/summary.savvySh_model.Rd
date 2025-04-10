% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.savvySh.R
\name{summary.savvySh_model}
\alias{summary.savvySh_model}
\title{Summarize a Slab and Shrinkage Linear Regression Model}
\usage{
\method{summary}{savvySh_model}(object, ...)
}
\arguments{
\item{object}{A fitted model object of class \code{savvySh_model}, produced by \code{\link{savvySh}}.}

\item{...}{Additional arguments (currently unused).}
}
\value{
Prints a multi-section summary for each estimator within the model.
No value is returned, but the summary is printed to the console.
}
\description{
Provides a comprehensive summary of each estimator within a \code{\link{savvySh_model}} object, including:
estimated coefficients, confidence intervals, residual statistics, R-squared measures, F-statistics,
and information criteria (AIC, BIC).
}
\details{
For each estimator (e.g., \code{"St"}, \code{"DSh"}, \code{"SR"}, \code{"GSR"}, \code{"Sh"}, etc.) present in the
\code{savvySh_model} object, this function computes:
\itemize{
  \item Residual distribution summary (quantiles).
  \item Estimated coefficient table (Estimates, standard errors, t-values, p-values, confidence intervals).
  \item Residual standard error and degrees of freedom.
  \item R-squared and adjusted R-squared.
  \item F-statistic (testing overall regression significance) and p-value.
  \item AIC, BIC, and deviance to gauge model fit.
}

These results are printed to the console for each estimator in turn. Users can compare how each
shrinkage approach performs with respect to fit and significance. Internally, it relies on
\code{summaryStats_savvySh} for key computations.
}
\seealso{
\code{\link{savvySh}} for fitting slab and shrinkage models,
  \code{\link{predict.savvySh_model}} for making predictions,
  \code{\link{coef.savvySh_model}} for extracting coefficients directly.
}
\author{
Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
}
