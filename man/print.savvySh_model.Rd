% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.savvySh.R
\name{print.savvySh_model}
\alias{print.savvySh_model}
\title{Print a Slab and Shrinkage Model Summary}
\usage{
\method{print}{savvySh_model}(x, digits = max(3, getOption("digits") - 3), estimator = NULL, ...)
}
\arguments{
\item{x}{A fitted \code{savvySh_model} object returned by \code{savvySh}.}

\item{digits}{An integer specifying the number of significant digits to display when printing
coefficients and \code{lambda}. Defaults to \code{max(3, getOption("digits") - 3)}.}

\item{estimator}{A character vector naming one or more estimators for which coefficients should be printed.
Valid names are those present in \code{x$coefficients} (e.g., \code{"St"}, \code{"DSh"}, \code{"Sh"},
\code{"SR"}, \code{"GSR"}, or \code{"ShrinkageRR"}). If \code{NULL}, coefficients for all estimators are printed.}

\item{...}{Additional arguments passed to \code{\link[base]{print}} (currently unused).}
}
\value{
Invisibly returns a \code{data.frame} summarizing each selected estimator's name, number of non-zero
coefficients, and the final \code{optimal_lambda} (if any).
}
\description{
Displays a concise summary of a fitted \code{savvySh_model} object, including the original
function call, the chosen model class, the number of non-zero coefficients per estimator,
and the optimal \code{lambda} value (if applicable). Additionally, it prints the coefficients for
the specified estimator(s) with user-specified precision.
}
\details{
This print method provides a quick diagnostic of the fitted model by showing:
\describe{
  \item{Summary Metrics}{A table that includes, for each estimator, the number of non-zero coefficients
      and the optimal \code{lambda} (if applicable).}
  \item{Coefficients}{For each selected estimator, the coefficients are printed with appropriate names:
      if an intercept is present, it is labeled \code{(Intercept)} and the remaining
      coefficients are labeled according to the predictor names.}
}
If the user does not specify an estimator using the \code{estimator} argument, the function prints
information for all available estimators stored in the model. If one or more estimators are specified,
only those are printed, after verifying that they exist in \code{x$coefficients}.

The method invisibly returns a summary \code{data.frame} containing key metrics for each estimator.
}
\seealso{
\code{\link{savvySh}} for fitting slab and shrinkage linear models,
  \code{\link{coef.savvySh_model}} and \code{\link{predict.savvySh_model}} for extracting coefficients
  and generating predictions.
}
\author{
Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
}
