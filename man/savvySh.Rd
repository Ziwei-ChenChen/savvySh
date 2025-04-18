% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/savvySh.main.R
\name{savvySh}
\alias{savvySh}
\title{Slab and Shrinkage Linear Regression Estimation}
\usage{
savvySh(x, y, model_class = c("Multiplicative", "Slab", "Linear", "ShrinkageRR"),
               v = 1, lambda_vals = NULL, nlambda = 100, folds = 10,
               foldid = FALSE, include_Sh = FALSE, exclude = NULL)
}
\arguments{
\item{x}{A matrix of predictor variables.}

\item{y}{A vector of response variable.}

\item{model_class}{A character string specifying the shrinkage model to use. Options can choose from \code{"Multiplicative"},
\code{"Slab"}, \code{"Linear"}, and \code{"ShrinkageRR"}. The default is \code{"Multiplicative"}.
 If the user supplies more than one model, a warning is issued and only the first option is used.}

\item{v}{A numeric value controlling the strength of shrinkage for the \code{SR} estimator in the \code{"Slab"} model.
Must be a positive number. Default is 1.}

\item{lambda_vals}{A vector of \code{lambda} values for \code{RR}. This is used only when
multicollinearity (rank deficiency) is detected and \code{"ShrinkageRR"} is not selected.
If \code{NULL}, a default sequence is used.}

\item{nlambda}{The number of \code{lambda} values to use for cross-validation if \code{lambda_vals} is \code{NULL}.
Only used when multicollinearity is present and \code{"ShrinkageRR"} is not called. The default is \code{100}.}

\item{folds}{Number of folds for cross-validation in \code{RR}. This is applicable only if multicollinearity occurs
and \code{"ShrinkageRR"} is not chosen. The default is \code{10} and must be an integer \code{>= 3}.}

\item{foldid}{Logical. If \code{TRUE}, saves the fold assignments in the output when multicollinearity is detected
and \code{"ShrinkageRR"} is not used. The default is \code{FALSE}.}

\item{include_Sh}{Logical. If \code{TRUE}, includes the Sh estimator in the \code{"Multiplicative"} model. The default is \code{FALSE}.}

\item{exclude}{A vector specifying columns to exclude from the predictors. The default is \code{NULL}.}
}
\value{
A list containing the following elements:
\item{call}{The matched function call.}
\item{model}{The data frame of \code{y} and \code{x} used in the analysis.}
\item{\code{optimal_lambda}}{If \code{x} is full rank, this value is \code{0}.
If \code{x} is rank-deficient, it is the chosen \code{RR} \code{lambda} from cross-validation.}
\item{model_class}{The selected model class.}
\item{coefficients}{A list of estimated coefficients for each applicable estimator in the \code{model_class}.}
\item{fitted_values}{A list of fitted values for each estimator.}
\item{pred_MSE}{A list of prediction MSEs for each estimator.}
\item{ridge_results (optional)}{
 A list containing detailed results from \code{RR}, used when multicollinearity (rank deficiency)
 is detected in \code{x} and the \code{"ShrinkageRR"} is not called. This element is included only when \code{RR} is applied instead of \code{OLS}
 due to the rank deficiency of \code{x}. It contains:
 \describe{
 \item{\code{lambda_range}}{The range of \code{lambda} values used in the \code{RR} cross-validation.}
 \item{\code{cvm}}{A vector of cross-validated MSEs for each \code{lambda} in \code{lambda_range}.}
 \item{\code{cvsd}}{The standard deviation of the cross-validated MSEs for each \code{lambda}.}
 \item{\code{ridge_coefficients}}{A matrix of coefficients from \code{RR} at each \code{lambda} value,
 with each column representing the coefficients corresponding to a specific \code{lambda}.}
 }
}
}
\description{
This function estimates coefficients in a linear regression model using several shrinkage methods,
including Multiplicative Shrinkage, Slab Regression, Linear shrinkage, and Shrinkage Ridge Regression.
Each method gives estimators that balance bias and variance by applying shrinkage to the ordinary
least squares (OLS) solution. The shrinkage estimators are computed based on different assumptions
about the data.
}
\details{
The \emph{Slab and Shrinkage Linear Regression Estimation} methodology provides four classes of shrinkage estimators
that reduce variance in the OLS solution by introducing a small, structured bias. These methods handle overfitting,
collinearity, and high-dimensional scenarios by controlling how and where the coefficients are shrunk. Each class offers a distinct strategy
for controlling instability and improving mean squared error (MSE) in linear models, tailored for different modeling contexts specified
in the \code{model_class} argument. Note that if the user provides more than one option in \code{model_class}, only the first option is used,
and a warning is issued.

\strong{Model Classes:}
\describe{

  \item{\strong{Multiplicative Shrinkage:}}{
    This class includes three estimators that use the \code{OLS} coefficients as a starting point and apply
    \emph{multiplicative} adjustments:

    \describe{
      \item{\code{St - }}{\emph{Stein estimator}, which shrinks all coefficients toward zero by a single global factor.
        This aims to reduce MSE while keeping the overall bias fairly uniform across coefficients.}
      \item{\code{DSh - }}{\emph{Diagonal Shrinkage}, assigning an individual factor to each coefficient based on its
        variance. This yields more targeted shrinkage than the global approach and often achieves a lower MSE.}
      \item{\code{Sh - }}{\emph{Shrinkage estimator} that solves a Sylvester equation for a full (non-diagonal) shrinkage matrix.
        It is more flexible but also more computationally demanding. Included only if \code{include_Sh = TRUE}.}
    }
  }

  \item{\strong{Slab Regression:}}{
    \emph{Slab Regression} applies an adaptive quadratic penalty term to the \code{OLS} objective:

    \describe{
      \item{\code{SR - }}{\emph{Simple Slab Regression}, which modifies the \code{OLS} objective by
        adding a penalty in a fixed direction (often the constant vector). This penalty is controlled by \code{v}
        and does not require cross-validation. It can be viewed as a special case of the \code{generalized lasso}
        but focuses on smooth (quadratic) rather than \eqn{\ell_1}{L1} regularization.}
      \item{\code{GSR - }}{\emph{Generalized Slab Regression}, extending SR by allowing shrinkage along multiple directions.
        Typically, these directions correspond to the eigenvectors of the design covariance matrix, effectively shrinking
        principal components.}
    }
  }

  \item{\strong{Linear Shrinkage:}}{
    The \emph{Linear Shrinkage (LSh)} estimator forms a convex combination of the OLS estimator (through the origin)
    and a target estimator that assumes uncorrelated predictors (\emph{diagonal} approximation of the covariance).
    This approach is simpler than a full matrix method and is well-suited for standardized data where the intercept
    is not needed.
  }

  \item{\strong{Shrinkage Ridge Regression:}}{
    The \emph{Shrinkage Ridge Regression (SRR)} extends standard \code{RR} by shrinking the design covariance
    matrix toward a spherical target (i.e., a diagonal matrix with equal entries). This additional regularization
    stabilizes the eigenvalues and yields more robust coefficient estimates, particularly when the predictors lie
    close to a low-dimensional subspace.
  }
}
}
\references{
Asimit, V., Cidota, M. A., Chen, Z., & Asimit, J. (2025).
\emph{Slab and Shrinkage Linear Regression Estimation}.
Retrieved from \url{https://openaccess.city.ac.uk/id/eprint/35005/}
}
\author{
Ziwei Chen, Vali Asimit, Marina Anca Cidota, Jennifer Asimit\cr
Maintainer: Ziwei Chen <ziwei.chen.3@citystgeorges.ac.uk>
}
