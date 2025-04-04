
#' Lagged Physiological Covariates for Cybersickness Prediction
#'
#' This dataset is derived from cybersickness_row by preprocessing the original physiological measurements.
#' For each of the 22 physiological signals, 10 lagged features are created for each participant and time point.
#' These lagged covariates, from time steps t-1 to t-10, are used as predictors for regression modeling in cybersickness studies.
#'
#' The preprocessing includes:
#' 1. Creating lagged covariates: For each physiological signal Xi (for i = 2 to 23), new variables are created for values at previous time steps, including Xi(t-1), Xi(t-2), ..., Xi(t-10).
#' 2. To avoid overlap between outcome and covariates, the last 10 rows for each participant are removed.
#'
#' A data frame with 25663 rows and 132 columns:
#' Intercept: Intercept column (all 1s).
#' X2(t-10) to X23(t-1): Lagged features for 22 physiological measurements, 10 lags per variable.
#'
"cybersickness_10lags"
