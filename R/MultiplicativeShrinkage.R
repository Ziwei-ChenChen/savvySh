# Internal functions for multiplicative shrinkage models: St, DSh, and Sh.
# These functions are not exported and are intended for internal use within the package's main function.

################################################################################
########################## Multiplicative Shrinkage ############################
################################################################################
# Main St function.
# Computes the Stein estimator (St) using multiplicative shrinkage.
# Parameters:
#   est              - Initial coefficient estimate vector.
#   sigma_square     - Variance of noise.
#   Sigma_lambda_inv - Inverse of the regularized covariance matrix.
# Returns:
#   A vector representing the St estimator.
St_ost <- function(est, sigma_square, Sigma_lambda_inv) {
  M0 <- sigma_square * sum(diag(Sigma_lambda_inv))
  a_star <- sum(est^2) / (sum(est^2) + M0)
  as.vector(a_star * est)
}

# Main DSh function.
# Computes the Diagonal Shrinkage (DSh) estimator using multiplicative shrinkage.
# Parameters:
#   est              - Initial coefficient estimate vector.
#   sigma_square     - Variance of noise.
#   Sigma_lambda_inv - Inverse of the regularized covariance matrix.
# Returns:
#   A vector representing the DSh estimator.
DSh_ost <- function(est, sigma_square, Sigma_lambda_inv) {
  b_star <- (est^2) / (est^2 + sigma_square * diag(Sigma_lambda_inv))
  as.vector(b_star * est)
}

# Main Sh function.
# Computes the Shrinkage (Sh) estimator C* est, where
# C* = est est^T Sigma / (sigma^2 + est^T Sigma est). With q = est^T Sigma est
# this is est * q / (sigma^2 + q).
# Parameters:
#   est              - Initial coefficient estimate vector.
#   sigma_square     - Variance of noise.
#   Sigma_lambda     - Regularized covariance matrix.
# Returns:
#   A vector representing the Sh estimator.
Sh_ost <- function(est, sigma_square, Sigma_lambda) {
  q <- as.numeric(crossprod(est, Sigma_lambda %*% est))
  as.vector(q / (sigma_square + q) * est)
}

