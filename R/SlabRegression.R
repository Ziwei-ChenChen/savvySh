# Internal functions for Slab regression models: SR.
# These functions are not exported and are intended for internal use within the package's main function.

################################################################################
############################## Slab Regression #################################
################################################################################
# SR estimator
# The slab direction is u = v * 1 and the penalty is mu** = sigma^2 / (u^T est)^2,
# so SR = est - mu** / (1 + mu** a1) * Sigma^{-1} u u^T est with a1 = u^T Sigma^{-1} u.
# The estimate is the same for every v > 0. When u^T est = 0, mu** is infinite
# and SR equals est.
# Parameters:
#   v                - A positive scalar used to construct the vector u.
#   est              - Initial coefficient estimate vector.
#   sigma_square     - Variance of noise.
#   Sigma_lambda     - Regularized covariance matrix (not used here but kept for consistency).
#   Sigma_lambda_inv - Inverse of Sigma_lambda.
# Returns:
#   A vector representing the SR estimator.
SR_ost <- function(v, est, sigma_square, Sigma_lambda, Sigma_lambda_inv) {
  u <- rep(v, length(est))
  Sinv_u <- as.vector(Sigma_lambda_inv %*% u)
  a1_u <- sum(u * Sinv_u)
  beta_u <- sum(u * est)
  # mu** / (1 + mu** a1), written so that beta_u = 0 needs no special case
  scalar_value <- sigma_square / (beta_u^2 + sigma_square * a1_u)
  as.vector(est - scalar_value * beta_u * Sinv_u)
}

# GSR estimator
# One slab for each eigenvector u_l of Sigma, with mu_l = sigma^2 / (u_l^T est)^2.
# The shrinkage along u_l is (mu_l / lambda_l) / (1 + mu_l / lambda_l)
# = sigma^2 / (sigma^2 + lambda_l (u_l^T est)^2).
# Parameters:
#   est              - Initial coefficient estimate vector.
#   sigma_square     - Variance of noise.
#   Sigma_lambda     - Regularized covariance matrix.
#   Sigma_lambda_inv - Inverse of Sigma_lambda (not used here but kept for consistency).
# Returns:
#   A vector representing the GSR estimator.
GSR_ost <- function(est, sigma_square, Sigma_lambda, Sigma_lambda_inv) {
  eigen_res <- eigen(Sigma_lambda, symmetric = TRUE)
  eigenvalues <- eigen_res$values
  U <- eigen_res$vectors
  beta_proj <- as.vector(crossprod(U, est))
  scalar_coeff <- sigma_square / (sigma_square + eigenvalues * beta_proj^2)
  as.vector(est - U %*% (scalar_coeff * beta_proj))
}

