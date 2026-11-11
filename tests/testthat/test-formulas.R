library(testthat)
library(savvySh)

set.seed(2024)
n <- 80
p <- 6
x <- matrix(rnorm(n * p), n, p) %*% chol(0.5^abs(outer(1:p, 1:p, "-")))
y <- drop(1 + x %*% seq(-1, 1.5, length.out = p) + rnorm(n, sd = 2))

xt <- cbind(1, x)
S <- crossprod(xt)
b <- drop(solve(S, crossprod(xt, y)))
s2 <- sum((y - xt %*% b)^2) / (n - p - 1)

test_that("Sh equals C* times the OLS estimate", {
  C <- b %*% t(b) %*% S / (s2 + drop(t(b) %*% S %*% b))
  fit <- savvySh(x, y, model_class = "Multiplicative", include_Sh = TRUE)
  expect_equal(fit$coefficients$Sh, drop(C %*% b), tolerance = 1e-10)
})

test_that("SR uses mu** = sigma^2 / (1'beta)^2 and does not depend on v", {
  one <- rep(1, p + 1)
  mu <- s2 / sum(b)^2
  a1 <- drop(t(one) %*% solve(S) %*% one)
  sr <- drop((diag(p + 1) - mu / (1 + mu * a1) * solve(S) %*% matrix(1, p + 1, p + 1)) %*% b)
  fit1 <- savvySh(x, y, model_class = "Slab", v = 1)
  fit3 <- savvySh(x, y, model_class = "Slab", v = 3)
  expect_equal(fit1$coefficients$SR, sr, tolerance = 1e-10)
  expect_equal(fit3$coefficients$SR, sr, tolerance = 1e-10)
})

test_that("GSR shrinks each eigen-direction by sigma^2 / (sigma^2 + lambda (u'beta)^2)", {
  e <- eigen(S, symmetric = TRUE)
  bu <- drop(crossprod(e$vectors, b))
  gsr <- drop(e$vectors %*% ((1 - s2 / (s2 + e$values * bu^2)) * bu))
  fit <- savvySh(x, y, model_class = "Slab")
  expect_equal(fit$coefficients$GSR, gsr, tolerance = 1e-10)
})

test_that("LSh uses t3 = beta' M'M beta and sigma^2 = RSS / (n - p - 1)", {
  xc <- scale(x, scale = FALSE)
  yc <- y - mean(y)
  Sc <- crossprod(xc)
  bc <- drop(solve(Sc, crossprod(xc, yc)))
  s2c <- sum((yc - xc %*% bc)^2) / (n - p - 1)
  St_inv <- diag(1 / diag(Sc))
  M <- St_inv %*% Sc - diag(p)
  t1 <- s2c * sum(diag(St_inv))
  t2 <- s2c * sum(diag(solve(Sc)))
  t3 <- drop(t(bc) %*% t(M) %*% M %*% bc)
  rho <- (t2 - t1) / (t2 - t1 + t3)
  lsh <- drop((rho * St_inv %*% Sc + (1 - rho) * diag(p)) %*% bc)
  fit <- savvySh(x, y, model_class = "Linear")
  expect_equal(fit$coefficients$LSh, lsh, tolerance = 1e-10)
})

test_that("SRR minimizes H(rho) with the fourth power in its last term", {
  e <- eigen(S, symmetric = TRUE)
  w <- drop(crossprod(e$vectors, crossprod(xt, y)))
  V <- w^2
  v_bar <- mean(e$values)
  H <- function(rho) {
    d <- (1 - rho) * e$values + rho * v_bar
    (sum(y^2) - 2 * sum(V / d) + sum(e$values * V / d^2)) / (n - p - 1) * sum(e$values / d^2) +
      rho^2 * sum((e$values - v_bar)^2 * V / d^4)
  }
  rho <- optimize(H, c(0, 1), tol = sqrt(.Machine$double.eps))$minimum
  srr <- drop(e$vectors %*% (w / ((1 - rho) * e$values + rho * v_bar)))
  fit <- savvySh(x, y, model_class = "ShrinkageRR")
  expect_equal(fit$coefficients$SRR, srr, tolerance = 1e-8)
})

test_that("ridge_penalty reproduces the glmnet ridge fit", {
  x_rd <- cbind(x, x[, 1] + x[, 2])
  lam <- 0.2
  g <- glmnet::glmnet(x_rd, y, alpha = 0, lambda = lam, thresh = 1e-20, maxit = 1e7)
  xt_rd <- cbind(1, x_rd)
  P <- diag(c(0, ridge_penalty(x_rd, y, lam)))
  b_closed <- drop(solve(crossprod(xt_rd) + P, crossprod(xt_rd, y)))
  expect_equal(as.vector(coef(g)), b_closed, tolerance = 1e-6)
})

test_that("the ridge fallback gives finite estimates and returns its folds", {
  x_rd <- cbind(x, x[, 1] + x[, 2])
  fit <- suppressWarnings(savvySh(x_rd, y, model_class = "Multiplicative", include_Sh = TRUE, foldid = TRUE))
  expect_length(fit$fold_assignments, n)
  expect_true(all(is.finite(unlist(fit$coefficients))))
})
