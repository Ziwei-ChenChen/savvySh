
# savvySh: Shrinkage Estimators for Linear Regression

The `savvySh` package provides a unified interface for fitting shrinkage estimators in linear regression, 
particularly useful in the presence of multicollinearity or high-dimensional covariates. 
It supports four shrinkage classes: Multiplicative Shrinkage, Slab Regression, Linear Shrinkage, and Shrinkage Ridge Regression (SRR).
These methods improve on classical OLS by trading a small amount of bias for a significant reduction in variance.

This package builds on theoretical work discussed in:

Asimit, V., Cidota, M. A., Chen, Z., & Asimit, J. (2025). *Slab and Shrinkage Linear Regression Estimation*.

Website available at: https://Ziwei-ChenChen.github.io/savvySh

## Installation Guide

Install the development version of `savvySh` from GitHub using:
```r
# install.packages("devtools")
devtools::install_github("Ziwei-ChenChen/savvySh")
```
Once installed, load the package:
```r
library(savvySh)
```

## Features

The package includes:

- **Multiplicative Shrinkage Estimators:** Stein (St), Diagonal Shrinkage (DSh), and matrix-based Shrinkage (Sh).

- **Slab Regression Estimators:** Simple Slab Regression (SR) and Generalized Slab Regression (GSR).

- **Linear Shrinkage (LSh):** Shrinking toward an uncorrelated target using a convex combination.

- **Shrinkage Ridge Regression (SRR):** Generalized ridge-type estimator with a covariance shrinkage component.

All shrinkage factors are computed in closed form (except SRR, which optimizes shrinkage intensity numerically).


## Usage

This is a basic example which shows you how to solve a common problem:

``` r
# Simulated example
set.seed(123)
x <- matrix(rnorm(100 * 10), 100, 10)
y <- rnorm(100)

# Fit shrinkage estimators
fit <- savvySh(x, y, model_class = "Multiplicative")

# Extract coefficients
coef(fit, estimator = "St")
coef(fit, estimator = "DSh")
```
## Authors

- Ziwei Chen – ziwei.chen.3@bayes.city.ac.uk

- Vali Asimit – asimit@city.ac.uk

- Marina Anca Cidota – cidota@fmi.unibuc.ro

- Jennifer Asimit – jennifer.asimit@mrc-bsu.cam.ac.uk

## License
This package is licensed under the MIT License.

