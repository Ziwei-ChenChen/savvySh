
# savvySh: Shrinkage Estimators for Linear Regression

The `savvySh` package provides a unified interface for fitting shrinkage estimators in linear regression, 
particularly useful in the presence of multicollinearity or high-dimensional covariates. 
It supports four shrinkage classes: Multiplicative Shrinkage, Slab Regression, Linear Shrinkage (LSh), and Shrinkage Ridge Regression (SRR).
These methods improve on classical OLS by trading a small amount of bias for a significant reduction in variance.

This package builds on theoretical work discussed in:

Asimit, V., Cidota, M. A., Chen, Z., & Asimit, J. (2025). *Slab and Shrinkage Linear Regression Estimation*.

Website available at: https://Ziwei-ChenChen.github.io/savvySh; besides, if you want to run the real data analysis on this [website](https://Ziwei-ChenChen.github.io/savvySh), please follow dependency from GitHub:
```r
remotes::install_github("Ziwei-ChenChen/ShirnkageGLMs")
```

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

`savvySh` provides four types of shrinkage estimators to improve regression performance by reducing variance:

- **Multiplicative Shrinkage:**  
  Shrinks OLS coefficients by multiplying them with estimated factors. Includes:  
  - **Stein (St):** Uses a single global factor.  
  - **Diagonal Shrinkage (DSh):** Shrinks each coefficient separately.  
  - **Shrinkage (Sh):** Uses a (non-diagonal) matrix shrinkage operator, estimated by solving a Sylvester equation.

- **Slab Regression:**  
  Adds a penalty to guide shrinkage:  
  - **SR:** Shrinks toward a fixed direction (e.g., vector of ones).  
  - **GSR:** Shrinks toward multiple directions (e.g., eigenvectors)

- **Linear Shrinkage (LSh):**  Combines the OLS estimate with a version that assumes covariates are uncorrelated. Best used when covariates are standardized.

- **Shrinkage Ridge Regression (SRR):** Modifies ridge regression by shrinking the covariance matrix toward a scaled identity matrix.
  The shrinkage level is chosen to minimize prediction error.

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

