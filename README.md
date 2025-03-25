
# savvySh: Shrinkage Methods for Linear Regression Estimation

The `savvySh` package provides a unified interface for fitting shrinkage estimators in linear regression, 
particularly useful in the presence of multicollinearity or high-dimensional covariates. 
It supports four shrinkage classes: *Multiplicative Shrinkage*, *Slab Regression*, *Linear Shrinkage*, and *Shrinkage Ridge Regression*.
These methods improve on the classical *ordinary least squares (OLS)* estimator by trading a small amount of bias for a significant reduction in variance.

This package builds on theoretical work discussed in:

Asimit, V., Cidota, M. A., Chen, Z., & Asimit, J. (2025). [*Slab and Shrinkage Linear Regression Estimation*](http//...).

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

`savvySh` provides several shrinkage estimators designed to improve regression accuracy by reducing MSE:

- **Multiplicative Shrinkage:** Applies shrinkage by multiplying the OLS estimates with data-driven factors: 

  - **Stein (St):** Applies a single global shrinkage factor to all coefficients.  
  
  - **Diagonal Shrinkage (DSh):** Applies a separate factor to each coefficient. 
  
  - **Shrinkage (Sh):** Uses a full matrix shrinkage operator estimated by solving a *Sylvester equation*.

- **Slab Regression:** Adds structured shrinkage based on penalty terms:  

  - **Slab Regression (SR):** Shrinks toward a fixed target direction (e.g., a vector of ones).  
  
  - **Generalized Slab Regression (GSR):** Shrinks toward multiple directions (e.g., eigenvectors).
  
- **Linear Shrinkage (LSh):** Takes a weighted average of the OLS estimator and a target estimator and is useful for standardized data.
  
- **Shrinkage Ridge Regression (SRR):** Modifies *Ridge Regression (RR)* by shrinking the sample covariance matrix toward a multiple of the identity matrix.

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

