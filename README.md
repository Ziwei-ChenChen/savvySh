
# savvySh: Shrinkage Methods for Linear Regression Estimation

The `savvySh` package provides a unified interface for fitting shrinkage estimators in linear regression, 
particularly useful in the presence of multicollinearity or high-dimensional covariates. 
It supports four shrinkage classes: *Multiplicative Shrinkage*, *Slab Regression*, *Linear Shrinkage*, and *Shrinkage Ridge Regression*.
These methods improve on the classical *ordinary least squares (OLS)* estimator by trading a small amount of bias for a significant reduction in variance.

This package builds on theoretical work discussed in:

Asimit, V., Cidota, M. A., Chen, Z., & Asimit, J. (2025). [*Slab and Shrinkage Linear Regression Estimation*](http//...).

The official documentation site is available at: https://Ziwei-ChenChen.github.io/savvySh

If you are interested in applying shrinkage methods within generalized linear models (GLMs), please refer to the companion package [`ShrinkageGLMs`](https://github.com/Ziwei-ChenChen/ShrinkageGLMs). You can install it directly from GitHub:

```r
remotes::install_github("Ziwei-ChenChen/ShrinkageGLMs")
```
This will enable you to explore real-data GLM examples shown in the vignette section of the documentation website.

In addition, shrinkage-based methods have also been applied in genetic fine-mapping. For code and examples in that context, please see: [`flashfm-savvySh`](https://github.com/jennasimit/flashfm-savvySh) – A dedicated repository demonstrating statistical fine-mapping using shrinkage estimators.


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

This is a basic example that shows you how to solve a common problem:

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

