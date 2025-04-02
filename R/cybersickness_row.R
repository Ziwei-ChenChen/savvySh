#' Raw Physiological Measurements for Cybersickness Study
#'
#' @description Raw data from a cybersickness study including heart rate, heart rate variability, and their percent changes.
#'
#' @format A data frame with 25893 rows and 17 columns:
#' \describe{
#'   \item{Epoch}{Time interval index.}
#'   \item{HR}{Heart Rate.}
#'   \item{PC_HR}{Percent change in Heart Rate.}
#'   \item{HR_MIN}{Minimum heart rate in the epoch.}
#'   \item{HR_MAX}{Maximum heart rate in the epoch.}
#'   \item{HRV}{Heart Rate Variability.}
#'   \item{PC_HRV}{Percent change in HRV.}
#'   \item{HRV_MIN}{Minimum HRV in the epoch.}
#'   \item{HRV_MAX}{Maximum HRV in the epoch.}
#'   \item{...}{Other columns include similar physiological signals.}
#' }
"cybersickness_row"
