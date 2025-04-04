#' Raw Physiological Measurements for Cybersickness Study
#'
#' Raw data from a cybersickness study including heart rate, heart rate variability, and their percent changes.
#'
#' A data frame with 25893 rows and 17 columns:
#' - Epoch: Time interval index.
#' - HR: Heart Rate.
#' - PC_HR: Percent change in Heart Rate.
#' - HR_MIN: Minimum heart rate in the epoch.
#' - HR_MAX: Maximum heart rate in the epoch.
#' - HRV: Heart Rate Variability.
#' - PC_HRV: Percent change in HRV.
#' - HRV_MIN: Minimum HRV in the epoch.
#' - HRV_MAX: Maximum HRV in the epoch.
#' - ...: Other columns include similar physiological signals.
#'
#' The original data can be found at:
#' \url{https://github.com/shovonis/CyberSicknessClassification/blob/master/lstm_regression/data/raw_data.csv}
#'
"cybersickness_raw"
