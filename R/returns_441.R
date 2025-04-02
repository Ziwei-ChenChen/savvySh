#' Stock Return Panel: 441 Stocks Across 6037 Dates
#'
#' This dataset contains returns for 441 stocks (identified by their PERMNO codes),
#' adjusted for dividends, over 6037 time periods. Each column corresponds to a unique stock,
#' and the first column is the date of observation.
#'
#' @format A data frame with 6037 rows and 442 columns:
#' \describe{
#'   \item{Date}{Date of the return observation (format: YYYY-MM-DD).}
#'   \item{PERMNO_*}{Each remaining column corresponds to one stock's return.}
#' }
#'
"returns_441"
