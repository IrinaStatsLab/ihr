#' Calculate heart rate reserve

#' @description
#' Computes the heart rate reserve (HRR), defined as the difference between the maximum heart rate and resting heart rate.
#' For details on computing resting heart rate, see \code{\link{calculate_RHR}}

#' @usage
#' calculate_HRR(data, method, quantile_val, tz)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param method \strong{Default: "max-RHR".} A choice for the user to choose which formula they want to use: HRR = max - Resting heart rate ("max-RHR") or HRR = max - min ("max-min")
#' @param quantile_val A choice for the user to choose which quantile value they want to use(most common 1, if there's strange max value, recommend to use 0.99)
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Heart rate reserve as a predictor of cardiovascular and all-cause mortality in men,
#' \doi{10.1249/01.MSS.0000038959.67619.30}.

#' @export
#' @examples
#' data(example_heart_1)
#' calculate_HRR(example_heart_1, 'max-min')
#' calculate_HRR(example_heart_1, quantile_val = 0.99)

calculate_HRR <- function(data, method = "max-RHR", quantile_val = 1, tz = "") {
  time = hr = id = max_hr = min_hr = RHR = NULL
  rm(list = c('time', 'hr', 'id', 'max_hr', 'min_hr', 'RHR'))
  data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S", tz = tz)

  RHR_data <- calculate_RHR(data, tz = tz)

  # If no RHR data, return NULL
  if (is.null(RHR_data)) {
    print("HRR cannot be calculated because RHR is missing.")
    return(NULL)
  }

  # Validate quantile value
  if (!is.numeric(quantile_val) || quantile_val <= 0 || quantile_val > 1) {
    stop("quantile_val must be a numeric value between 0 (exclusive) and 1 (inclusive).")
  }

  HRR_hr_data <- data |>
    dplyr::group_by(id) |>
    dplyr::summarize(max_hr = quantile(hr, quantile_val, na.rm = TRUE),
                     min_hr = min(hr, na.rm = TRUE),
                     .groups = 'drop')

  HRR_hr_data <- dplyr::left_join(HRR_hr_data, RHR_data, by = "id")

  # Choose calculation method
  if (method == "max-RHR") {
    HRR_hr_data <- HRR_hr_data |>
      dplyr::mutate(HRR = max_hr - RHR)
  } else if (method == "max-min") {
    HRR_hr_data <- HRR_hr_data |>
      dplyr::mutate(HRR = max_hr - min_hr)
  } else {
    stop("Invalid method. Choose either 'max-RHR' or 'max-min'.")
  }

  return(HRR_hr_data)
}
