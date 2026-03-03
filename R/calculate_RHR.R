#' Calculate resting heart rate

#' @description
#' Computes the resting heart rate (RHR) according to the mean/min heart rate
#' value between 3am to 7am. See reference for details on computing RHR

#' @usage
#' calculate_RHR(data, method, tz)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param method \strong{Default: "mean".} Method for calculating RHR. Must be either "mean" for average or "min" for minimum HR during 3am-7am
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Measure by measure: Resting heart rate across the 24-hour cycle,
#' \doi{10.1371/journal.pdig.0000236}.

#' @export
#' @examples
#' data(example_heart_1)
#' calculate_RHR(example_heart_1)


calculate_RHR <- function(data, method = c("mean", "min"), tz = "") {
  time = hr = id = hour = NULL
  rm(list = c('time', 'hr', 'id', 'hour'))
  method = match.arg(method, c("mean", "min"))

  data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S", tz = tz)
  data$hour <- as.numeric(format(data$time, "%H"))
  filtered_data <- subset(data, hour >= 3 & hour < 7)

  if (nrow(filtered_data) == 0) {
    print("No data between 03:00 and 07:00")
    return(NULL)  # Return NULL to indicate no data
  }


  if(method == "mean"){
    rhr_data <- filtered_data |>
      dplyr::group_by(id) |>
      dplyr::summarize(RHR = mean(hr, na.rm = TRUE), .groups = 'drop')
    return(rhr_data)
  }
  else{
    rhr_data <- filtered_data |>
      dplyr::group_by(id) |>
      dplyr::summarize(RHR = min(hr, na.rm = TRUE), .groups = 'drop')
    return(rhr_data)
  }
}
