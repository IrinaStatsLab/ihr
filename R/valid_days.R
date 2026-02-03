#' Boolean value for a valid day of HR data
#'
#' @description
#' The function 'valid_days' computes whether a day within an individual's HR data is valid, defined as
#' whether a day has more than a pre-specified hours worth of data.
#'
#' @param data Dataframe with column names ("id, "time", "hr")
#' @param hour Hour value that a day must have in order for that corresponding day to be considered valid. \strong{Default: 10.}
#' @param min_in_hr The number of minutes within the hour that an observation must have for an hour to contribute to a valid day. \strong{Default: 60.}
#'
#' @return A tibble item with the columns id, day, full_hours, valid_day
#'
#' @details
#' For an individual's HR data, steps must be taken to ensure the quality of the data that is analyzed. 'valid_days' is
#' designed to return the days in which an individual has worn their device for a sufficient amount for unbiased estimates
#' (To the discretion of the data analyst). Default values selected by most common approaches (Reference Provided)
#'
#' @references
#' Reporting adherence, validity and physical activity measures of wearable activity trackers in medical research: A systematic review
#' \doi{10.1016/j.ijmedinf.2022.104696}
#'
#' @export
#'
#' @examples
#' data(example_heart_1)
#' valid_days(example_heart_1)
#'

valid_days <- function(data, hour = 10, min_in_hr = 60){
  id = hr = day = minutes_in_hour = full_hours = day = NULL
  rm(list = c('id', 'hr', 'day', 'minutes_in_hour', 'full_hours', "day"))

  if(min_in_hr > 60){
    message("min_in_hr exceeds 60 minutes, will default to 60 minutes")
    min_in_hr <- 60
  }

  if(hour > 24){
    message("hour exceeds 24 hours, will default to 24 hours")
  }

  # Summarize data by minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  # Collects the number of minutes within each hour of observation
  min_per_hours <- data |>
    dplyr::mutate(day = as.Date(time),
                  hour = lubridate::floor_date(time, "hour")) |>
  dplyr::group_by(id, day, hour) |>
  dplyr::summarise(minutes_in_hour = dplyr::n(), .groups = "drop")

  # Table with ID, Date, Number of valid hour, and whether this is a valid day by definition
  valid_hour_per_day <- min_per_hours |>
    dplyr::group_by(id, day) |>
    dplyr::summarise(full_hours = sum(minutes_in_hour >= min_in_hr), .groups = "drop") |>
    dplyr::mutate(valid_day = full_hours >= hour)

  valid_hour_per_day$day <- as.POSIXct(valid_hour_per_day$day)

  return(valid_hour_per_day)
}
