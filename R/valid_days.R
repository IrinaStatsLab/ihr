valid_days <- function(data){
  # Summarize data by minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  min_per_hours <- data |>
    dplyr::mutate(day = as.Date(time),
                  hour = floor_date(time, "hour")) |>
  group_by(id, day, hour) |>
  summarise(minutes_in_hour = n(), .groups = "drop")

  valid_hour_per_day <- min_per_hours |>
    group_by(id, day) |>
    summarise(full_hours = sum(minutes_in_hour == 60),
              valid_day = full_hours >= 10,
              .groups = "drop")

  return(valid_hour_per_day)
}
