#' Calculate the time an individual spend in different stages

#' @description
#' The function summary_PA computes the time(in percentage) for different individuals spend in different stages
#' Using the thresholds: sedentary/sleep (<20% HRR), light (20–39% HRR), moderate (40–59% HRR), and vigorous (>=60% HRR)
#' The definition of these thresholds are sourced from the reference. For description of heart rate reserve calculation, see \code{\link{calculate_HRR}}

#' @usage
#' summarize_PA(data, tz)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Using Heart Rate and Accelerometry to Define Quantity and Intensity of Physical Activity in Older Adults
#' \doi{10.1093/gerona/gly029}

#' @export
#' @examples
#' data(example_heart_1)
#' summarize_PA(example_heart_1)
summarize_PA <- function(data, tz = "") {
  time = hr = id = hour = minutes = minute = RHR = mean_hr = HRR = PA_stage = total_minutes = NULL

  data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S", tz = tz)

  # Round time down to the nearest minute
  data <- data |>
    dplyr::mutate(minute = lubridate::floor_date(time, unit = "minute"))

  # Average HR within each minute per participant
  min_hr_data <- data |>
    dplyr::group_by(id, minute) |>
    dplyr::summarize(mean_hr = mean(hr, na.rm = TRUE), .groups = "drop")

  # Calculate RHR and max HR per participant
  HRR_info <- calculate_HRR(data, method = "max-RHR")
  if (is.null(HRR_info)) {
    message("Missing RHR or HRR, cannot classify PA.")
    return(NULL)
  }

  # Classify PA stage by %HRR
  classified <- dplyr::left_join(min_hr_data, HRR_info, by = "id") |>
    dplyr::mutate(
      pct_HRR = (mean_hr - RHR) / HRR * 100,
      PA_stage = dplyr::case_when(
        pct_HRR < 20 ~ "Sedentary/Sleep",
        pct_HRR < 40 ~ "Light",
        pct_HRR < 60 ~ "Moderate",
        pct_HRR >= 60 ~ "Vigorous",
        TRUE ~ NA_character_
      )
    )

  desired_order <- c("Sedentary/Sleep", "Light", "Moderate", "Vigorous")

  # Count minutes per stage and total minutes
  summary_table_raw <- classified |>
    dplyr::group_by(id, PA_stage) |>
    dplyr::summarize(minutes = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = PA_stage, values_from = minutes, values_fill = 0)

  # Add total and calculate percentages
  summary_table <- summary_table_raw |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::all_of(desired_order))),
                  dplyr::across(dplyr::all_of(desired_order), ~ (.x / total) * 100, .names = "{.col}")) |>
    dplyr::ungroup() |>
    dplyr::select(id, dplyr::all_of(desired_order))  # Only return percentages

  return(summary_table)
}
