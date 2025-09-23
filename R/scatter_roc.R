#' Plot the relationship between Rate of Change (ROC) and Heart Rate
#'
#' @description
#' The function \code{scatter_roc} produces a scatterplot of ROC and Heart Rate
#'
#' @usage
#' scatter_roc(data, timelag = 1, inter_gap = 15, tz = "")
#'
#' @inheritParams roc
#'
#' @return A ggplot object with ROC on the X-axis and the latter Heart Rate value on the Y-axis (see \code{\link{roc}} for details on ROC)
#'
#' @export
#'
#' @details
#' A ggplot is returned that shows the relationship of a subject's Rate of Change and the subject's
#' heart rate. This allows a personalized insight into every subject's ROC, understanding the heart rate in which they ended at
#' and whether the ROC value comes from a higher activity heart rate or from another context.
#'
#'
#' The horizontal lines separate heart rate into 4 distinct sections. The thresholds here are as follows:
#' 1. Vigorous: >= 60 % HRR + RHR
#' 2. Moderate: 40 % HRR + RHR -  60 % HRR + RHR
#' 3. Light: 20 % HRR + RHR -  40 % HRR + RHR
#' 4. Sedentary/Sleep: <= 20 % HRR + RHR
#' (see \code{\link{plot_ranges_PA}})
#'
#' @examples
#' data(example_heart_1)
#' scatter_roc(example_heart_1)
#'
scatter_roc <- function(data, timelag = 1, inter_gap = 15, tz = ""){

  id = hr = RHR = HRR = hr_20 = hr_40 = hr_60 = NULL
  rm(list = c("id", "hr", "RHR", "HRR", "hr_20", "hr_40", "hr_60"))

  data = check_data_columns(data)

  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  # Interpolating for all id, flattening to one row
  full_hr <- data.frame()
  for(i in unique(data$id)){
    hr_data <- HR2DayByDay(dplyr::filter(data, id == i), dt0 = 1, inter_gap = inter_gap)
    hr_data <- as.data.frame(c(t(hr_data$gd2d)))
    full_hr <- rbind(full_hr, hr_data)
  }

  colnames(full_hr) <- "hr"

  # Generating ROC for all ids
  roc_data <- data |>
    dplyr::group_by(id) |>
    dplyr::reframe(
      roc = roc(data.frame(id, time, hr), timelag, inter_gap, tz)$roc)

  roc_data <- cbind(roc_data, full_hr) |>
    dplyr::filter(!is.na(roc))

  # Generating personalized HR Vals

  personal_hr <- calculate_HRR(data)
  add_lines <- F

  if(!is.null(personal_hr)){
    personal_hr <- personal_hr |>
      dplyr::mutate(hr_20 = round(RHR + 0.20 * HRR, 1),
                    hr_40 = round(RHR + 0.40 * HRR, 1),
                    hr_60 = round(RHR + 0.60 * HRR, 1))
    add_lines <- T
  } else{
    message("HR Thresholds could not be computed, no lines will be inputted in the plot")
  }

  .p <- roc_data |>
    ggplot(aes(x = roc, y = hr)) +
    geom_point(alpha = 0.3) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    facet_wrap(~id) +
    scale_x_continuous(name = "Rate of Change") +
    scale_y_continuous(name = "Heart Rate (BPM)")

  if(add_lines){
    .p <- .p +
    geom_hline(data = personal_hr, aes(yintercept = hr_20), color = "#0073C2", linetype = "dashed", linewidth = 1) +
      geom_hline(data = personal_hr, aes(yintercept = hr_40), color = "#48BA3C", linetype = "dashed", linewidth = 1) +
      geom_hline(data = personal_hr, aes(yintercept = hr_60), color = "#F9B500", linetype = "dashed", linewidth = 1)
  }

  return(.p)

}

