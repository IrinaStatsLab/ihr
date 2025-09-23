#' Plot daily heart rate profiles
#'
#' @description
#' The function `plot_daily` plots daily heart rate time series profiles for a single subject.
#'
#' @usage
#' plot_daily(data, maxd = 14, inter_gap = 15, tz = "")
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#' @param maxd \strong{Default: 14.} Number of days to plot. If less than `maxd` days of data are available, all days are plotted.
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation.
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' @return Daily heart rate time series plots for a single subject
#'
#' @export
#'
#'
#' @details
#' Only a single subject's data may be plotted. The black line shows the heart rate values.
#' The colored areas of the plot refers to the individual's episode threshold. For more details on episode
#' calculation see \code{\link{episode_calculation}}
#'
#'
#'
#' @examples
#'
#' data(example_heart_1)
#' plot_daily(example_heart_1)
#'
plot_daily <- function (data, maxd = 14, inter_gap = 15, tz = "") {

  hr =  id = level_group = reltime = day_of_week = each_day = gap = time_group = NULL
  rm(list = c("hr", "id", "level_group", "reltime", "day_of_week", "each_day", "gap", "time_group"))
  if (!lubridate::is.POSIXct(data$time)){ # Check if already in date format
    data$time = as.character(data$time)
    data$time = as.POSIXct(data$time, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # Check if any NAs from conversion, this happens if wrong time format (e.g. 25:00:00) or wrong time zone which will affect daylight savings time
    if (any(is.na(data$time))){
      warning(paste("During time conversion,", sum(is.na(data$time)), "values were set to NA. Check the correct time zone specification."))
    }
  }
  data = data[complete.cases(data), ] # prevent downstream warnings for NA time values
  subject <- unique(data$id)
  ns <- length(subject)
  if (ns > 1) {
    subject <- subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data <- dplyr::filter(data, id == subject)
  }

  # === Get actual thresholds from HRR ===
  HRR_info <- calculate_HRR(data, tz = tz)

  if (nrow(HRR_info) == 0 || any(is.na(HRR_info$RHR), is.na(HRR_info$HRR))) {
    message("Cannot compute daily heart rate plot: HRR thresholds (RHR/HRR) unavailable. Returning placeholder plot.")
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0, y = 0, hjust = 0,
                          label = "Daily plot unavailable:\nRHR/HRR could not be computed.")
    )
  }

  HRR_info <- dplyr::filter(HRR_info, id == subject)
  summary_info <- summary_hr(data)

  RHR <- HRR_info$RHR
  HRR <- HRR_info$HRR
  hr_20 <- RHR + 0.20 * HRR
  hr_40 <- RHR + 0.40 * HRR
  hr_60 <- RHR + 0.60 * HRR



  days = sort(unique(lubridate::date(data$time)))
  max_days = min(length(days), maxd)
  start_day = ifelse(max_days == maxd, length(days) - maxd + 1, 1)
  kdays = days[start_day:length(days)]

  plot_data <- data |>
    dplyr::mutate(each_day = lubridate::date(time)) |>
    dplyr::filter(each_day %in% kdays) |> # select only maxd
    dplyr::mutate(day_of_week = as.character(lubridate::wday(time, label = TRUE, abbr = FALSE)),
                  reltime = hms::as_hms(paste(lubridate::hour(time), lubridate::minute(time), lubridate::second(time), sep = ":")),
                  hr_level = dplyr::case_when(hr > hr_60 ~ "Vigorous", hr < hr_20 ~ "Sedentary/Sleep", TRUE ~ "normal"))

  hr_level <- plot_data |>
    dplyr::mutate(level_group = rep(1:length(rle(hr_level)[[1]]), rle(hr_level)[[1]])) |>
    dplyr::group_by(level_group) |>
    dplyr::reframe(id = id[1], time = c(time[1] - 10, time, time[dplyr::n()] + 10),
                   reltime = hms::as_hms(c(reltime[1] - 10, reltime, reltime[dplyr::n()] + 10)),
                   day_of_week = c(day_of_week[1], day_of_week, day_of_week[dplyr::n()]),
                   each_day = c(each_day[1], each_day, each_day[dplyr::n()]),
                   class = hr_level[1], .groups = "drop")


  plot_data <- plot_data[complete.cases(plot_data), ] |>
    dplyr::group_by(id) |>
    dplyr::arrange(data.frame(id, time, hr), time) |>
    dplyr::ungroup() # ensure ascending time by subject
  gaps <- plot_data |>
    dplyr::mutate(gap = ifelse(difftime(time, dplyr::lag(time), units = "mins") > inter_gap,
                               TRUE, FALSE), row = 1:length(time)) |>
    dplyr::slice(1, which(gap))
  gaps <- c(gaps$row, nrow(plot_data) + 1)
  plot_data <- plot_data |>
    dplyr::mutate(time_group = rep(1:(length(gaps) - 1), diff(gaps))) # group by consecutive times to avoid artifacts

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(reltime, hr, group = time_group)) +
    ggplot2::geom_ribbon(ggplot2::aes(reltime, ymin = hr_40, ymax = hr_60),
                         fill = "#F9B500", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(reltime, ymin = hr_20, ymax = hr_40),
                         fill = "#48BA3C", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(reltime, ymin = hr_60, ymax = summary_info$max_hr),
                         fill = "#8E1B1B", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(reltime, ymin = summary_info$min_hr, ymax = hr_20),
                         fill = "#0073C2", alpha = 0.5) +
    ggplot2::scale_x_time(breaks = c(hms::as_hms(c('00:00:00', '12:00:00', '24:00:00'))),
                          labels = c('12 am', '12 pm', '12 am')) +
    ggplot2::facet_wrap(~each_day + day_of_week, ncol = 7, ) +
    ggplot2::ylab("hr [BPM]") + ggplot2::xlab(NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA))

}
