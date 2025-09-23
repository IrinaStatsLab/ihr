#' Display Ambulatory Heart rate Profile (AHP) statistics for selected subject
#'
#' @usage
#' ahp(data, maxd = 14, inter_gap = 15, dt0 = 1, tz = "", daily = TRUE)
#'
#' @param data A dataFrame object with column names "id", "time", "hr"
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation.
#' @param dt0 Grid Length, in minutes
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' @param daily \strong{Default: TRUE.} Logical indicator whether AHP should include separate daily plots.
#' @param maxd max days
#'
#' @return A plot displaying heart rate measurements range, selected heart rate statistics (Resting Heart Rate, Heart Rate Reserve), percentage spent in target ranges and quantiles of 24 hour profile.
#'
#' @details
#' For details on the different metrics and plots see the following:
#' Time in range bar plot (see \code{\link{plot_ranges_PA}})
#' Summary quantile plot (see \code{\link{plot_ahp}})
#' Daily Plot (see \code{\link{plot_daily}})
#' Resting Heart Rate (see \code{\link{calculate_RHR}})
#' Heart Rate Reserve (see \code{\link{calculate_HRR}})
#' Range of Measurement Data (see \code{\link{percent_nonmissing}})
#'
#' @export
#'
#' @examples
#' data(example_heart_1)
#' ahp(example_heart_1, daily = FALSE)


ahp <- function(data, maxd = 14, inter_gap = 15, dt0 = 1, tz = "", daily = TRUE){

  id = NULL
  rm(list = c("id"))

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data |> dplyr::filter(id == subject)
  }

  # Calculate range of measurements
  out_range = percent_nonmissing(data)

  RHR_data <- calculate_RHR(data, tz = tz)
  HRR_data <- calculate_HRR(data, tz = tz)

  # Extract values for subject
  RHR_value <- if (!is.null(RHR_data)) round(RHR_data$RHR[RHR_data$id == subject]) else NA
  HRR_value <- if (!is.null(HRR_data)) round(HRR_data$HRR[HRR_data$id == subject]) else NA
  Max_value <- if (!is.null(HRR_data)) round(HRR_data$max_hr[HRR_data$id == subject]) else NA

  # Calculate metrics
  tableStat = data.frame("Heartrate statistics", "value")
  tableStat[1, 1] = "id"
  tableStat[1, 2] = as.character(subject)
  tableStat[2, 1] = "Start Date"
  tableStat[2, 2] = as.character(out_range$start_time)
  tableStat[3, 1] = "End Date"
  tableStat[3, 2] = as.character(out_range$end_time)
  tableStat[4, 1] = "Duration"
  tableStat[4, 2] = paste(as.character(out_range$unique_days),"days")
  tableStat[5, 1] = "% Time Heart rate is not missing"
  tableStat[5, 2] = paste0(round(out_range$percent_nonmissing), "%")
  tableStat[6, 1] = "Resting Heart Rate(RHR)"
  tableStat[6, 2] = if (!is.na(RHR_value)) paste0(RHR_value, " bpm") else "Not available"
  tableStat[7, 1] = "Heart Rate Reserve(HRR)"
  tableStat[7, 2] = if (!is.na(HRR_value)) paste0(HRR_value, " bpm") else "Not available"
  tableStat[8, 1] = "Max Heart Rate"
  tableStat[8, 2] = if (!is.na(HRR_value)) paste0(Max_value, " bpm") else "Not available"


  # Make a pretty table
  mytheme <- gridExtra::ttheme_default(core = list(fg_params = list(hjust = 0, x = 0.03, fontsize = 12)), colhead = list(fg_params = list(fontsize=9, fontface="bold")))

  t1 = gridExtra::tableGrob(tableStat, rows = NULL, cols = NULL, theme = mytheme)

  # Create percentage plot
  p1 = plot_ranges_PA(data)

  p2 = plot_ahp(data, inter_gap = inter_gap, tz = tz)

  if (daily){
    # Create daily plots
    p3 = plot_daily(data, maxd = maxd, inter_gap = inter_gap, tz = tz)

    # Combine metrics and plot in one display
    pFinal = (patchwork::wrap_elements(t1) +  p1 + patchwork::guide_area() + patchwork::plot_layout(widths = c(4, 1, 1))) / p2 / p3
  }else{
    pFinal = (patchwork::wrap_elements(t1) +  p1 + patchwork::guide_area() + patchwork::plot_layout(widths = c(4, 1, 1))) / p2
  }
  pFinal
}
