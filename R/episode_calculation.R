### algorithm description
# (1) apply the following to each subject (episode_single)
#       (a) interpolate to create equidistant grid
#       (b) split into contiguous segments based on gaps
#       (c) classify events in each segment (event_class)
#       (d) summarize episodes (episode_summary)
# (a) event_class: label events of each type for each segment
#       (a) must be >= duration (function input is # idx to get # minutes)
#       (b) ends at >= dur_length (function input is top level dur_length/dt0)
# (b) episode_summary: calculate summary statistics
#       (a) return for each type of episode: # episodes, mean duration, mean hr value

event_class = function(data, level_type, threshold, event_duration, end_duration) {
  event = level = pos_start = pos_end = NULL
  rm(list = c("event", "level", "pos_start", "pos_end"))

  annotated = data
  if (level_type == 'low') {
    annotated$level = annotated$pct_HRR < threshold
  } else if (level_type == 'high') {
    annotated$level = annotated$pct_HRR >= threshold
  }

  level_rle = rle(annotated$level)$lengths
  annotated$event = rep(1:length(level_rle), level_rle)
  annotated = annotated |>
    dplyr::group_by(event) |>
    dplyr::mutate(
      pos_start = level[1] && (dplyr::n() >= event_duration),
      start = c(ifelse(pos_start[1], "start", NA_character_), rep(NA_character_, dplyr::n()-1)),
      pos_end = !level[1] && (dplyr::n() >= end_duration),
      end = c(ifelse(pos_end[1], "end", NA_character_), rep(NA_character_, dplyr::n()-1))
    )

  starts = which(!is.na(annotated$start))
  ends = c(0, which(!is.na(annotated$end)), nrow(data) + 1)
  pairs = data.frame(
    starts_ref = starts,
    ends_ref = sapply(starts, function(x) {min(ends[ends > x]) - 1})
  )

  if (nrow(pairs) == 0) {
    output = rep(0, nrow(data))
    return(output)
  } else {
    pairs = pairs[!duplicated(pairs$ends_ref), ]
    vseq = Vectorize(seq.default, c("from", "to"))
    event_idx = unlist(vseq(pairs$start, pairs$end))
    event_label = rep(1:nrow(pairs), (pairs$end - pairs$start)+1)

    output = rep(0, nrow(data))
    output[event_idx] = event_label
    return(output)
  }
}

# Summary function using HRR stages
episode_summary = function(data, dt0, dur_length) {
  event = segment = hr = NULL
  rm(list = c("event", "segment", "hr"))

  episode_summary_helper = function(data, level_label, dt0, dur_length) {
    hr = event_duration = NULL
    rm(list = c("hr", "event_duration"))
    data = data[, c(1:5, which(colnames(data) == level_label))]
    colnames(data) = c("id", "time", "hr", "pct_HRR", "segment", "event")

    if (all(data$event == 0)) {
      output = c(0, 0, NA, 0)
      return(output)
    }

    events = data[data$event != 0, c("hr", "segment", "event")]
    data_sum = events |>
      dplyr::group_by(event, segment) |>
      dplyr::summarise(
        event_duration = dplyr::n() * dt0,
        event_hr = mean(hr),
        .groups = "drop"
      )

    min_duration = dt0 * ceiling(dur_length / dt0)

    if (nrow(data_sum) == 0) {
      output = c(0, 0, NA, 0)
      return(output)
    }

    avg_ep_per_day = nrow(data_sum)/(nrow(data) * dt0 / 60 / 24)
    avg_ep_duration = mean(data_sum$event_duration)
    avg_ep_hr = mean(data_sum$event_hr)
    total_episodes = nrow(data_sum)

    output = c(avg_ep_per_day, avg_ep_duration, avg_ep_hr, total_episodes)
    return(output)
  }

  labels = c("Sedentary", "Moderate", "Vigorous")
  out_list = sapply(labels, function(x) episode_summary_helper(data, x, dt0, dur_length))

  output = data.frame(
    level = labels,
    avg_ep_per_day = out_list[1, ],
    avg_ep_duration = out_list[2, ],
    avg_ep_hr = out_list[3, ],
    total_episodes = out_list[4, ]
  )
  return(output)
}
episode_single = function(data, dur_length, end_length, return_data, dt0, inter_gap, tz) {
  id = segment = pct_HRR = hr = NULL
  rm(list = c("id", "segment", "hr"))

  HRR_info <- calculate_HRR(data)
  if (is.null(HRR_info)) {
    return(NULL)
  }
  RHR <- HRR_info$RHR[1]
  maxHR <- HRR_info$max_hr[1]
  HRR <- maxHR - RHR

  data_ip <- HR2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
  dt0 = data_ip[[3]]
  day_one = as.POSIXct(data_ip[[2]][1], tz = tz)
  ndays = length(data_ip[[2]])
  time_ip =  day_one + cumsum(rep(dt0 * 60, ndays * 24 * 60 /dt0))

  new_data = data.frame(
    id = data$id[1],
    time = time_ip,
    hr = as.vector(t(data_ip[[1]]))
  )

  new_data = new_data |>
    dplyr::mutate(pct_HRR = (hr - RHR)/ HRR * 100)

  dur_idx = ceiling(dur_length/dt0)
  end_idx = ceiling(end_length/dt0)

  na_idx = is.na(new_data$hr)
  segment_rle = rle(na_idx)$lengths
  segment_data = new_data
  segment_data$segment = rep(1:length(segment_rle), segment_rle)
  segment_data = segment_data[!is.na(segment_data$hr), ]

  ep_per_seg = segment_data |>
    dplyr::group_by(segment) |>
    dplyr::mutate(
      Sedentary = event_class(data.frame(id, time, hr, pct_HRR), "low", 20, dur_idx, end_idx),
      Moderate = event_class(data.frame(id, time, hr, pct_HRR), "high", 40, dur_idx, end_idx),
      Vigorous = event_class(data.frame(id, time, hr, pct_HRR), "high", 60, dur_idx, end_idx)
    )

  if (return_data) {
    return(ep_per_seg)
  }

  output = episode_summary(ep_per_seg, dt0, dur_length)
  return(output)
}


#' Calculates low/high heartrate episodes with summary statistics
#' @name episode_calculation
#'
#' @description
#' The function determines episodes or events, calculates summary statistics,
#' and optionally returns data with episode label columns added
#'
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#' @param dur_length \strong{Default: 15.} Numeric value specifying the minimum duration in minutes to be
#' considered an episode. Note dur_length should be a multiple of the data recording
#' interval otherwise the function will round up to the nearest multiple.
#' @param end_length \strong{Default: 15.} Numeric value specifying the minimum duration in minutes of
#' improved heart rate for an episode to end.
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' @param return_data \strong{Default: FALSE.} Boolean indicating whether to also return data with episode labels.
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation.
#' @param dt0 Grid Length
#'
#'
#' @return If return_data is FALSE, a single dataframe with columns:
#' \item{id}{Subject id}
#' \item{type}{Type of episode - either low or high}
#' \item{level}{Level of episode - one of lv1, lv2, extended, lv1_excl}
#' \item{avg_ep_per_day}{Average number of episodes per day calculated as
#' (total # episodes)/(recording time in days (24hrs))}
#' \item{avg_ep_duration}{Average duration of episodes in minutes}
#' \item{avg_ep_hr}{Average heart rate in the episode in minutes}
#' \item{total_episodes}{Total number of episodes in the subject's heart rate trace}
#'
#' If return_data is TRUE, returns a list where the first entry is the episode summary dataframe
#' (see above) and the second entry is the input data with episode labels added. Note
#' the data returned here has been interpolated using the HR2DayByDay() function.
#' Mostly for use with epicalc_profile function. Format of the second list entry is:
#' \item{id}{Subject id}
#' \item{time}{Interpolated timestamps}
#' \item{hr}{heart rate in minutes}
#'
#' @details Episodes are calculated using the thresholds: sedentary/sleep (<20% HRR), moderate (40–59% HRR), and vigorous (>=60% HRR)
#'
#' @references
#' Using Heart Rate and Accelerometry to Define Quantity and Intensity of Physical Activity in Older Adults
#' \doi{10.1093/gerona/gly029}
#'
#' @export
#'
#'
#' @examples episode_calculation(example_heart_1)
#'


episode_calculation = function(data, dur_length = 15, end_length = 15, return_data = FALSE, dt0 = 1, inter_gap = 15, tz = "") {
  id = hr = NULL
  rm(list = c("id", "hr"))
  data <- data |>
    dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"))
  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  if (dur_length > inter_gap) {
    warning("Interpolation gap parameter less than episode duration; data gaps may cause incorrect computation")
  }

  out <- data |>
    dplyr::group_by(id) |>
    dplyr::reframe(episode_single(data.frame(id, time, hr),
                                  dur_length = dur_length,
                                  end_length = end_length,
                                  return_data = FALSE,
                                  dt0 = dt0,
                                  inter_gap = inter_gap,
                                  tz = tz)) |>
    dplyr::ungroup()

  if (return_data) {
    ep_data <- data |>
      dplyr::group_by(id) |>
      dplyr::reframe(episode_single(data.frame(id, time, hr),
                                    dur_length = dur_length,
                                    end_length = end_length,
                                    return_data = TRUE,
                                    dt0 = dt0,
                                    inter_gap = inter_gap,
                                    tz = tz)) |>
      dplyr::ungroup()

    return(list(episodes = out, data = ep_data))
  }

  return(out)
}
