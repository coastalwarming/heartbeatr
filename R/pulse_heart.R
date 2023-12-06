#' Determine the heartbeat rate in one PULSE channel
#'
#' @description
#' Take data from one PULSE channel and identify the heartbeat wave peaks using an algorithm that searches for maxima across multiple scales.
#'
#' @param split_window_one_channel A tibble with PULSE data for only one channel with columns `$time` and `$val`
#'
#' @return
#' A one-row tibble with 5 columns:
#' * `time`, time at the center of split_window_one_channel$time
#' * `t_pks`, timestamps of each wave peak identified
#' * `n`, number of wave peaks identified
#' * `hz`, heartbeat rate estimate (in Hz)
#' * `sd`, standard deviation of the intervals between wave peaks
#'
#' @details
#' improved function from https://github.com/ig248/pyampd
#'
#' @export
#'
#' @seealso [pulse_find_peaks_all_channels()], [pulse_heart()]
#'
#' [pulse_read()], [pulse_split()], [pulse_smooth()], [pulse_optimize()], [PULSE()]
#'
#' @examples
#' # Begin prepare data ----
#' pulse_data_sub <- pulse_data
#' pulse_data_sub$data <- pulse_data_sub$data[,1:5]
#' pulse_data_split <- pulse_split(
#'    pulse_data_sub,
#'    window_width_secs = 30,
#'    window_shift_secs = 60,
#'    min_data_points = 0.8,
#'    with_progress = TRUE)
#' pulse_data_split <- pulse_optimize(pulse_data_split)
#' split_window <- pulse_data_split[[1]]
#' split_window_one_channel <- split_window[,1:2]
#' colnames(split_window_one_channel) <- c("time", "val")
#' # End prepare data ----
#'
#' # Determine heartbeat rates in one channel in one time window
#' pulse_find_peaks_one_channel(split_window_one_channel)
pulse_find_peaks_one_channel <- function(split_window_one_channel) {

  t <- split_window_one_channel$time
  y <- split_window_one_channel$val

  N <- length(y)
  L <- N %/% 2  # half-length of the array

  # de-trend data
  # S <- seq(1:N)
  # f <- fitted(lm(y ~ S))
  # y <- y - f

  # create a Boolean matrix with L rows and N columns, initialized with TRUE
  MAT <- matrix(rep(TRUE, L * N), nrow = L)

  # loop to compare to right and left neighbors
  for (k in 1:L) {
    MAT[k, 1:(N - k)] <- MAT[k, 1:(N - k)] & (y[1:(N - k)] > y[(k + 1):N])
    MAT[k, (k + 1):N] <- MAT[k, (k + 1):N] & (y[(k + 1):N] > y[1:(N - k)])
  }

  # find scale with most maxima
  G <- rowSums(MAT)
  G <- G * (L:1) # normalize to adjust for new edge regions
  l_scale <- which.max(G)

  # find peaks that persist on all scales up to l
  MAT <- MAT[1:l_scale, , drop = FALSE]
  pks_logical <- apply(MAT, 2, all)
  pks <- which(pks_logical)

  # compute stats
  t_pks <- t[pks]
  intervals <- as.numeric(diff(t_pks))

  hz    <- round(mean(1 / intervals), 3)
  hz_sd <- round(stats::sd(1 / intervals), 3)

  # return
  tibble::tibble(
    time  = mean(t),
    t_pks = list(t_pks),
    n  = length(pks),
    hz = hz,
    sd = hz_sd
  )
}

#' Determine the heartbeat rate in all channels of a PULSE split window
#'
#' @description
#' Take data from PULSE data window and run `pulse_find_peaks_one_channel` in all channels.
#'
#' @inheritParams pulse_interpolate
#'
#' @return
#' A tibble with up to 10 rows (one for each channel) and 6 columns:
#' * `id`, PULSE channel IDs
#' * `time`, time at the center of split_window_one_channel$time
#' * `data`, a list of tibbles with raw PULSE data for each combination of channel and window, with columns `time`, `val` and `peak` (`TRUE` in rows corresponding to wave peaks)
#' * `n`, number of wave peaks identified
#' * `hz`, heartbeat rate estimate (in Hz)
#' * `sd`, standard deviation of the intervals between wave peaks

#' @export
#'
#' @seealso [pulse_find_peaks_one_channel()], [pulse_heart()]
#'
#' [pulse_read()], [pulse_split()], [pulse_smooth()], [pulse_optimize()], [PULSE()]
#'
#' @examples
#' # Begin prepare data ----
#' pulse_data_sub <- pulse_data
#' pulse_data_sub$data <- pulse_data_sub$data[,1:5]
#' pulse_data_split <- pulse_split(
#'    pulse_data_sub,
#'    window_width_secs = 30,
#'    window_shift_secs = 60,
#'    min_data_points = 0.8,
#'    with_progress = TRUE)
#' pulse_data_split <- pulse_optimize(pulse_data_split)
#' split_window <- pulse_data_split[[1]]
#' # End prepare data ----
#'
#' # Determine heartbeat rates in all channels in one time window
#' pulse_find_peaks_all_channels(split_window)
pulse_find_peaks_all_channels <- function(split_window) {
  stopifnot(is.pulse.tbl(split_window))

  # rearrange data
  long_split_window <- split_window %>%
    tidyr::pivot_longer(
      cols = -time,
      names_to = "id",
      values_to = "val"
    ) %>%
    tidyr::nest(data = c("time", "val"))

  # determine the heart rate in all available channels
  long_split_window <- dplyr::bind_cols(
    long_split_window,
    purrr::map(long_split_window$data, pulse_find_peaks_one_channel) %>%
      purrr::list_rbind()
  )

  # signal peaks in data
  long_split_window <- long_split_window %>%
    dplyr::mutate(data = purrr::map2(data, t_pks, ~dplyr::mutate(.x, peak = time %in% .y))) %>%
    dplyr::select(-t_pks) %>%
    dplyr::relocate(id, time)

  # return
  return(long_split_window)
}

#' Determine the heartbeat rate in all channels of a split PULSE object (`STEP 4`)
#'
#' @description
#' * `step 1` -- [`pulse_read()`]
#' * `step 2` -- [`pulse_split()`]
#' * `step 3` -- [`pulse_optimize()`]
#' * **`-->>` step 4 -- [`pulse_heart()`] `<<--` FINAL PROCESSING STEP**
#'
#' For each combination of channel and time window, determine the heartbeat rate automatically.
#'
#' `pulse_heart()` takes the output from a call to `pulse_optimize()` (or `pulse_split()` if optimization is skipped - highly discouraged) and employs an algorithm optimized for the identification of wave peaks in noisy data to determine the heartbeat rate in all channels of the PULSE dataset.
#'
#' @inheritParams pulse_read
#' @inheritParams pulse_split
#' @inheritParams pulse_optimize
#'
#' @return
#' A tibble with nrows = (number of channels) * (number of windows in `pulse_data_split`) and 6 columns:
#' * `id`, PULSE channel IDs
#' * `time`, time at the center of each time window
#' * `data`, a list of tibbles with raw PULSE data for each combination of channel and window, with columns `time`, `val` and `peak` (`TRUE` in rows corresponding to wave peaks)
#' * `n`, number of wave peaks identified
#' * `hz`, heartbeat rate estimate (in Hz)
#' * `sd`, standard deviation of the intervals between wave peaks
#'
#' @export
#'
#' @seealso [pulse_find_peaks_all_channels()], [pulse_find_peaks_one_channel()], [progressr::handlers()], [future::plan()]
#'
#' [pulse_read()], [pulse_split()], [pulse_optimize()], [pulse_heart()], [PULSE()], [pulse_summarise()]
#'
#' @examples
#' # Begin prepare data ----
#' pulse_data_sub <- pulse_data
#' pulse_data_sub$data <- pulse_data_sub$data[,1:5]
#' pulse_data_split <- pulse_split(
#'    pulse_data_sub,
#'    window_width_secs = 30,
#'    window_shift_secs = 60,
#'    min_data_points = 0.8,
#'    with_progress = TRUE)
#' pulse_data_split <- pulse_optimize(pulse_data_split)
#' # End prepare data ----
#'
#' # Determine heartbeat rates in all channels in all time window
#' pulse_heart(pulse_data_split)
pulse_heart <- function(pulse_data_split, with_progress = NULL, msg = TRUE) {
  ## CHECKS INITIATED ## ------------------- ##
  stopifnot(all(purrr::map_lgl(pulse_data_split, is.pulse.tbl)))
  stopifnot(is.logical(msg))
  ## CHECKS COMPLETED ## ------------------- ##

  # set up parallel computing
  current_strategy <- future::plan() %>%
    class() %>%
    magrittr::extract(2)
  if (msg) {
    if (current_strategy == "sequential") {
      message("  --> [i] parallel computing not engaged")
      message("  --> [i] if too slow, type ?PULSE for help on how to use parallel computing\n")
    } else {
      message("  --> [i] parallel computing engaged")
      message(stringr::str_c("  --> [i] current future strategy: ", current_strategy, "\n"))
    }
  }

  # set progress reporting strategy
  if (!is.null(with_progress)) {
    if (with_progress) {
      old_handlers <- progressr::handlers("cli")
    } else {
      old_handlers <- progressr::handlers("void")
    }
    on.exit(if (is.null(old_handlers)) progressr::handlers("void") else progressr::handlers(old_handlers), add = TRUE)
  }

  # process data
  bar <- progressr::progressor(along = pulse_data_split)

  heart_rates <- future.apply::future_lapply(
    pulse_data_split,
    function(x) {
      bar(message = "determining heart rates |")
      pulse_find_peaks_all_channels(x)
    })

  # tidy
  heart_rates <- heart_rates %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(id, time) %>%
    dplyr::mutate(id = factor(id)) %>%
    # remove rows for which less than 3 peaks were detected
    dplyr::filter(n > 2)

  # return
  return(heart_rates)
}
