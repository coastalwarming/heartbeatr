#' Plot processed PULSE data
#'
#' @description
#' A shortcut function based on `ggplot2` to facilitate the quick inspection of the results of the analysis (with the options to separate channels or not).
#'
#' @details
#' This function is **NOT meant** for high-level displaying of the data - it's simply a quick shortcut to facilitate data inspection.
#'
#' When inspecting the plot, assess if the `loess` confidence intervals are too wide for any given channel, which is indicative of data with high variability (usually not desired).
#'
#' @param heart_rates The output of [PULSE()] (or [`pulse_heart()`], [`pulse_normalize()`] and [pulse_summarise()]).
#' @param normalized Logical, whether to plot `hz_norm` (`TRUE`) or `hz` (`FALSE`, the default).
#' @param smooth Logical, whether to plot a `loess` smoothing curve (`TRUE`, the default) or a line (`FALSE`).
#' @param points Logical, whether to plot the data points (defaults to `TRUE`).
#' @param facets Logical, whether to separate channels in facets (`TRUE`, the default) or to plot all data together (`FALSE`)
#'
#' @return
#' A ggplot object that can be augmented using ggplot2 syntax or plotted right away
#'
#' @export
#'
#' @seealso [pulse_plot_raw()], [PULSE()], [`pulse_heart()`], [pulse_normalize()]
#'
#' @examples
#' # Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' heart_rates <- PULSE(
#'   paths,
#'   discard_channels  = paste0("s", 5:10),
#'   window_width_secs = 30,
#'   window_shift_secs = 60,
#'   min_data_points   = 0.8,
#'   target_freq = 40,
#'   bandwidth   = 0.2,
#'   with_progress = TRUE
#'   )
#' # End prepare data ----
#'
#' # Default
#' pulse_plot_all(heart_rates)
#'
#' # Without facets, the different basal heartbeat rates become evident in non-normalized data
#' pulse_plot_all(heart_rates, facets = FALSE)
#'
#' # Without facets, normalized data always lines up (around 1) during the baseline period
#' pulse_plot_all(pulse_normalize(heart_rates), normalize = TRUE, facets = FALSE)
#'
#' # The plot can be modified using ggplot2 syntax
#' pulse_plot_all(heart_rates) + ggplot2::theme_classic()
pulse_plot_all <- function(heart_rates, normalized = FALSE, smooth = TRUE, points = TRUE, facets = TRUE) {
  heart_rates <- heart_rates %>% dplyr::mutate(val = hz)
  if (normalized) {
    if (!any(colnames(heart_rates) == "hz_norm")) stop("\n  --> [x] 'hz_norm' is missing in 'heart_rates'\n  --> [x] normalized heartbeat rates cannot be plotted\n  --> [i] run 'heart_rates' through 'pulse_normalize()' to fix this")

    heart_rates <- heart_rates %>% dplyr::mutate(val = hz_norm)
  }

  p <- ggplot2::ggplot(heart_rates) +
    ggplot2::xlab("") +
    ggplot2::ylab(
      stringr::str_c(
        if (normalized) "normalized " else "",
        "heartbeat frequency (Hz)"
      )
    )

  if (smooth) {
    p <- p + ggplot2::geom_smooth(ggplot2::aes(time, val, color = id, fill = id), formula = y ~ x, method = "loess", linewidth = 0.5)
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(time, val, col = id))
  }

  if (points) p <- p + ggplot2::geom_point(ggplot2::aes(time, val, col = id))
  if (facets) p <- p + ggplot2::facet_wrap(facets = "id", ncol = 1)

  p
}

#' Plot raw PULSE data
#'
#' @description
#' A shortcut function based on `ggplot2` to facilitate the quick inspection of the raw data underlying the analysis (with the peaks detected signaled with red dots). Useful to visually inspect the performance of the algorithm.
#'
#' @details
#' This function is **NOT meant** for high-level displaying of the data - it's simply a quick shortcut to facilitate data inspection.
#'
#' When inspecting the plot, assess if red dots top all heartbeats and no more than that. Difficult datasets may result in true heartbeats being missed or non-heartbeats (noise) being erroneously detected (false positives and/or false negatives). Note that the wider the time window (controlled by the `window_width_secs` parameter in [`pulse_split()`]) and the higher the heartbeat rate, the less critical are a few false positives or negatives (over a 10 secs window, missing 1 peak in 10 results in hz to drop by 10% (from 1 to 0.9), while over a 30 secs window, missing 1 peak in 30 results in a drop of 3.33% (from 1 to 0.967), and missing 1 peak in 60 results in a drop of just 1.7%.
#'
#' @inheritParams pulse_plot_all
#' @param ID String naming the target channel id (must match exactly)
#' @param i A numeric value pointing to the index of the target window
#' @param range A numeric value indicating how many more windows to plot (centered around `i`, i.e., if `i = 5` and `range = 2`, windows 3 to 7 will be plotted, with window 5 at the center)
#'
#' @return
#' A ggplot object that can be augmented using ggplot2 syntax or plotted right away
#'
#' @export
#'
#' @seealso [pulse_plot_all()], [PULSE()], [`pulse_heart()`], [pulse_normalize()]
#'
#' @examples
#' # Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' heart_rates <- PULSE(
#'   paths,
#'   discard_channels  = paste0("s", 5:10),
#'   window_width_secs = 30,
#'   window_shift_secs = 60,
#'   min_data_points   = 0.8,
#'   target_freq = 40,
#'   bandwidth   = 0.2,
#'   with_progress = TRUE
#'   )
#' # End prepare data ----
#'
#' # Single window (more detail)
#' pulse_plot_raw(heart_rates, "limpet_1", 5)
#'
#' # Multiple windows (less detail but more context)
#' pulse_plot_raw(heart_rates, "limpet_1", 5, 2) # stats are shown for window 'i'
#'
#' # The plot can be modified using ggplot2 syntax
#' pulse_plot_raw(heart_rates, "limpet_1", 10) + ggplot2::theme_classic()
pulse_plot_raw <- function(heart_rates, ID, i, range = 0) {

  # comer timestamp

  tmp1 <- heart_rates %>%
    dplyr::filter(id == ID) %>%
    dplyr::slice(i)

  tmp2 <- tmp1 %>%
    dplyr::pull(data) %>%
    dplyr::first()

  diff <- tmp2$time %>% range() %>% diff()
  units(diff) <- "secs"

  str1 <- stringr::str_c(ID, " @ ", tmp1$time)
  str2 <- stringr::str_c(tmp1$n, " wave peaks (over ", diff, " secs), hz = ", tmp1$hz, " (", round(tmp1$hz * 60, 0), " bpm, sd = ", tmp1$sd, ")")

  p <- ggplot2::ggplot() +
    ggplot2::ggtitle(label = str1, subtitle = str2) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())

  if (range) {
    tmp3 <- heart_rates %>%
      dplyr::filter(id == ID) %>%
      dplyr::slice((i - range):(i + range)) %>%
      dplyr::rename(t = time) %>%
      tidyr::unnest(cols = "data")

    p <- p +
      ggplot2::geom_line(data = tmp3, ggplot2::aes(time, val)) +
      ggplot2::geom_point(data = dplyr::filter(tmp3, peak), ggplot2::aes(time, val), col = "red") +
      ggplot2::facet_wrap(facets = "t", ncol = 1, scales = "free_x")
  } else {
    p <- p +
      ggplot2::geom_line(data = tmp2, ggplot2::aes(time, val)) +
      ggplot2::geom_point(data = dplyr::filter(tmp2, peak), ggplot2::aes(time, val), col = "red")
  }

  # return
  return(p)
}


