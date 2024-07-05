plot_test <- function(data) {
	ggplot2::ggplot(data) +
		ggplot2::geom_line(ggplot2::aes(time, val)) +
		ggplot2::geom_point(data = dplyr::filter(data, peak), ggplot2::aes(time, val), col = "red")
}

#' Plot processed PULSE data
#'
#' @description
#' A shortcut function based on `ggplot2` to facilitate the quick inspection of the results of the analysis (with the option to separate channels or not).
#'
#' @details
#' This function is **NOT meant** for high-level displaying of PULSE data - it's simply a quick shortcut to facilitate data inspection.
#'
#' When inspecting the plot with `smooth = TRUE`, assess if the `loess` confidence intervals are too wide for any given channel, which is indicative of data with high variability (not ideal).
#'
#' If using `smooth = FALSE`, then look for the width of the confidence interval for each data point.
#'
#' @param heart_rates The output of [PULSE()] (or [pulse_heart()], [pulse_doublecheck()], [pulse_normalize()] and [pulse_summarise()]).
#' @param ID String naming a single target channel id (must match exactly); defaults to `NULL`, which results in all IDs being plotted
#' @param normalized Logical, whether to plot `hz_norm` (`TRUE`) or `hz` (`FALSE`, the default).
#' @param smooth Logical, whether to plot a `loess` smoothing curve (`TRUE`, the default) or a line (`FALSE`).
#' @param points Logical, whether to plot the data points (defaults to `TRUE`).
#' @param facets Logical, whether to separate channels in facets (`TRUE`, the default) or to plot all data together (`FALSE`)
#' @param bpm Logical, whether to plot heartbeat frequency using Hz (`FALSE`, the default) or Beats Per Minute (`TRUE`); overriden to `FALSE`if `normalized` is set to `TRUE`
#'
#' @return
#' A ggplot object that can be augmented using ggplot2 syntax or plotted right away
#'
#' @export
#'
#' @seealso
#'  * use [pulse_plot_raw()] to quickly plot the raw PULSE data for a given channel/split window
#'  * [pulse_heart()] or [PULSE()] generate the input for `pulse_plot`
#'  * call [pulse_normalize()] to anchor heart beat data from each channel to a reference period during the experiment
#'
#' @examples
#' ## Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' heart_rates <- PULSE(
#'   paths,
#'   discard_channels  = paste0("s", 5:10),
#'   window_width_secs = 30,
#'   window_shift_secs = 60,
#'   min_data_points   = 0.8,
#'   interpolation_freq = 40,
#'   bandwidth   = 0.2,
#'   raw_v_smoothed = TRUE,
#'   with_progress = TRUE
#'   )
#' ## End prepare data ----
#'
#' # Default
#' pulse_plot(heart_rates)
#'
#' # Without facets, the different basal heartbeat rates become evident in non-normalized data
#' pulse_plot(heart_rates, facets = FALSE)
#'
#' # Without facets, normalized data always lines up (around 1) during the baseline period
#' pulse_plot(pulse_normalize(heart_rates), normalized = TRUE, facets = FALSE)
#'
#' # The plot can be modified using ggplot2 syntax
#' pulse_plot(heart_rates) + ggplot2::theme_classic()
#'
#' # Data can also be visualized using BPM (Beats Per Minute)
#' pulse_plot(heart_rates, bpm = TRUE)
#'
#' # If inspecting the heart rate estimates for a single ID and suppressing the
#' # LOESS smoothing, the confidence interval of each estimate is also shown
#' pulse_plot(heart_rates, ID = "limpet_1", smooth = FALSE)
pulse_plot <- function(heart_rates, ID = NULL, normalized = FALSE, smooth = TRUE, points = TRUE, facets = TRUE, bpm = FALSE) {

	hr <- if (bpm) {
		dplyr::mutate(heart_rates, val = hz * 60, val_ci = ci * 60)
	} else {
		dplyr::mutate(heart_rates, val = hz, val_ci = ci)
	}
	if (!is.null(ID)) hr <- dplyr::filter(hr, id == ID)

	if (normalized) {
		if (!any(colnames(hr) == "hz_norm")) stop("\n  --> [x] 'hz_norm' is missing in 'heart_rates'\n  --> [x] normalized heart beat frequencies cannot be plotted\n  --> [i] run 'heart_rates' through 'pulse_normalize()' first to fix this")
		hr  <- dplyr::mutate(hr, val = hz_norm)
		bpm <- FALSE
	}

	p <- ggplot2::ggplot(hr) +
		ggplot2::xlab("") +
		ggplot2::ylab(
			if (bpm) {
				"heartbeat frequency (bpm)"
			} else {
				stringr::str_c(
					if (normalized) "normalized " else "",
					"heartbeat frequency (Hz)"
				)
			}
		)

	if (!is.null(ID)) {
		if (smooth) {
			p <- p +
				ggplot2::geom_smooth(ggplot2::aes(time, val), formula = y ~ x, method = "loess", linewidth = 0.5)
		} else {
			if (!normalized) {
				p <- p +
					ggplot2::geom_ribbon(ggplot2::aes(time, ymin = val - val_ci, ymax = val + val_ci), fill = "grey80") +
					ggplot2::geom_line(ggplot2::aes(time, val - val_ci), linetype = "dashed", linewidth = 0.25) +
					ggplot2::geom_line(ggplot2::aes(time, val + val_ci), linetype = "dashed", linewidth = 0.25)
			}
			p <- p + ggplot2::geom_line(ggplot2::aes(time, val), linewidth = 0.5)
		}
		if (points) p <- p + ggplot2::geom_point(ggplot2::aes(time, val))
		p <- p + ggplot2::ggtitle(
			label = ID,
			subtitle = stringr::str_c(
				min(hr$time) %>% as.character(),
				"  /  ",
				max(hr$time) %>% as.character()
			)
		)

	} else {
		if (smooth) {
			p <- p + ggplot2::geom_smooth(ggplot2::aes(time, val, color = id, fill = id), formula = y ~ x, method = "loess", linewidth = 0.5)
		} else {
			p <- p + ggplot2::geom_line(ggplot2::aes(time, val, col = id))
		}
		if (points) p <- p + ggplot2::geom_point(ggplot2::aes(time, val, col = id))
		if (facets) p <- p + ggplot2::facet_wrap(facets = "id", ncol = 1)
	}

	# return
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
#' @inheritParams pulse_plot
#' @param target_i A numeric value pointing to the index of the target window
#' @param target_time A target time expressed as POSIX time or a string that can be converted by `as.POSIXct()`; `target_i` will be computed as the window closest to the `target_time`
#' @param range A numeric value indicating how many more windows to plot (centered around the target window, i.e., if `target_i = 5` and `range = 2`, windows 3 to 7 will be plotted, with window 5 at the center)
#'
#' @return
#' A ggplot object that can be augmented using ggplot2 syntax or plotted right away
#'
#' @export
#'
#' @seealso
#'  * use [pulse_plot()] to plot processed PULSE data for a several channels
#'  * [pulse_heart()] or [PULSE()] generate the input for `pulse_plot`
#'  * call [pulse_normalize()] to anchor heart beat data from each channel to a reference period during the experiment
#'
#' @examples
#' ## Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' heart_rates <- PULSE(
#'   paths,
#'   discard_channels  = paste0("s", 5:10),
#'   window_width_secs = 30,
#'   window_shift_secs = 60,
#'   min_data_points   = 0.8,
#'   interpolation_freq = 40,
#'   bandwidth   = 0.2,
#'   with_progress = TRUE
#'   )
#' ## End prepare data ----
#'
#' # Single window (more detail)
#' # using a target date and time
#' pulse_plot_raw(heart_rates, "limpet_1", "2022-12-29 13:55")
#' # using the index (in this case, the 5th window)
#' pulse_plot_raw(heart_rates, "limpet_1", target_i = 5)
#'
#' # Multiple windows (less detail but more context)
#' pulse_plot_raw(heart_rates, "limpet_1", "2022-12-29 13:55", 2)
#'
#' # The plot can be modified using ggplot2 syntax
#' pulse_plot_raw(heart_rates, "limpet_1", target_i = 5) + ggplot2::theme_classic()
#'
#' # Use it to inspect if the windows where heart beat frequency
#' #  was suspected to be double the real value were correctly handled
#' doubles <- dplyr::filter(heart_rates, d_r == 1) # 4 rows
#'
#' pulse_plot_raw(doubles, "limpet_4", target_i = 1)
#' # only 3 out of 4 peaks were detected, which is to be expected given
#' #  the big drop in the signal midway through the window
#' # --> this hz estimate isn't accurate
#'
#' pulse_plot_raw(doubles, "limpet_4", target_i = 2)
#' # 4 consecutive peaks detected and properly spaced
#' # --> this hz estimate is accurate
#'
#' pulse_plot_raw(doubles, "limpet_4", target_i = 3)
#' # 4 consecutive peaks detected and properly spaced
#' # NOTE THAT EVEN THOUGH THE PEAKS DETECTED ARE SLIGHTLY SHIFTED
#' #  FROM THE SIGNAL PEAKS, they are properly spaced (matching the
#' #  signal's frequency)
#' # --> this hz estimate is still accurate
#'
#' pulse_plot_raw(doubles, "limpet_4", target_i = 4)
#' # this window captured mostly just noise,
#' # --> this hz estimate is meaningless
#'
#' # Now note that the column 'keep' already signaled correctly
#' #  which of these 4 windows should be kept and which should
#' #  be discarded.
#' # In any case, if upon inspection one reaches a different
#' #  conclusion, the column 'keep' can be modified accordingly
#'
#' # just as an example, let's change the value of 'keep'
#' #  corresponding to row 4 in doubles
#' target <- 4
#'
#' # first we find the corresponding row in heart_rates
#' i <- which(
#'	heart_rates$i == doubles$i[target] &
#'	heart_rates$id == doubles$id[target])
#'
#' heart_rates$keep[i] <- TRUE # was FALSE but now is TRUE
pulse_plot_raw <- function(heart_rates, ID, target_time = NULL, range = 0, target_i = NULL) {

	stopifnot(sum(!is.null(target_time), !is.null(target_i)) == 1)

	# make sure timestamps are rounded to the second, so that they print nicely
	heart_rates$time <- lubridate::round_date(heart_rates$time, "secs")

	tmp <- heart_rates %>%
		dplyr::filter(id == ID) %>%
		tibble::add_column(col = "1")

	if (!is.null(target_time)) {
		TZ <- lubridate::tz(tmp$time[1])
		target_time <- as.POSIXct(target_time, tz = TZ)
		target_i <- which.min(abs(tmp$time - target_time))
	}

	target_i_rng <- (target_i - range):(target_i + range)

	tmp$col[target_i] <- "2"

	tmp <- dplyr::slice(tmp, target_i_rng)

	i <- range + 1

	diff <- tmp %>%
		dplyr::pull(data) %>%
		magrittr::extract2(i) %>%
		dplyr::pull(time) %>%
		range() %>%
		diff() %>%
		round(1)
	units(diff) <- "secs"

	str1 <- stringr::str_c(ID, " @ ", tmp$time[i], " (target_i = ", target_i, ")")
	str2 <- stringr::str_c(tmp$n[i], " wave peaks (over ", diff, " secs), hz = ", tmp$hz[i], " (", round(tmp$hz[i] * 60, 0), " bpm, sd = ", tmp$sd[i], ")")

	tmp <- tmp %>%
		dplyr::rename(t = time) %>%
		dplyr::mutate(label = stringr::str_c(as.character(t), " (sd of the intervals = ", sd, ")")) %>%
		tidyr::unnest(cols = "data")

	p <- ggplot2::ggplot() +
		ggplot2::ggtitle(label = str1, subtitle = str2) +
		ggplot2::xlab("") + ggplot2::ylab("") +
		ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank()) +
		ggplot2::geom_line(data = tmp, ggplot2::aes(time, val, col = col)) +
		ggplot2::scale_color_manual(values = if (range == 0) "black" else c("grey60", "black"), guide = "none") +
		ggplot2::geom_point(data = dplyr::filter(tmp, peak), ggplot2::aes(time, val), col = "red") +
		ggplot2::facet_wrap(facets = "label", ncol = 1, scales = "free_x")

	# return
	p
}

#' Plot and animate PULSE data
#'
#' @description
#' **EXPERIMENTAL** - A shortcut function based on `ggplot2` and `gganimate` to facilitate the quick inspection of the quality of the analysis (with the peaks detected signaled with red dots). Useful to visually inspect the performance of the algorithm.
#'
#' @details
#' This function is **NOT meant** for high-level displaying of the data - it's simply a quick shortcut to facilitate data inspection.
#'
#' This function is **EXPERIMENTAL**, and the resulting animation may not be correctly formatted in every machine - please be patient and provide constructive feedback.
#'
#' On a machine running MacOS, the animation movie can easily be inspected using `QuickTime Player`.
#'
#' When inspecting the movie, go through each frame and assess if red dots top all heartbeats and no more than that. Additional info is displayed in the graph's title (such as sd). Difficult datasets may result in true heartbeats being missed or non-heartbeats (noise) being erroneously detected (false positives and/or false negatives). Note that the wider the time window (controlled by the `window_width_secs` parameter in [`pulse_split()`]) and the higher the heartbeat rate, the less critical are a few false positives or negatives (over a 10 secs window, missing 1 peak in 10 results in hz to drop by 10% (from 1 to 0.9), while over a 30 secs window, missing 1 peak in 30 results in a drop of 3.33% (from 1 to 0.967), and missing 1 peak in 60 results in a drop of just 1.7%. With the animated plot, it becomes much quicker and easy to inspect the overall performance of the analysis over a large dataset.
#'
#' @inheritParams pulse_plot
#' @param folder A valid path to an existing folder where the file `anim.mp4` will be saved. If not provided, `anim.mp4` will be saved to the current work directory. In any case, the final path is always printed at the end.
#' @param fps An integer representing the number of frames per second in the animation; defaults to `10`; more details below.
#' @param ID String naming a single or several target channel ids (each must match exactly); defaults to `NULL`, which results in all IDs being plotted
#'
#' @return
#' An `.mp4` file is saved and its path printed to the console.
#'
#' @section More on setting the right value for `fps`:
#' The default value of `10` generates a shorter video with 10 individual graphs displayed over 1 second. At this rate, the resulting animation file is smaller, the animation plays quicker and detailed inspection requires pausing the animation playback (but frame-by-frame inspection is still possible). This is ideal for large datasets.
#' Alternatively, lower values will result in each individual graph persisting in the animation for a longer period, making it easier to inspect without the need to hit pause and play so often. Naturaly, the file size will also be larger, and if the dataset is large then the animation may extend for an uncomfortably long period of time. A value of `1` will result in only one graph being shown over each second.
#'
#' @export
#'
#' @seealso
#'  * use [pulse_plot()] to plot processed PULSE data for a several channels
#'  * [pulse_heart()] or [PULSE()] generate the input for `pulse_anim`
#'
#' @examples
#' ## Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' heart_rates <- PULSE(
#'   paths,
#'   discard_channels  = paste0("s", 5:10),
#'   window_width_secs = 30,
#'   window_shift_secs = 60,
#'   min_data_points   = 0.8,
#'   interpolation_freq = 40,
#'   bandwidth   = 0.2,
#'   with_progress = TRUE
#'   )
#' ## End prepare data ----
#'
#' \dontrun{
#'
#' # data from only one channel
#' pulse_anim(heart_rates, ID = "limpet_1")
#'
#' # all channels
#' pulse_anim(heart_rates)
#' # notice how quickly one can inspect the entire dataset
#' #  and determine that only channel "limpet_1" has recorded good data
#' }
pulse_anim <- function(heart_rates, folder = NULL, fps = 10, ID = NULL) {
	if (is.null(folder)) folder <- getwd()
	if (!dir.exists(dirname(folder))) stop("\n  --> [x] the folder provided doesn't exist")

	files <- dir(folder) %>% stringr::str_subset(pattern = "^anim_.+\\.mp4$")
	if (!length(files))	{
		path <- file.path(folder, "anim_01.mp4")
	} else {
		i <- substr(files, 6, 7) %>%
			as.numeric() %>%
			max()
		path <- file.path(folder, paste0("anim_", formatC(i + 1, flag = "0", width = 2), ".mp4"))
	}
	if (file.exists(path)) stop("\n  --> [x] something went wrong when trying to come up with a new\n       filename for the animation file\n  --> [i] the easiest solution is to save to a folder where there\n       aren't any animation files saved yet")

	if (is.null(ID)) ID <- unique(heart_rates$id)

	i_char <- heart_rates$i %>% max() %>% nchar()
	n_char <- heart_rates$n %>% max() %>% nchar()

	tmp <- heart_rates %>%
		dplyr::filter(id %in% ID) %>%
		dplyr::rename(t = time) %>%
		dplyr::arrange(id, t) %>%
		dplyr::mutate(
			state = stringr::str_c(
				"\n", id, "  (i = ", formatC(i, width = i_char), ")",
				"\n", t,
				"\n(hz = ", formatC(hz, digits = 2, format = "f"),
				", bpm = ", formatC(hz * 60, digits = 1, format = "f"), ")",
				"\n(n peaks = ", formatC(  n, width = n_char),
				", sd = ",  formatC(sd, digits = 2, format = "f"),
				" [ ", ifelse(sd < 1, "*", " "), ifelse(sd < 0.5, "*", " "), ifelse(sd < 0.2, "*", " "), " ] )"
			),
			data = purrr::map(data, ~dplyr::mutate(.x, time = time - as.numeric(min(time))))
		) %>%
		tidyr::unnest(data)

	anim <- ggplot2::ggplot() +
		ggplot2::ggtitle("", subtitle = "{closest_state}") +
		ggplot2::xlab("") + ggplot2::ylab("") +
		ggplot2::theme(
			axis.text = ggplot2::element_blank(),
			axis.ticks = ggplot2::element_blank()
		) +
		ggplot2::geom_line(
			data = tmp,
			ggplot2::aes(time, val, col = id)) +
		ggplot2::geom_point(
			data = dplyr::filter(tmp, peak),
			ggplot2::aes(time, val),
			col = "red") +
		ggplot2::theme(
			legend.position = "bottom" ,
			plot.margin = ggplot2::margin(t = 70)
			) +
		gganimate::transition_states(state, wrap = FALSE)

	gganimate::animate(anim,
										 nframes = dplyr::n_distinct(tmp$state),
										 fps = fps,
										 renderer = gganimate::av_renderer(file = basename(path)))

	message("\n\n  --> [i] path to animation file:\n    ", path)
}

