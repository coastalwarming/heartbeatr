#' Process PULSE data from a single experiment  (`STEPS 1-4`)
#'
#' @description
#' **ALL STEPS EXECUTED SEQUENTIALLY**
#'
#' * `step 1` -- [`pulse_read()`]
#' * `step 2` -- [`pulse_split()`]
#' * `step 3` -- [`pulse_optimize()`]
#' * `step 4` -- [`pulse_heart()`]
#'
#' * `extra step` -- [`pulse_summarise()`]
#'
#' This is a wrapper function that provides a shortcut to running all 4 steps of the PULSE multi-channel data processing pipeline in sequence, namely `pulse_read()` >> `pulse_split()` >> `pulse_optimize()` >> `pulse_heart()`.
#'
#' **IMPORTANT NOTE**: [`pulse_summarise()`] is not included in [`PULSE()`] because it isn't essential for the PULSE data processing pipeline. However, in many instances it's important to run the output from [`PULSE()`] through `pulse_summarise()` before analysing the hearbeat rate data. This is because certain combinations of parameters may result in too many data points (leading to oversampling), a situation that can be resolved with `pulse_summarise()`. Be sure to check its help file (`?pulse_summarise`) before processing any large PULSE datasets, to understand the two main strategies that can be employed to handle oversampling and reduce sensitivity to pockets of poor-quality data across a dataset.
#'
#' `PULSE()` takes a vector of `paths` to PULSE csv files produced by a PULSE multi-channel system during **a single experiment** and automatically computes the heartbeat frequencies in all target channels across use-defined time windows. The entire workflow may take less than 5 minutes to run on a small dataset (a few hours of data) if (1) `params` are chosen with speed in mind, (2) parallel computing is enabled and (3) the code is run on a modern machine. Conversely, large datasets (spanning several days) may take hours or even days to run.
#'
#' @inheritParams pulse_read
#' @inheritParams pulse_split
#' @inheritParams pulse_optimize
#' @inheritParams pulse_heart
#' @param msg A logical to decide if non-crucial messages (but not errors) are shown (defaults to `TRUE`)
#'
#' @param discard_channels A string with the names of channels to be discarded from the analysis. `discard_channels` is forced to lowercase, but other than that, the **exact** names must be provided. Discarding unused channels can greatly speed the workflow!
#'
#' @section One experiment:
#' The PULSE workflow must be applied to a single experiment each time. By *experiment* we mean a collection of PULSE data where all the relevant parameters are invariant, including (but not limited):
#' * the version of the firmware installed in the PULSE multi-channel
#' * the names of all channels (including unused channels)
#' * the frequency at which data was captured
#'
#' Note also that even if two PULSE systems have been used in the same *scientific experiment*, data from each device must be processed independently, and only merged at the end. There's no drawback in doing so, it just is important to understand that that's how data must be processed by the [`heartbeatr-package`].
#'
#' @section Additional details:
#' Check the helpfiles of the underlying functions to obtain additional details about each of the steps implemented under `PULSE()`, namely:
#' * [`pulse_read()`] describes constraints to the type of files that can be read with the [`heartbeatr-package`].
#' * [`pulse_split()`] provides important advice to set `window_width_secs` and `window_shift_secs`, and what to expect when lower/higher values are used.
#' * [`pulse_optimize()`] explains in detail how the optimization process (interpolation + smoothing) behaves and how it impacts the performance of the analysis.
#' * [`pulse_heart()`] outlines the algorithm used to robustely identify peaks in the heartbeat wave data and some of its limitations.
#'
#' Also check [`pulse_summarise()`] for important info about oversampling and strategies to handle it while processing PULSE data with the [`heartbeatr-package`].
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
#' @seealso
#'  * check [progressr::handlers()] to customize the reporting of progress
#'  * check [future::plan()] to optimize parallel processing
#'  * [approx()] is used by [pulse_interpolate()] for the linear interpolation of PULSE data
#'  * [ksmooth()] is used by [pulse_smooth()] for the kernel smoothing of PULSE data
#'  * [pulse_read()], [pulse_split()], [pulse_optimize()] and [pulse_heart()] are the functions needed for the complete PULSE processing workflow
#'  * [pulse_summarise()] can be used to reduce the number of data points returned
#'
#' @examples
#' # Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' # End prepare data ----
#'
#' # Execute the entire PULSE data processing pipeline with only one call
#' PULSE(
#'   paths,
#'   discard_channels  = paste0("s", 5:10),
#'   window_width_secs = 30,
#'   window_shift_secs = 60,
#'   min_data_points   = 0.8,
#'   interpolation_freq = 40,
#'   bandwidth   = 0.2,
#'   with_progress = TRUE
#'   )
PULSE <- function(paths, discard_channels = NULL, window_width_secs, window_shift_secs, min_data_points, interpolation_freq = 40, bandwidth = 0.2, with_progress = NULL, msg = TRUE) {
	## CHECKS INITIATED ## ------------------- ##

	# pulse_read
	checks <- pulse_read_checks(paths)
	if (!checks$ok) {
		stop(checks$msg)
	}

	# pulse_split
	stopifnot(is.numeric(window_width_secs))
	stopifnot(length(window_width_secs) == 1)
	stopifnot(is.numeric(window_shift_secs))
	stopifnot(length(window_shift_secs) == 1)
	stopifnot(is.numeric(min_data_points))
	stopifnot(dplyr::between(min_data_points, 0, 1))

	# pulse_optimize
	stopifnot(is.numeric(interpolation_freq))
	stopifnot(length(interpolation_freq) == 1)
	if (!(interpolation_freq == 0 | interpolation_freq >= 40)) stop("\n  --> [x] interpolation_freq must be zero or a value >= 40")
	stopifnot(is.numeric(bandwidth))
	stopifnot(length(bandwidth) == 1)

	# pulse
	if (!is.null(discard_channels)) stopifnot(is.character(discard_channels))

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

	# read data
	pulse_data <- pulse_read(
		paths,
		with_progress = with_progress,
		msg = FALSE
	)

	# discard unused/unwanted channels
	if (!is.null(discard_channels)) {
		discard_channels <- stringr::str_to_lower(discard_channels)
		not_match <- discard_channels[!(discard_channels %in% colnames(pulse_data$data))]
		if (length(not_match)) stop(stringr::str_c("\n  --> [x] all elements of 'discard_channels' must be exact matches to a channel ID\n  --> [i] offending elements: ", stringr::str_c(not_match, collapse = ", ")))

		dups <- discard_channels[duplicated(discard_channels)]
		if (length(dups)) warning(stringr::str_c("  --> [x] all elements of 'discard_channels' should be unique channel IDs\n  --> [i] duplicated elements: ", stringr::str_c(dups, collapse = ", "), "\n  --> [i] work not interrupted, but consider revising 'discard_channels'"))

		pulse_data$data <- dplyr::select(pulse_data$data, -dplyr::any_of(discard_channels))
	}

	# split data
	pulse_data_split <- pulse_split(
		pulse_data,
		window_width_secs = window_width_secs,
		window_shift_secs = window_shift_secs,
		min_data_points = min_data_points,
		with_progress = with_progress,
		msg = FALSE
	)

	# optimize data
	pulse_data_split <- pulse_optimize(
		pulse_data_split,
		interpolation_freq = interpolation_freq,
		bandwidth = bandwidth
	)

	# compute heartbeat rates
	heart_rates <- pulse_heart(
		pulse_data_split,
		with_progress = with_progress,
		msg = FALSE
	)

	# return
	heart_rates
}
