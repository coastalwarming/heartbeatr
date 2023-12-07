data <- t_pks <- id <- sd <- n <- msg <- time <- hz <- hz_norm <- peak <- val <- NULL

is.pulse <- function(path) {
	lines <- path %>%
		readr::read_lines(n_max = 50) %>%
		stringr::str_to_lower()

	has.eb <- lines %>%
		stringr::str_detect("www.electricblue.eu") %>%
		any()

	has.pulse <- lines %>%
		stringr::str_detect("pulse") %>%
		any()

	return(has.eb & has.pulse)
}

## CHECK THIS FUNCTION
is.pulse.tbl <- function(tbl) {
	lgl <- all(tibble::is_tibble(tbl))
	if (lgl) lgl <- (colnames(tbl)[1] == "time")
	if (lgl) lgl <- (class(tbl$time)[1] == "POSIXct")
	if (lgl) lgl <- (all(class(tbl[,-1] %>% unlist()) == "numeric"))
	lgl
}

is.pulse.multi <- function(path) {
	lines <- path %>%
		readr::read_lines(n_max = 50) %>%
		stringr::str_to_lower()  %>%
		stringr::str_detect("downloading device type") %>%
		any() %>%
		magrittr::not()
}

#' Get paths to pulse example files
#'
#' @description
#' `heartbeatr-package` comes bundled with several sample files in its inst/extdata directory. This function make them easy to access
#'
#' @param pattern Pattern to select one or more example files. Pattern is vectorized, so more than one value can be supplied. If NULL, all example files are listed.
#'
#' @return The full path to one or more example files, or the filenames of all example files available.
#' @export
#'
#' @seealso
#'  * [pulse_read()] can be used to read data from the example files
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, and it can be called to read and process the example files
#'
#' @examples
#' # Get the filenames of all example files
#' pulse_example()
#'
#' # Get the full path to a target example file
#' pulse_example("RAW_original_20221229_1350.CSV")
#'
#' # Get the full paths to any example files matching a search string
#' pulse_example("RAW_or")
#'
#' # 'pulse_example()' is vectorized, meaning that multiple search strings can be used
#' pulse_example(c("RAW_or", "not_a_"))
pulse_example <- function(pattern = NULL) {
	filenames <- dir(system.file("extdata", package = "heartbeatr"))
	if (is.null(pattern)) {
		filenames
	} else {
		targets <- pattern %>%
			purrr::map(~stringr::str_subset(filenames, .x)) %>%
			unlist()
		system.file("extdata", targets, package = "heartbeatr", mustWork = TRUE)
	}
}

#' Normalize PULSE heartbeat rate estimates
#'
#' @description
#' Take the output from [`PULSE()`] (or [`pulse_heart()`]) and add a new column `hz_norm` containing the normalized heartbeat rate. The normalization of heartbeat rates is achieved by computing, for each individual (i.e., PULSE channel), the average heartbeat rate during a reference baseline period (ideally measured during acclimation, before the experiment is initiated).
#'
#' @section Details:
#' Normalizing heartbeat rates is important because even individuals from the same species, the same age cohort and subjected to the same treatment will have different basal heartbeat rates. By normalizing these differences are minimized, and the analysis can focus on the change of hearbeat rate relative to a reference period (i.e., the baseline period chosen) rather than on the absolute values of heartbeat rate - which can be misleading.
#'
#' The period chosen for the baseline doesn't need to be long - it's much more important that conditions (and hopefully heartbeat rates) are as stable as possible during that period.
#'
#' After normalization, heartbeat rates during the baseline period will, by definition, average `1`. Elsewhere, normalized heartbeat rates represent ratios against the baseline: `2` representa a doubling of the basal rate, while `0.5` indicates half of the basal rate. This means that two individuals may experience a doubling of heartbeat rate throughout an experiment even if their absolute heartbeat rates are markedly different from each other (e.g., individual 1 with hz at t0 = 0.6 and hz at t1 = 1.2, and individual 2 with hz at t0 = 0.8 and hz at t1 = 1.6 will both show hz_norm at t0 = 1 and hz_norm at t1 = 2).
#'
#' @section Different baseline periods for each channel:
#' [`pulse_normalize()`] only allows setting a single baseline period. If different periods are needed for different channels or groups of channels, generate two or more subsets of `heart_rates` containing `heart_rates$id` that share the same baseline periods, normalize and bind the data together at the end (see example below).
#'
#' @param heart_rates The output of [PULSE()] or [`pulse_heart()`]
#' @param t0 Either `NULL` (default) or a [`POSIXct`] object pointing to the beginning of the period to be used to establish the baseline hearbeat rate (same value is used for all channels). If set to `NULL`, the baseline period is set to the earliest timestamp available.
#' @param span_mins Number of minutes since `t0`, indicating the width of the baseline period (baseline from `t0` to (`t0` + `span_mins` * 60)
#'
#' @return The same tibble provided in `heart_rates`, with an additional column `hz_norm` containing the normalized heartbeat rates
#'
#' @export
#'
#' @seealso
#'  * [pulse_heart()] is the function that generates the input for `pulse_normalize`
#'  * [pulse_plot_all()] can be called to visualize the output from `pulse_normalize`
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, and its output can also be passed on to `pulse_normalize`
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
#' heart_rates <- pulse_heart(pulse_data_split)
#' # End prepare data ----
#'
#' # Normalize data using the same period as baseline for all channels
#' pulse_normalize(heart_rates)
#'
#' # Apply different baseline periods to two groups of IDs
#' group_1 <- c("limpet_1", "limpet_2")
#' rbind(
#'   pulse_normalize(heart_rates[ (heart_rates$id %in% group_1), ], span_mins = 10),
#'   pulse_normalize(heart_rates[!(heart_rates$id %in% group_1), ], span_mins = 30)
#' )
pulse_normalize <- function(heart_rates, t0 = NULL, span_mins = 10) {
	stopifnot(is.numeric(span_mins))
	stopifnot(length(span_mins) == 1)
	if (!is.null(t0)) if (class(t0)[1] != "POSIXct") stop("\n  --> [x] t0 must be either 'NULL' or a POSIXct object")

	if (is.null(t0)) t0 <- min(heart_rates$time)

	# find the basal heart rate
	baseline <- heart_rates %>%
		dplyr::filter(dplyr::between(time, t0, t0 + span_mins * 60)) %>%
		dplyr::group_by(id) %>%
		dplyr::summarise(hz_norm = mean(hz))

	# compute the normalized heartbeat rate
	heart_rates <- dplyr::left_join(heart_rates, baseline, by = "id") %>%
		dplyr::mutate(hz_norm = hz / hz_norm)

	# return
	return(heart_rates)
}

#' Summarise PULSE heartbeat rate estimates over new time windows
#'
#' @description
#' Take the output from [`PULSE()`] (or [`pulse_heart()`]) and summarise `hz` estimates over new user-defined time windows using `fun` (a summary function). In effect, this procedure reduces the number of data points available over time.
#'
#' Note that the output of `pulse_summarise()` can be inspected with [`pulse_plot_all()`] but not `pulse_plot_raw()`.
#'
#' @section Details:
#' The PULSE multi-channel system captures data continuously. When processing those data, users should aim to obtain estimates of heartbeat rate at a frequency that conforms to their system's natural temporal variability, or risk running into oversampling (which has important statistical implications and must be avoided or explicitly handled).
#'
#' With this in mind, users can follow two strategies:
#'
#' *If, for example, users are targeting 1 data point every 5 mins...*
#'
#' * If the raw data is of good quality (i.e., minimal noise, signal wave with large amplitude), users can opt for a relatively narrow split_window (e.g, by setting `window_width_secs` in [`PULSE()`] (or [`pulse_split()`]) to `30` secs) and to only sample split_windows every 5 mins with `window_shift_secs = 300`. This means that data is processed in 5-mins split-windows where 30 secs of data are used and four and a half mins of data are skipped, yielding our target of 1 data point every 5 mins. Doing so will greatly speed up the processing of the data (less windows to work on), and the final output will immediately have the desired sample frequency. On the other hand, if any of the split_windows effectively analysed features a gap in the data or happens to coincide with the occasional drop in signal quality, those estimates of heartbeat rate will reflect that lack of quality (even if *better* data may be present in the four and a half mins of skipped data). This strategy is usually used at the beginning to assess the dataset, and depending on the results the more time-consuming strategy described next may or may not be used instead.
#'
#' * If sufficient computing power is available and/or the raw data can't be guaranteed to be high quality from beginning to end, users can opt for a strategy that scans the entire dataset without skipping any data. This can be achieved by setting `window_width_secs` and `window_shift_secs` in [`PULSE()`] (or [`pulse_split()`]) to the same value. In this case, if both parameters are set to `30` secs, processing will take significantly longer and each 5 mins of data will result in `10` data points. Then, `pulse_summarise` can be used with `span_mins = 5` to summarise the data points back to the target sample frequency. More importantly, if the right summary function is used, this strategy can greatly reduce the negative impact of spurious *bad* readings. For example, setting `fun = median`, will reduce the contribution of values of `hz` that deviate from the center ("wrong" values) to the final heartbeat estimate for a given time window). Thus, if the computational penalty is bearable, this more robust strategy can prove useful.
#'
#' @inheritParams pulse_normalize
#' @param fun A function to summarise each new time window (defaults to `median`, which provides a good summary if all estimates are good while also minimizing the impact of spurious *not-so-good* estimates). If using a custom function, ensure that the function takes a vector of `numeric` values and outputs a single `numeric` value.
#' @param span_mins A numeric value expressing the width of the new summarizing windows (in `mins`, defaults to `10`)
#' @param min_data_points A numeric value indicating the minimum number of data points in each new summarizing window. Windows covering less data points are discarded. Defaults to `2`, but if set to `0` (zero) no window is discarded.
#'
#' @return A similar tibble as the one provided in `heart_rates`, but without the `data` column (raw data is no longer available) and with less rows. IMPORTANT NOTE: The column `sd` now represents the standard deviation of all heartbeat rate estimates within each summarizing window (and not the standard deviation of the intervals between each identified wave peak, as was the case in `heart_rates`).
#'
#' @export
#'
#' @seealso
#'  * [pulse_heart()] is the function that generates the input for `pulse_summarise`
#'  * [pulse_plot_all()] can be called to visualize the output from `pulse_summarise`
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, and its output can also be passed on to `pulse_summarise`
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
#' # Summarise heartbeat estimates (1 data point every 5 mins)
#' nrow(heart_rates) # == 95
#' nrow(pulse_summarise(heart_rates, span_mins = 5)) # == 20
#' pulse_summarise(heart_rates, span_mins = 5)
#'
#' # Note that visualizing the output from 'plot_summarise()' with
#' #  'pulse_plot_all()' may result in many warnings
#' pulse_plot_all(pulse_summarise(heart_rates, span_mins = 5))
#' "> There were 44 warnings (use warnings() to see them)"
#'
#' # That happens when the value chosen for 'span_mins' is such
#' #  that the output from 'plot_summarise()' doesn't contain
#' #  enough data points for the smoothing curve to be computed
#' # Alternatively, do one of the following:
#'
#' # reduce 'span_mins' to still get enough data points
#' pulse_plot_all(pulse_summarise(heart_rates, span_mins = 2, min_data_points = 0))
#'
#' # or disable the smoothing curve
#' pulse_plot_all(pulse_summarise(heart_rates, span_mins = 5), smooth = FALSE)
pulse_summarise <- function(heart_rates, fun = stats::median, span_mins = 10, min_data_points = 2) {
	heart_rates %>%
		dplyr::group_by(id, time = lubridate::floor_date(time, stringr::str_c(span_mins, " mins"))) %>%
		dplyr::summarise(
			sd = stats::sd(hz),
			hz = fun(hz),
			n  = dplyr::n(),
			.groups = "drop"
		) %>%
		dplyr::filter(n > min_data_points)
}
