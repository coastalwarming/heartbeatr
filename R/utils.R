data <- t_pks <- id <- sd <- n <- msg <- time <- hz <- hz_norm <- peak <- val <- ci <- bpm_ci <- val_ci <- state <- d_r <- d_f <- i <- keep <- path <- multi <- x <- y <- . <- NULL

#' heartbeatr utility function
#' @description
#' `heartbeatr-package` utility function
#'
#' @param path file path
#' @export
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

	# return
	has.eb & has.pulse
}

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
#' Take the output from [`PULSE()`] (or [`pulse_heart()`], or [`pulse_doublecheck()`]) and add a new column `hz_norm`, containing the normalized heartbeat rate. The normalization of heartbeat rates is achieved by computing, for each individual (i.e., PULSE channel), the average heartbeat rate during a reference baseline period (ideally measured during acclimation, before the treatment is initiated).
#'
#' @section Details:
#' Normalizing heartbeat rates is important because even individuals from the same species, the same age cohort and subjected to the same treatment will have different basal heart beat frequencies. After normalizing, these differences are minimized, and the analysis can focus on the change of hear beat frequency relative to a reference period (i.e., the baseline period chosen) rather than on the absolute values of heart beat freaquency - which can be misleading.
#'
#' The period chosen for the baseline doesn't need to be long - it's much more important that conditions (and hopefully heart beat frequencies) are as stable and least stressful as possible during that period.
#'
#' After normalization, heart beat frequencies during the baseline period will, by definition, average to `1`. Elsewhere, normalized heart beat frequencies represent ratios against the baseline: `2` represents a heart beat frequency double the basal frequency, while `0.5` indicates half of the basal frequency This means that two individuals may experience a doubling of heart beat frequency throughout an experiment even if their absolute heart beat frequencies are markedly different from each other (e.g., individual 1 with hz = 0.6 at t0 and hz = 1.2 at t1, and individual 2 with hz = 0.8 at t0 and hz = 1.6 at t1, will both show hz_norm = 1 at t0 and hz_norm = 2 at t1).
#'
#' @section Different baseline periods for each channel:
#' `pulse_normalize` only allows setting a single baseline period. If different periods are needed for different channels or groups of channels, generate two or more subsets of `heart_rates` containing `heart_rates$id` that share the same baseline periods, normalize and bind the data together at the end (see example below).
#'
#' @param heart_rates The output of [PULSE()], [`pulse_heart()`] or [`pulse_doublecheck()`].
#' @param t0 Either `NULL` (default) or a [`POSIXct`] object pointing to the beginning of the period to be used to establish the baseline heart beat frequency (same value is used for all channels). If set to `NULL`, the baseline period is set to the earliest timestamp available.
#' @param span_mins Number of minutes since `t0`, indicating the width of the baseline period (baseline from `t0` to (`t0` + `span_mins` * 60))
#'
#' @return The same tibble provided as input, with an additional column `hz_norm` containing the normalized heart beat frequencies.
#'
#' @export
#'
#' @seealso
#'  * [pulse_heart()] and [pulse_doublecheck()] are the functions that generate the input for `pulse_normalize`
#'  * [pulse_plot()] can be called to visualize the output from `pulse_normalize`
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, and its output can also be passed on to `pulse_normalize`
#'
#' @examples
#' ## Begin prepare data ----
#' pulse_data_sub <- pulse_data
#' pulse_data_sub$data <- pulse_data_sub$data[,1:5]
#' pulse_data_split <- pulse_split(
#'    pulse_data_sub,
#'    window_width_secs = 30,
#'    window_shift_secs = 60,
#'    min_data_points = 0.8,
#'    with_progress = TRUE)
#' pulse_data_split <- pulse_optimize(pulse_data_split, multi = pulse_data$multi)
#' heart_rates <- pulse_heart(pulse_data_split)
#' heart_rates <- pulse_doublecheck(heart_rates)
#' ## End prepare data ----
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
	if (!is.null(t0)) if (class(t0)[1] != "POSIXct") cli::cli_abort("t0 must be either 'NULL' or a POSIXct object")

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
	heart_rates
}

#' #' Summarise PULSE heartbeat rate estimates over new time windows
#' #'
#' #' @description
#' #' Take the output from [`PULSE()`] (or [`pulse_heart()`], [`pulse_doublecheck()`]) and summarise `hz` estimates over new user-defined time windows using `fun` (a summary function). In effect, this procedure reduces the number of data points available over time.
#' #'
#' #' Note that the output of `pulse_summarise()` can be inspected with [`pulse_plot()`] but not [`pulse_plot_raw()`].
#' #'
#' #' @section Details:
#' #' The PULSE multi-channel system captures data continuously. When processing those data, users should aim to obtain estimates of heart beat frequency at a rate that conforms to their system's natural temporal variability, or risk running into oversampling (which has important statistical implications and must be avoided or explicitly handled).
#' #'
#' #' With this in mind, users can follow two strategies:
#' #'
#' #' *If, for example, users are targeting 1 data point every 5 mins...*
#' #'
#' #' * If the raw data is of good quality (i.e., minimal noise, signal wave with large amplitude), users can opt for a relatively narrow split_window (e.g, by setting `window_width_secs` in [`PULSE()`] (or [`pulse_split()`]) to `30` secs) and to only sample split_windows every 5 mins with `window_shift_secs = 300`. This means that data is processed in 5-mins split-windows where 30 secs of data are used and four and a half mins of data are skipped, yielding our target of 1 data point every 5 mins. Doing so will greatly speed up the processing of the data (less and smaller windows to work on), and the final output will immediately have the desired sample frequency. On the other hand, if any of the split_windows effectively analysed features a gap in the data or happens to coincide with the occasional drop in signal quality, those estimates of heartbeat rate will reflect that lack of quality (even if *better* data may be present in the four and a half mins of data that is being skipped). This strategy is usually used at the beginning to assess the dataset, and depending on the results, the more time-consuming strategy described next may have to be used instead.
#' #'
#' #' * If sufficient computing power is available and/or the raw data can't be guaranteed to be high quality from beginning to end, users can opt for a strategy that scans the entire dataset without skipping any data. This can be achieved by setting `window_width_secs` and `window_shift_secs` in [`PULSE()`] (or [`pulse_split()`]) to the same low value. In this case, if both parameters are set to `30` secs, processing will take significantly longer and each 5 mins of data will result in `10` data points. Then, `pulse_summarise` can be used with `span_mins = 5` to summarise the data points back to the target sample frequency. More importantly, if the right summary function is used, this strategy can greatly reduce the negative impact of spurious *bad* readings. For example, setting `fun = median`, will reduce the contribution of values of `hz` that deviate from the center ("wrong" values) to the final heartbeat estimate for a given time window). Thus, if the computational penalty is bearable, this more robust strategy can prove useful.
#' #'
#' #' @inheritParams pulse_normalize
#' #' @param fun A function to summarise each new time window (defaults to `median`, which provides a good summary if all estimates are good while also minimizing the impact of spurious *not-so-good* estimates). If using a custom function, ensure that the function takes a vector of `numeric` values and outputs a single `numeric` value.
#' #' @param norm Logical indicating if the summarsing should be based on the raw Hz estimate (`FALSE`) or the normalized frequency (`TRUE`); note that this feature can only be used if the dataset has been processed with [pulse_normalize()] first; defaults to `FALSE`.
#' #' @param span_mins An integer expressing the width of the new summary windows (in `mins`, defaults to `10`)
#' #' @param min_data_points A numeric value indicating the minimum number of data points in each new summarizing window. Windows covering less data points are discarded. Defaults to `2`. If set to `0` (zero), no window is ever discarded.
#' #'
#' #' @return A similar tibble as the one provided for input, but fewer columns and rows. Among the columns now absent is the `data` column (raw data is no longer available). IMPORTANT NOTE: Despite retaining the same names, several columns present in the output now provide slightly different information (because they are recalculated for each summarizing window): `time` corresponds to the first time stamp of the summarizing window; `n` shows the number of valid original windows used by the summary function; `sd` represents the standard deviation of all heartbeat rate estimates within each summarizing window (and not the standard deviation of the intervals between each identified wave peak, as was the case in `heart_rates`); `ci` is the confidence interval of the new value for `hz`.
#' #'
#' #' @export
#' #'
#' #' @seealso
#' #'  * [pulse_heart()] and [pulse_doublecheck()] are the functions that generate the input for `pulse_summarise`
#' #'  * [pulse_plot()] can be called to visualize the output from `pulse_summarise`
#' #'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, and its output can also be passed on to `pulse_summarise`
#' #'
#' #' @examples
#' #' ## Begin prepare data ----
#' #' paths <- pulse_example("RAW_original_")
#' #' heart_rates <- PULSE(
#' #'   paths,
#' #'   discard_channels  = paste0("s", 5:10),
#' #'   window_width_secs = 30,
#' #'   window_shift_secs = 60,
#' #'   min_data_points   = 0.8,
#' #'   interpolation_freq = 40,
#' #'   bandwidth   = 0.2,
#' #'   with_progress = TRUE
#' #'   )
#' #' ## End prepare data ----
#' #'
#' #' # Summarise heartbeat estimates (1 data point every 5 mins)
#' #' nrow(heart_rates) # == 96
#' #' nrow(pulse_summarise(heart_rates, span_mins = 5)) # == 20
#' #' pulse_summarise(heart_rates, span_mins = 5)
#' #'
#' #' # Note that visualizing the output from 'plot_summarise()' with
#' #' #  'pulse_plot()' may result in many warnings
#' #' pulse_plot(pulse_summarise(heart_rates, span_mins = 5))
#' #' "> There were 44 warnings (use warnings() to see them)"
#' #'
#' #' # That happens when the value chosen for 'span_mins' is such
#' #' #  that the output from 'plot_summarise()' doesn't contain
#' #' #  enough data points for the smoothing curve to be computed
#' #' # Alternatively, do one of the following:
#' #'
#' #' # reduce 'span_mins' to still get enough data points
#' #' pulse_plot(pulse_summarise(heart_rates, span_mins = 2, min_data_points = 0))
#' #'
#' #' # or disable the smoothing curve
#' #' pulse_plot(pulse_summarise(heart_rates, span_mins = 5), smooth = FALSE)
#' pulse_summarise <- function(heart_rates, norm = FALSE, fun = stats::median, span_mins = 10, min_data_points = 2) {
#' 	# make whole (just in case)
#' 	span_mins <- floor(span_mins)
#'
#' 	if (norm) {
#' 		if (any(colnames(heart_rates) == "hz_norm")) {
#' 			heart_rates$hz <- heart_rates$hz_norm
#' 		} else {
#' 			stop("\n  --> [x] 'hz_norm' is missing in 'heart_rates'\n  --> [x] normalized heart beat frequencies cannot be used\n  --> [i] run 'heart_rates' through 'pulse_normalize()' first to fix this")
#' 		}
#' 	}
#'
#' 	# summarise
#' 	heart_rates <- heart_rates %>%
#' 		dplyr::group_by(
#' 			id,
#' 			time = lubridate::floor_date(
#' 				time,
#' 				stringr::str_c(span_mins, " mins")
#' 			)
#' 		) %>%
#' 		dplyr::summarise(
#' 			i  = min(i),
#' 			n  = dplyr::n(),
#' 			sd = stats::sd(hz),
#' 			hz = fun(hz),
#' 			ci = sd * 1.96,
#' 			.groups = "drop"
#' 		)
#'
#' 	# tidy
#' 	heart_rates <- heart_rates %>%
#' 		dplyr::filter(n > min_data_points) %>%
#' 		dplyr::relocate(i) %>%
#' 		dplyr::mutate(i = factor(i) %>% as.numeric()) %>%
#' 		dplyr::arrange(i)
#'
#' 	# output
#' 	heart_rates
#' }

