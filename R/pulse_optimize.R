#' Increase the number of data points in PULSE data through interpolation
#'
#' @description
#' The performance of the algorithm employed in the downstream function `pulse_heart()` for the detection of heart beat wave crests depends significantly on there being a sufficient number of data points around each crest. `pulse_interpolate()` reshapes the data non-destructively and improves the likelihood of `pulse_heart()` successfully estimating the inherent heartbeat rates.
#'
#' * `INTERPOLATION` is highly recommended because tests on real data have shown that a frequency of at least 40 Hz is crucial to ensure wave crests can be discerned even when the underlying heart beat rate is high (i.e., at rates above 2-3 Hz). Since the PULSE multi-channel system is not designed to capture data at such high rates (partially because it would generate files unnecessarily large), `pulse_interpolate()` is used instead to artificially increase the temporal resolution of the data by linearly interpolating to the target frequency. It is important to note that this process DOES NOT ALTER the shape of the heart beat wave, it just introduces intermediary data points. Also, the only downside to using very high values for `interpolation_freq` is the proportional increase in computing time and size of the outputs together with minimal improvements in the performance of `pulse_heart()` - but no artefacts are expected.
#'
#' @param split_window One element of `pulse_data_split` (which is the output from `pulse_split()`).
#' @param interpolation_freq A numeric value expressing the frequency (in Hz) to which PULSE data should be interpolated. Can be set to `0` (zero) or any value equal or greater than `40` (the default). If set to zero, no interpolation is performed.
#'
#' @return
#' The same PULSE tibble supplied in `split_window`, with the standard 2 or more columns (time + one or more channels), but now with data interpolated to `interpolation_freq` (i.e., with more data points)
#'
#' @export
#'
#' @seealso
#'  * [approx()] is used for the linear interpolation of PULSE data
#'  * [pulse_optimize()] is a wrapper function that executes `pulse_interpolate` and [pulse_smooth()] sequentially
#'  *  [pulse_read()], [pulse_split()], [pulse_heart()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
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
#' ## End prepare data ----
#'
#' # Interpolate data to 40 Hz
#' pulse_interpolate(pulse_data_split$data[[1]], 40)
pulse_interpolate <- function(split_window, interpolation_freq = 40) {
	stopifnot(is.pulse.tbl(split_window))
	stopifnot(is.numeric(interpolation_freq))
	stopifnot(length(interpolation_freq) == 1)
	if (!(interpolation_freq == 0 | interpolation_freq >= 40)) stop("\n  --> [x] interpolation_freq must be zero or a value >= 40")

	new_split_window <- split_window

	if (interpolation_freq != 0) {
		# get current timestamp stats
		time1  <- split_window$time

		t0 <- dplyr::first(time1)
		t1 <- dplyr::last(time1)

		span <- t1 - t0
		units(span) <- "secs"
		span <- as.numeric(span) %>% round()

		# determine the new timestamps
		npoints <- span * interpolation_freq
		time2 <- seq.POSIXt(t0, t1, length.out = npoints)

		# interpolate to time2
		new_split_window <- new_split_window %>%
			dplyr::select(-time) %>%
			dplyr::reframe(dplyr::across(dplyr::everything(), ~approx(time1, .x, time2)$y)) %>%
			tibble::add_column(time = time2) %>%
			dplyr::relocate(time) %>%
			tidyr::drop_na()
	}

	# return
	new_split_window
}

#' Smooth PULSE data
#'
#' @description
#' The performance of the algorithm employed in the downstream function `pulse_heart()` for the detection of heart beat wave crests depends significantly on the data not being too noisy. `pulse_smooth()` reshapes the data non-destructively and improves the likelihood of `pulse_heart()` successfully estimating the inherent heartbeat rates.
#'
#' * `SMOOTHING` isn't crucial, but should be experimented with when `pulse_heart()` produces too many heartbeat rate estimates that are clearly incorrect. In such situations, `pulse_smooth()` applies a smoothing filter (normal Kernel Regression Smoother) to the data to smooth out high-frequency noise and render a more sinusoidal wave, which is easier to handle. Users should exercise caution when setting `bandwidth` and generally opt for lower values, as there's a threshold to bandwidth values above which the resulting smoothed pulse data becomes completely unrelated to the original data, and the subsequent heartbeat rates computed with `pulse_heart()` will be totally wrong. Always double-check the data after applying a stronger smoothing. Note that if applied with the default `bandwidth`, smoothing incurs no penalty and hardly changes the data - so it isn't worth going out of the way to not apply smoothing.
#'
#' @inheritParams pulse_interpolate
#' @param bandwidth A numeric value expressing the bandwidth. If equal to `0` (zero) no smoothing is applied. Ideally kept low (defaults to `0.2`) so that only very high frequency noise is removed, but can be pushed up all the way to `1` or above (especially when the heartbeat rate is expected to be slow, as is typical of oysters, but double check the resulting data). Type `?ksmooth` for additional info.
#'
#' @return
#' The same PULSE tibble supplied in `split_window`, with the standard 2 or more columns (time + one or more channels), but now with data for all channels smoothed.
#'
#' @export
#'
#' @seealso
#'  * [ksmooth()] is used for the kernel smoothing of PULSE data
#'  * [pulse_optimize()] is a wrapper function that executes [pulse_interpolate()] and `pulse_smooth` sequentially
#'  *  [pulse_read()], [pulse_split()], [pulse_heart()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
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
#' ## End prepare data ----
#'
#' # Smooth data slightly ('bandwidth' = 2)
#' pulse_smooth(pulse_data_split$data[[1]], 0.2)
pulse_smooth <- function(split_window, bandwidth = 0.2) {
	stopifnot(is.pulse.tbl(split_window))
	stopifnot(is.numeric(bandwidth))
	stopifnot(length(bandwidth) == 1)

	new_split_window <- split_window

	if (bandwidth != 0) {
		# get current timestamps
		time1 <- split_window$time

		# smooth data
		new_split_window <- new_split_window %>%
			dplyr::select(-time) %>%
			dplyr::reframe(dplyr::across(dplyr::everything(), ~ksmooth(time1, .x, kernel = "normal", bandwidth = bandwidth)$y)) %>%
			tibble::add_column(time = time1) %>%
			dplyr::relocate(time) %>%
			tidyr::drop_na()
	}

	# return
	new_split_window
}

#' Optimize PULSE data through interpolation and smoothing (`STEP 3`)
#'
#' @description
#' * `step 1` -- [`pulse_read()`]
#' * `step 2` -- [`pulse_split()`]
#' * **`-->>` step 3 -- [`pulse_optimize()`] `<<--`**
#' * `step 4` -- [`pulse_heart()`]
#' * `step 5` -- [`pulse_doublecheck()`]
#'
#' IMPORTANT NOTE: `pulse_optimize()` can be skipped, but that is highly discouraged.
#'
#' The performance of the algorithm employed in the downstream function `pulse_heart()` for the detection of heartbeat wave crests depends significantly on `(i)` there being a sufficient number of data points around each crest and `(ii)` the data not being too noisy. `pulse_optimize()` uses first `pulse_interpolate()` and then `pulse_smooth()` to reshape the data non-destructively and improve the likelihood of `pulse_heart()` successfully estimating the inherent heartbeat rates.
#'
#' * `INTERPOLATION` is highly recommended because tests on real data have shown that a frequency of at least 40 Hz is crucial to ensure wave crests can be discerned even when the underlying heartbeat rate is high (i.e., at rates above 2-3 Hz). Since the PULSE multi-channel system is not designed to capture data at such high rates (partially because it would generate files unnecessarily large), `pulse_interpolate()` is used instead to artificially increase the temporal resolution of the data by linearly interpolating to the target frequency. It is important to note that this process DOES NOT ALTER the shape of the heart beat wave, it just introduces intermediary data points. Also, the only downside to using very high values for `interpolation_freq` is the proportional increase in computing time and size of the outputs together with minimal improvements in the performance of `pulse_heart()` - but no artefacts are expected.
#'
#' * `SMOOTHING` isn't as crucial, but should be experimented with when `pulse_heart()` produces too many heartbeat rate estimates that are clearly incorrect. In such situations, `pulse_smooth()` applies a smoothing filter (normal Kernel Regression Smoother) to the data to smooth out high-frequency noise and render a more sinusoidal wave, which is easier to handle. Unlike `interpolation_freq`, users should exercise caution when setting `bandwidth` and generally opt for lower values, as there's a threshold to bandwidth values above which the resulting smoothed pulse data becomes completely unrelated to the original data, and the subsequent heartbeat rates computed with `pulse_heart()` will be totally wrong. Always double-check the data after applying a stronger smoothing. Note that if applied with the default `bandwidth`, smoothing incurs no penalty and hardly changes the data - so it isn't worth going out of the way to not apply smoothing.
#'
#' @inheritParams pulse_interpolate
#' @inheritParams pulse_smooth
#' @param pulse_data_split The output of [`pulse_split()`], i.e., a tibble with three columns: $`i`, a numeric vector with the order indexes of each time window, $`smoothed`, a logical indicating if the data has been smoothed, and $`data`, a list with PULSE data split across time windows, each window being a subset of `pulse_data` (a tibble with at least 2 columns (time + one or more channels) containing PULSE data with timestamps within that time window)
#' @param raw_v_smoothed Logical. If set to `FALSE` (default), the output includes only one list obtained after applying interpolation and smoothing according to the values set. If set to `TRUE`, a list with two lists is returned, one after applying only interpolation (i.e., "raw"), and the other after applying both interpolation and smoothing (i.e, "smoothed"). Mostly used for supplying downstream functions with more robust information.
#'
#' @return
#' The same structure as the input data, which is a tibble with three columns, but now with the values on column $`smoothed` switched to `TRUE` if smoothing was applied and the contents of column $`data` modified in accordance with the parameters called. If `raw_v_smoothed` is `FALSE`, the tibble returned will have the same number of rows as the input tibble. If `raw_v_smoothed` is `TRUE`, the tibble returned will have twice the number of rows as the input tibble, with half not smoothed (i.e., only interpolation applied), the other half smoothed (i.e., interpolation and smoothing applied) and the order indexes in $`i` duplicated. Downstream functions will process both types of output automatically.
#'
#' @export
#'
#' @seealso
#'  * [approx()] is used by [pulse_interpolate()] for the linear interpolation of PULSE data
#'  * [ksmooth()] is used by [pulse_smooth()] for the kernel smoothing of PULSE data
#'  * `pulse_optimize` is a wrapper function that executes `pulse_interpolate` and `pulse_smooth` sequentially
#'  *  [pulse_read()], [pulse_split()], [pulse_heart()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
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
#' ## End prepare data ----
#'
#' # Optimize data by interpolating to 40 Hz and applying a slight smoothing
#' pulse_optimize(pulse_data_split, 40, 0.2)
pulse_optimize <- function(pulse_data_split, interpolation_freq = 40, bandwidth = 0.2, raw_v_smoothed = FALSE) {
	## CHECKS INITIATED ## ------------------- ##
	stopifnot(all(purrr::map_lgl(pulse_data_split$data, is.pulse.tbl)))
	stopifnot(is.numeric(interpolation_freq))
	stopifnot(length(interpolation_freq) == 1)
	if (!(interpolation_freq == 0 | interpolation_freq >= 40)) stop("\n  --> [x] interpolation_freq must be zero or a value >= 40")
	stopifnot(is.numeric(bandwidth))
	stopifnot(length(bandwidth) == 1)
	## CHECKS COMPLETED ## ------------------- ##

	pulse_data_split <-  pulse_data_split %>%
		dplyr::mutate(
			data = purrr::map(
				data,
				pulse_interpolate,
				interpolation_freq = interpolation_freq)
		)

	s_pulse_data_split <- pulse_data_split %>%
		dplyr::mutate(
			smoothed = TRUE,
			data = purrr::map(
				data,
				pulse_smooth,
				bandwidth = bandwidth)
		)

	# return
	if (raw_v_smoothed) {
		dplyr::bind_rows(
			pulse_data_split,
			s_pulse_data_split
		)
	} else {
		s_pulse_data_split
	}
}
