#' Find peaks of waves in raw PULSE data
#' @description
#' `heartbeatr-package` Find peaks of waves in raw PULSE data
#'
#' @param t time
#' @param y val
#' @export
find_peaks <- function(t, y) {
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

	# sometimes the algorithm picks up peaks at the beginning or end of the
	# split window, and those must be rejected
	if (N > 20) {
		l <- 10
		pks_logical[1:l] <- FALSE
		pks_logical[(N-l+1):N] <- FALSE
	}

	pks <- which(pks_logical)
	pks
}

#' Determine the heart beat frequency in one PULSE channel
#'
#' @description
#' Take data from one PULSE channel and identify the heartbeat wave peaks using an algorithm that searches for maxima across multiple scales.
#'
#' @param split_window_one_channel A tibble with PULSE data for only one channel with columns `$time` and `$val`
#'
#' @return
#' A one-row tibble with 8 columns:
#' * `time`,  time at the center of split_window_one_channel$time
#' * `t_pks`, time stamps of each wave peak identified
#' * `n`,     number of wave peaks identified
#' * `sd`,    standard deviation of the intervals between wave peaks
# #' * `sd`,  standard deviation of the normalized intervals between wave peaks
#' * `hz`,    heartbeat rate estimate (in Hz)
#' * `ci`,    confidence interval (hz ± ci)
#'
#' @details
#' function builds upon code from https://github.com/ig248/pyampd
#'
# #' @section Standard Deviation:
# #' The `sd` outputed refers to the spread of the intervals between each peak identified. It is a measure of the quality of the raw data and the ability of the algorithm to identify a real heart beat. The lower the `sd`, the more regular are the intervals between peaks, and the more likely that the algorithm did find a real signal. Conversely, higher `sd`s indicate that the peaks are found at irregular intervals, and is an indication of poor quality data. Because the frequency of the heartbeat in the data influences the magnitude of the `sd`, the absolute values for the intervals between each peak are first normalized (divided by the mean of those intervals, thus becoming a proportion). This ensures that `sd` values from different split windows can be directly compared, but should caution before reading too much into those values.
# #' In detail, `sd` is computed by: 1) taking the timestamps for each peak identified \[`t_pks`, 2)\] computing the intervals between each pair of consecutive peaks \[`as.numeric(diff(t_pks))`\], 2) normalizing \[`intervals / mean(intervals)`\], and 3) computing `sd` \[`sd(intervals)`\].
#'
#' @section BPM:
#' To convert to Beats Per Minute, simply multiply `hz` and `ci` by 60.
#'
#' @export
#'
#' @seealso
#'  * [pulse_find_peaks_all_channels()] runs `pulse_find_peaks_one_channel` on all PULSE channels
#'  * [pulse_read()], [pulse_split()], [pulse_optimize()], [pulse_heart()], [pulse_doublecheck()] are the functions needed for the complete PULSE processing workflow
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
#' pulse_data_split <- pulse_optimize(pulse_data_split, multi = pulse_data$multi)
#' split_window <- pulse_data_split$data[[1]]
#' split_window_one_channel <- split_window[,1:2]
#' colnames(split_window_one_channel) <- c("time", "val")
#' # End prepare data ----
#'
#' ## Determine heartbeat rates in one channel in one time window
#' pulse_find_peaks_one_channel(split_window_one_channel)
pulse_find_peaks_one_channel <- function(split_window_one_channel) {

	t <- split_window_one_channel$time
	y <- split_window_one_channel$val

	has.peak <- split_window_one_channel %>%
		colnames() %>%
		"=="("peak") %>%
		any()

	if (has.peak) {
		pks <- which(split_window_one_channel$peak)
	} else {
		pks <- find_peaks(t, y)
		if (length(pks) < 3) {
			split_window_one_channel <- pulse_smooth1(split_window_one_channel)
			t <- split_window_one_channel$time
			y <- split_window_one_channel$val
			pks <- find_peaks(t, y)
		}
	}

	# compute stats
	t_pks <- t[pks]
	intervals <- t_pks %>% diff() %>% as.numeric()
	hz        <- mean(1 / intervals)
	hz_sd     <- stats::sd(intervals)
	hz_CI     <- hz_sd * 1.96

	# return
	tibble::tibble(
		time   = mean(t),
		t_pks  = list(t_pks),
		n      = length(pks),
		sd     = round(hz_sd,  3),
		hz     = round(hz,     3),
		ci     = round(hz_CI,  3)
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
#' * `data`, a list of tibbles with raw PULSE data for each combination of channel and window, with columns `time`, `val` and `peak` (`TRUE` when data points correspond to wave peaks)
#' * `n`,   number of wave peaks identified
#' * `sd`,  standard deviation of the intervals between wave peaks (normalized)
#' * `hz`,  heartbeat rate estimate (in Hz)
#' * `ci`,  confidence interval (hz ± ci)

#' @export
#'
#' @seealso
#'  * `pulse_find_peaks_all_channels` runs [pulse_find_peaks_one_channel()] on all PULSE channels
#'  * [pulse_read()], [pulse_split()], [pulse_optimize()] and [pulse_heart()] are the functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
#'
#' @section BPM:
#' To convert to Beats Per Minute, simply multiply `hz` and `ci` by 60.
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
#' split_window <- pulse_data_split$data[[1]]
#' ## End prepare data ----
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

#' Choose the best heart beat frequency estimate from among two estimates derived from raw and smoothed data
#'
#' @description
#' When running [pulse_optimize()] with `raw_v_smoothed = TRUE`, two estimates are generated for each data point, and `pulse_choose_keep` is used to automatically select the best one
#'
#' @inheritParams pulse_doublecheck
#'
#' @return
#' A tibble with the same structure as the input, but now with only one estimate for each combination of `id` and `time` (the one that was deemed better).
#'
#' @export
#'
#' @seealso
#'  *  [pulse_read()], [pulse_split()], [pulse_optimize()], [pulse_heart()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
pulse_choose_keep <- function(heart_rates) {
	r_v_s <- heart_rates$smoothed %>%
		unique() %>%
		length() %>%
		"=="(2)

	if (r_v_s) {
		nchar    <- max(nchar(heart_rates$i))
		split_hr <- stringr::str_c(
			formatC(heart_rates$i, flag = "0", width = nchar),
			heart_rates$id,
			heart_rates$time
		)
		split_hr <- split(heart_rates, split_hr)

		heart_rates <- purrr::map_dfr(
			split_hr,
			~{
				r <- dplyr::filter(.x, !smoothed)
				s <- dplyr::filter(.x,  smoothed)

				r_na <- is.na(r$sd)
				s_na <- is.na(s$sd)

				out <- s # default output is the smoothed data

				if (
					(s_na) | # sd can't be computed for smoothed data
					(!s$keep) # smoothed data is valid but doesn't fit criteria
				) {
					# raw is kept even if it also is poor quality
					out <- r
				}
				out
			}
		)
	}
	heart_rates
}

#' Determine the heartbeat rate in all channels of a split PULSE object (`STEP 4`)
#'
#' @description
#' * `step 1` -- [pulse_read()]
#' * `step 2` -- [pulse_split()]
#' * `step 3` -- [pulse_optimize()]
#' * **`-->>` step 4 -- [pulse_heart()] `<<--`**
#' * `step 5` -- [pulse_doublecheck()]
#'
#' For each combination of channel and time window, determine the heartbeat rate automatically.
#'
#' `pulse_heart()` takes the output from a call to `pulse_optimize()` (or `pulse_split()` if optimization is skipped, but that is highly discouraged) and employs an algorithm optimized for the identification of wave peaks in noisy data to determine the heart beat frequency in all channels of the PULSE dataset.
#'
#' @inheritParams pulse_read
#' @inheritParams pulse_split
#' @inheritParams pulse_optimize
#' @param N Minimum number of peaks detected in each time window for it to be considered a "keep" (defaults to `3`)
#' @param SD Maximum value for the sd of the time intervals between each peak detected for it to be considered a "keep" (defaults to `0.5`)
#'
#' @return
#' A tibble with nrows = (number of channels) * (number of windows in `pulse_data_split`) and 10 columns:
#' * `i`, index of each time window's order
#' * `smoothed`, whether the data has been smoothed with [pulse_smooth()]
#' * `id`, PULSE channel IDs
#' * `time`, time at the center of each time window
#' * `data`, a list of tibbles with raw PULSE data for each combination of channel and window, with columns `time`, `val` and `peak` (`TRUE` in rows corresponding to wave peaks)
#' * `n`,   number of wave peaks identified
#' * `sd`,  standard deviation of the intervals between wave peaks
#' * `hz`,  heartbeat rate estimate (in Hz)
#' * `ci`,  confidence interval (hz ± ci)
#' * `keep`, whether `n` and `sd` are within the target thresholds
#'
#' @section BPM:
#' To convert to Beats Per Minute, simply multiply `hz` and `ci` by 60.
#'
#' @export
#'
#' @seealso
#'  * check [progressr::handlers()] to customize the reporting of progress
#'  * check [future::plan()] to optimize parallel processing
#'  * [pulse_find_peaks_all_channels()] runs [pulse_find_peaks_one_channel()] on all PULSE channels
#'  *  [pulse_read()], [pulse_split()], [pulse_optimize()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
 # * [pulse_summarise()] can be used to reduce the number of data points returned
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
#' pulse_data_split <- pulse_optimize(
#'		pulse_data_split,
#'		raw_v_smoothed = TRUE,
#'		multi = pulse_data$multi)
#' ## End prepare data ----
#'
#' # Determine heartbeat rates in all channels in all time window
#' pulse_heart(pulse_data_split)
pulse_heart <- function(pulse_data_split, N = 3, SD = 0.5, with_progress = NULL, msg = TRUE) {
	## CHECKS INITIATED ## ------------------- ##
	stopifnot(all(purrr::map_lgl(pulse_data_split$data, is.pulse.tbl)))
	stopifnot(is.logical(msg))
	## CHECKS COMPLETED ## ------------------- ##

	# set up parallel computing
	current_strategy <- future::plan() %>%
		class() %>%
		magrittr::extract(2)
	if (msg) {
		if (current_strategy == "sequential") {
			cli::cli_alert("parallel computing not engaged")
			cli::cli_alert("if too slow, type ?PULSE for help on how to use parallel computing")
		} else {
			cli::cli_alert("parallel computing engaged")
			cli::cli_alert(stringr::str_c("[i] current future strategy: ", current_strategy, "\n"))
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
	bar <- progressr::progressor(along = pulse_data_split$data)

	heart_rates <- future.apply::future_lapply(
		pulse_data_split$data,
		function(x) {
			bar(message = "determining heart rates |")
			pulse_find_peaks_all_channels(x)
		})

	# tidy
	heart_rates <- pulse_data_split %>%
		dplyr::select(-data) %>%
		tibble::add_column(heart = heart_rates) %>%
		tidyr::unnest(cols = "heart")

	# keep (check if n and sd meet the provided thresholds)
	heart_rates <- heart_rates %>%
		dplyr::mutate(
			keep = (n > N) & (sd < SD),
			keep = dplyr::if_else(is.na(keep), FALSE, keep)
		)

	# when pulse_optimize was run with raw_v_smoothed = TRUE
	# heart_rates includes a raw and a smoothed version of each
	# data point, and only the best estimate will be kept
	heart_rates <- pulse_choose_keep(heart_rates)

	# return
	heart_rates
}

#' Fix heart rate frequencies double the real value (`STEP 5`)
#'
#' @description
#' * `step 1` -- [pulse_read()]
#' * `step 2` -- [pulse_split()]
#' * `step 3` -- [pulse_optimize()]
#' * `step 4` -- [pulse_heart()]
#' * **`-->>` step 5 -- [pulse_doublecheck()] `<<--`**
#'
#' @description
#' Flag (and correct) data points where it is likely that the heart rate frequency computed corresponds to double the actual heart rate frequency due to the algorithm having identified two peaks per heart beat
#'
#' @inheritParams pulse_heart
#' @param heart_rates The output from [pulse_heart()]
#' @param flag Decimal from 0 to 1. Values of `ratio` above this number will be flagged as instances where the algorithm resulted in double the real heart rate. Defaults to `0.9`. Values above `1`are meaningless (zero data points will be flagged), and values below `~0.66` are too lax (many data points will be flagged when they shouldn't).
#' @param correct Logical. If `FALSE`, data points with `hz` values likely double the real value are flagged **BUT NOT CORRECTED**. If `TRUE`, `hz` (as well as `data`, `n`, `sd` and `ci`) are corrected accordingly. Note that the correction is not reversible! (defaults to `TRUE`)
#'
#' @return A tibble similar to the one used as input, now augmented with three new columns: `d_r` and `d_f`. Values of `d_r` (ratio) close to `1` are indicative that the value for `hz` determined by the algorithm should be halved. If `correct`was set to `TRUE`, `d_f` flags data points where `hz` **HAS BEEN HALVED**. If `correct`was set to `FALSE`, then `d_f` flags data points where `hz` **SHOULD BE HALVED**.
#'
#' @section Heart beat frequency estimation:
#' For many invertebrates, the circulatory system includes more than one contractile chamber, meaning that there are two consecutive movements that may or may not be detected by the PULSE system's IR sensors. Furthermore, when the sensor is attached to the shell of the animal, it remains at a fixed position even as the soft body tissues move below that. As a result, even if one takes explicit care to position the sensor in such a way that only one wave peak is detected for each heart beat cycle, at some point the animal may move and the sensor's field of view may come to encompass both contractile chambers. When that occurs, the shape of the signal detected will include two peaks per heart beat cycle, the relative sizes of which may vary considerably. To be clear, there's nothing wrong with such a signal. However, it creates a problem: the algorithm detects peaks, and therefore, if two peaks are detected for each heart beat, the resulting estimate for the heart beat frequency will show a value twice as much as the real value.
#'
#' @section Detection method:
#' While it is often easy to discern if a PULSE data point has two peaks per heart beat upon visual inspection, to do so automatically is much harder. The strategy employed here relies on analyzing the intervals between consecutive peaks and looking for a regular alternation between longer and shorter intervals. If intervals are consistently shorter, then longer, then shorter again, we can assume that the distribution of interval times is bimodal, and that there are always two peaks more closer together separated by a longer interval - a classical two-peaks-per-heart-beat situation. For example, let's say 24 peaks are detected. We can compute the time span between each peak, which will correspond to 23 intervals (measured in seconds). Then, intervals can be classified as being long or short depending on where they are compared to their median value. Lastly, we divide the number of alternating intervals by the total number of intervals, deriving the ratio of switching intervals. The closer the ratio is to `1`, the more certain we are that we are facing a situation where the algorithm will result in a heart beat frequency twice the real value. Because the choice of a threshold to flag data points as in need to be halved or not is arbritary, both the flagging and the ratio are provided in the output, thus enabling a reassessment of the resulting classification.
#'
#' @export
#'
#' @seealso
#'  * [pulse_heart()] generates the tibble that is used as input.
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, including the identification of possible heart rate doublings
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
#' pulse_data_split <- pulse_optimize(
#'		pulse_data_split,
#'		raw_v_smoothed = TRUE,
#'		multi = pulse_data$multi)
#' heart_rates <- pulse_heart(pulse_data_split)
#' ## End prepare data ----
#'
#' # Correct heart beat frequency estimates
#' pulse_doublecheck(heart_rates)
pulse_doublecheck <- function(heart_rates, flag = 0.9, N = 3, SD = 0.5, correct = TRUE) {
	hr_data <- heart_rates$data

	# find the time spans between consecutive peaks
	diffs <- purrr::map(hr_data, ~.x %>%
												dplyr::filter(peak) %>%
												dplyr::pull(time) %>%
												diff() %>%
												as.numeric()
	)

	# identify which ones are above the median
	magnitudes <- purrr::map_dbl(diffs, ~ifelse(length(.x), (.x - quantile(.x, 0.5)) %>% range() %>% diff(), 0))

	positives  <- purrr::map(diffs, ~(.x - quantile(.x, 0.5) > 0))

	positives[purrr::map_lgl(positives, ~length(.x) == 0)]  <- FALSE
	positives[magnitudes < 1]  <- FALSE

	# compute the magnitude of the difference between the average value for the peaks above the mean, and the average value for the beaks below the mean
	# if heart beats where identified as two peaks, this is indicative of how different the two groups of time spans are
	# POS <- purrr::map2(positives, diffs, ~.y[ .x])
	# NEG <- purrr::map2(positives, diffs, ~.y[!.x])
	# MGN <- purrr::map2_dbl(POS, NEG, ~((.x %>% mean()) - (.y %>% mean())))

	# investigate if there's an alternation between values above and below the mean
	# values closer to 1 indicate that the alternation is perfect (one above, one below, one above, etc)
	alternates <- purrr::map(positives, ~.x %>%
													 	which() %>%
													 	diff() %>%
													 	"=="(2)
	)
	alternates[purrr::map_dbl(alternates, length) < 6] <- FALSE
	alternates <- purrr::map_dbl(alternates, ~sum(.x) / length(.x)) # the value is expressed as a ratio

	# tidy
	hr <- heart_rates %>%
		dplyr::mutate(
			d_r = alternates,
			# d_m = MGN,
			d_f = d_r > flag
		)

	# apply correction
	if (correct & any(hr$d_f)) {
		ii <- which(hr$d_f)
		hr_true <- hr %>%
			dplyr::slice(ii) %>%
			pulse_halve(N, SD)
		hr <- hr %>%
			dplyr::slice(-ii) %>%
			dplyr::bind_rows(hr_true) %>%
			dplyr::arrange(i, time, id)
	}

	# return
	hr
}

#' Halves heart beat frequencies computed by `pulse_heart`
#'
#' @description
#' Halves the heart beat frequency computed by `pulse_heart` when double peaks have been detected by `pulse_correct`. Note that the correction cannot be reverted (if just testing, store as a different variable). The associated stats are recalculated. This function is used by `pulse_correct`, it is not immediately usable as standalone.
#'
#' @inheritParams pulse_heart
#' @param hr A tibble as the one used as input to [pulse_doublecheck()], but with the additional column `d_f`, which flags rows where heart beat frequencies need to be halved. All rows supplied are halved, so input should be a filtered version of the full dataset.
#'
#' @return A tibble with as many rows as the one provided as input, but with `data`, `n`, `sd`, `hz` and `ci` adjusted accordingly.
#'
#' @export
#'
#' @seealso
#'  * [pulse_doublecheck()] is the function within the [`heartbeatr-package`] that uses `pulse_halve`
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once, including the identification and correction of possible heart rate doublings
#'
pulse_halve <- function(hr, N, SD) {

	stopifnot(tibble::is_tibble(hr))
	stopifnot(any(colnames(hr) == "d_f"))

	new_hr <- purrr::map_dfr(
		hr$data,
		~{
			x <- .x
			old_peaks <- which(x$peak)
			old_peaks <- old_peaks[1:(length(old_peaks) - (length(old_peaks) %% 2))] # length needs to be even
			pairs <- rep(seq(1, length(old_peaks) / 2), each = 2)
			if (length(unique(pairs)) > 2) {
				new_peaks <- old_peaks %>%
					split(pairs) %>%
					purrr::map_dbl(mean) %>%
					round()

				x$peak <- FALSE
				x$peak[new_peaks] <- TRUE
			}
			new_dat <- pulse_find_peaks_one_channel(x)
			new_dat$data <- list(x)
			new_dat
		}
	)

	hr$data <- new_hr$data
	hr$n    <- new_hr$n
	hr$sd   <- new_hr$sd
	hr$hz   <- new_hr$hz
	hr$ci   <- new_hr$ci
	hr$d_f  <- FALSE
	keep    <- (hr$n > N) & (hr$sd < SD)
	hr$keep <- dplyr::if_else(is.na(keep), FALSE, keep)

	hr
}

