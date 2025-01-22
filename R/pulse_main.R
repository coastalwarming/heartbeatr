#' Process PULSE data from a single experiment  (`STEPS 1-5`)
#'
#' @description
#' **ALL STEPS EXECUTED SEQUENTIALLY**
#'
#' * `step 1` -- [pulse_read()]
#' * `step 2` -- [pulse_split()]
#' * `step 3` -- [pulse_optimize()]
#' * `step 4` -- [pulse_heart()]
#' * `step 5` -- [pulse_doublecheck()]
#'
# * `extra step` -- [pulse_summarise()]
#'
#' This is a wrapper function that provides a shortcut to running all 5 steps of the PULSE multi-channel data processing pipeline in sequence, namely `pulse_read()` >> `pulse_split()` >> `pulse_optimize()` >> `pulse_heart()` >> `pulse_doublecheck()`.
#'
# **IMPORTANT NOTE**: [pulse_summarise()] is not included in [PULSE()] because it isn't essential for the PULSE data processing pipeline. However, in many instances it's important to run the output from [PULSE()] through `pulse_summarise()` before analyzing the estimated heart beat frequencies. This is because certain combinations of parameters may result in too many data points (leading to oversampling), a situation that can be resolved with `pulse_summarise()`. Be sure to check its help file (`?pulse_summarise`) before processing any large PULSE datasets, to understand the two main strategies that can be employed to handle oversampling and reduce sensitivity to pockets of poor-quality data across a dataset.
#'
#' `PULSE()` takes a vector of `paths` to PULSE csv files produced by a PULSE multi-channel system during **a single experiment** and automatically computes the heartbeat frequencies in all target channels across use-defined time windows. The entire workflow may take less than 5 minutes to run on a small dataset (a few hours of data) if (1) `params` are chosen with speed in mind, (2) parallel computing is enabled and (3) the code is run on a modern machine. Conversely, large datasets (spanning several days) may take hours or even days to run. In extreme situations, datasets may be too large for the machine to handle (due to memory limitations), and one is better off processing batches at a time.
#'
#' @inheritParams pulse_read
#' @inheritParams pulse_split
#' @inheritParams pulse_optimize
#' @inheritParams pulse_heart
#' @inheritParams pulse_doublecheck
#' @param msg A logical to decide if non-crucial messages (but not errors) are shown (defaults to `TRUE`)
#' @param discard_channels A string with the names of channels to be discarded from the analysis. `discard_channels` is forced to lowercase, but other than that, the **exact** names must be provided. Discarding unused channels can greatly speed the workflow!
#' @param raw_v_smoothed Logical indicating whether or not to also compute heart rates before applying smoothing; this will increase the quality of the output but also double the processing time (defaults to `TRUE`)
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
#' Check the help files of the underlying functions to obtain additional details about each of the steps implemented under `PULSE()`, namely:
#' * [pulse_read()] describes constraints to the type of files that can be read with the [`heartbeatr-package`] and explains how time zones are handled.
#' * [pulse_split()] provides important advice on how to set `window_width_secs` and `window_shift_secs`, what to expect when lower/higher values are used, and explains how easily to run the [`heartbeatr-package`] with parallel computing.
#' * [pulse_optimize()] explains in detail how the optimization process (interpolation + smoothing) behaves and how it impacts the performance of the analysis.
#' * [pulse_heart()] outlines the algorithm used to identify peaks in the heart beat wave data and some of its limitations.
#' * [pulse_doublecheck()] explains the method used to detect situations when the algorithm's processing resulted in an heart beat frequency double the real value.
#'
# Also check [pulse_summarise()] for important info about oversampling and strategies to handle it while processing PULSE data with the [`heartbeatr-package`].
#'
#' @section BPM:
#' To convert to Beats Per Minute (bpm), simply multiply `hz` and `ci` by 60.
#'
#' @return
#' A tibble with nrows = (number of channels) * (number of windows in `pulse_data_split`) and 13 columns:
#' * `i`, the order of each time window
#' * `smoothed`, logical flagging smoothed data
#' * `id`, PULSE channel IDs
#' * `time`, time at the center of each time window
#' * `data`, a list of tibbles with raw PULSE data for each combination of channel and window, with columns `time`, `val` and `peak` (`TRUE` in rows corresponding to wave peaks)
#' * `n`, number of wave peaks identified
#' * `sd`, standard deviation of the intervals between wave peaks
#' * `hz`, heartbeat rate estimate (in Hz)
#' * `ci`, confidence interval (hz ± ci)
#' * `keep`, logical indicating whether data points meet N and SD criteria
#' * `d_r`, ratio of consecutive asymmetric peaks
# * `d_m`, magnitude of difference between the two groups of asymmetric peaks
#' * `d_f`, logical flagging data points where heart beat frequency is likely double the real value
#'
#' @export
#'
#' @seealso
#'  * check [progressr::handlers()] to customize the reporting of progress
#'  * check [future::plan()] to optimize parallel processing
#'  * [approx()] is used by [pulse_interpolate()] for the linear interpolation of PULSE data
#'  * [ksmooth()] is used by [pulse_smooth()] for the kernel smoothing of PULSE data
#'  * [pulse_read()], [pulse_split()], [pulse_optimize()], [pulse_heart()] and [pulse_doublecheck()] are the functions needed for the complete PULSE processing workflow
 # * [pulse_summarise()] can be used to reduce the number of data points returned
#'
#' @examples
#' ## Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' ## End prepare data ----
#'
#' # Execute the entire PULSE data processing pipeline with only one call
#'
#' \dontrun{
#' # --> WITHOUT parallel computation
#'   require(future)
#'   # check current future plan
#'   future::plan()
#'   # set a parallelized future plan (and save the previous)
#'   old_plan <- future::plan("sequential")
#'
#'   PULSE(
#'  	paths,
#'  	discard_channels  = paste0("s", 5:10),
#'  	window_width_secs = 30,
#'  	window_shift_secs = 60,
#'  	min_data_points   = 0.8,
#'  	interpolation_freq = 40,
#'  	bandwidth   = 0.2,
#'  	raw_v_smoothed = TRUE,
#'  	N = 3,
#'  	SD = 0.5,
#'  	correct = TRUE,
#'  	with_progress = TRUE
#'   )
#'
#'   # reset future plan to the original value
#'   future::plan(old_plan)
#'   # confirm that the current future plan is set to the original value
#'   future::plan()}
#'
#' \dontrun{
#' # --> WITH parallel computation
#'   require(future)
#'   # check current future plan
#'   future::plan()
#'   # set a parallelized future plan (and save the previous)
#'   old_plan <- future::plan("multisession")
#'
#'   PULSE(...)
#'
#'   # reset future plan to the original value
#'   future::plan(old_plan)
#'   # confirm that the current future plan is set to the original value
#'   future::plan()}
#'
#' # Equivalent to...
#' pulse_data <- pulse_read(paths, with_progress = TRUE)
#'
#' keep_cols <- !(colnames(pulse_data$data) %in% paste0("s", 5:10))
#' pulse_data$data <- pulse_data$data[,keep_cols]
#'
#' pulse_data_split <- pulse_split(
#'							pulse_data,
#'							window_width_secs = 30,
#'							window_shift_secs = 60,
#'							min_data_points = 0.8,
#'							with_progress = TRUE)
#'
#' pulse_data_optimized <- pulse_optimize(
#'								pulse_data_split,
#'								interpolation_freq = 40,
#'								bandwidth = 0.2,
#'								raw_v_smoothed = TRUE,
#'								multi = pulse_data$multi)
#'
#' heart_rates <- pulse_heart(
#'						pulse_data_optimized,
#'						N = 3, SD = 0.5,
#'						with_progress = TRUE)
#'
#' heart_rates <- pulse_doublecheck(
#'						heart_rates,
#'						N = 3, SD = 0.5,
#'						correct = TRUE)
#'
PULSE <- function(paths, window_width_secs, window_shift_secs, min_data_points = 0.8, interpolation_freq = 40, bandwidth = 0.2, N = 3, SD = 0.5, raw_v_smoothed = TRUE, correct = TRUE, with_progress = NULL, discard_channels = NULL, msg = TRUE) {
	if (length(paths) == 1) if (file.info(paths)$isdir) paths <- dir(paths, full.names = TRUE) %>% tolower() %>% stringr::str_subset(".csv$")

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
	if (!(interpolation_freq == 0 | interpolation_freq >= 40)) cli::cli_abort("interpolation_freq must be zero or a value >= 40")
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
			cli::cli_alert("parallel computing not engaged")
			cli::cli_alert("if too slow, type ?PULSE for help on how to use parallel computing")
		} else {
			cli::cli_alert("parallel computing engaged")
			cli::cli_alert(stringr::str_c("[i] current future strategy: ", current_strategy, "\n"))
		}
	}

	# read data
	pulse_data <- pulse_read(
		paths,
		with_progress = with_progress,
		msg = FALSE
	)

	# discard unused/unwanted channels
	if (pulse_data$multi) {
		if (!is.null(discard_channels)) {
			discard_channels <- stringr::str_to_lower(discard_channels)
			not_match <- discard_channels[!(discard_channels %in% colnames(pulse_data$data))]
			if (length(not_match)) cli::cli_abort(stringr::str_c("\n  --> [x] all elements of 'discard_channels' must be exact matches to a channel ID\n  --> [i] offending elements: ", stringr::str_c(not_match, collapse = ", ")))

			dups <- discard_channels[duplicated(discard_channels)]
			if (length(dups)) cli::cli_warn(stringr::str_c("  --> [x] all elements of 'discard_channels' should be unique channel IDs\n  --> [i] duplicated elements: ", stringr::str_c(dups, collapse = ", "), "\n  --> [i] work not interrupted, but consider revising 'discard_channels'"))

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
	} else {
		pulse_data_split <- tibble::tibble(
			smoothed = FALSE,
			data = pulse_data$data$data
		) %>%
			tibble::rowid_to_column("i")
	}

	# optimize
	pulse_data_optimized <- pulse_optimize(
		pulse_data_split,
		interpolation_freq = interpolation_freq,
		bandwidth = bandwidth,
		raw_v_smoothed = TRUE,
		multi = pulse_data$multi
	)

	# heart rate
	heart_rates <- pulse_heart(
		pulse_data_optimized,
		N = N, SD = SD,
		with_progress = with_progress,
		msg = FALSE
	)

	# correct
	heart_rates <- pulse_doublecheck(
		heart_rates,
		N = N, SD = SD,
		correct = correct
	)

	if (!pulse_data$multi) {
		heart_rates <- dplyr::bind_cols(
			dplyr::select(pulse_data$data, -data, -time),
			dplyr::select(heart_rates, -id)
		) %>%
			dplyr::relocate(i)
	}

	# return
	heart_rates
}
#
# future::plan("multisession")
# folder <- "~/RS Dropbox/Rui Seabra/RS/bio/tasks/hb/hb_pulse_package/test_data_copy/large_dataset_rocio"
# folder <- "~/RS Dropbox/Rui Seabra/RS/bio/tasks/hb/hb_pulse_package/test_data_copy/one_channel_mariana"
# allow_dir_create = TRUE
# chunks = 3
# bind_data = TRUE
# window_width_secs = 30
# window_shift_secs = 60
# min_data_points   = 0.8
# interpolation_freq = 40
# bandwidth   = 0.2
# raw_v_smoothed = TRUE
# N = 3
# SD = 0.5
# correct = TRUE
# with_progress = TRUE
# discard_channels = NULL
# msg = TRUE

#' Process PULSE data file by file  (`STEPS 1-5`)
#'
#' @description
#' This function runs `PULSE()` file by file, instead of attempting to read all files at once. This is required when dataset are too large (more than 20-30 files), as otherwise the system may become stuck due to the amount of data that needs to be kept in the memory. Because the results of processing data for each hourly file in the dataset are saved to a `job_folder`, `PULSE_by_file()` has the added benefit of allowing the entire job to be stopped and resumed, facilitating the advance in the processing even if a crash occurs.
#'
#' @inheritParams pulse_read
#' @inheritParams pulse_split
#' @inheritParams pulse_optimize
#' @inheritParams pulse_heart
#' @inheritParams pulse_doublecheck
#' @inheritParams PULSE
#' @param folder The path to a folder where several PULSE files are stored
#' @param allow_dir_create Logical, defaults to FALSE. Only when set to TRUE does `PULSE_by_file()` actually do anything. This is to force the user to accept that a job_folder will be created inside of the `folder` supplied - without this folder `PULSE_by_file()` cannot operate. It is STRONGLY advised to maintain a copy of the dataset being processed to avoid any inadvertent data loss. By setting `allow_dir_create` to TRUE the user is taking responsibility for the management of their files.
#' @param chunks Numeric, defaults to 3; Corresponds to the number of files processed at once during each `for` cycle; higher numbers result in a quicker and more efficient operation, but shouldn't be set too high, as otherwise the system may become overwhelmed once more (which is what `PULSE_by_file()` is designed to avoid).
#' @param bind_data Logical, defaults to TRUE. If set to TRUE, after processing all chunks, `PULSE_by_file()` will try to read all files in the job_folder and return a single unified tibble with all data. Please be aware that there's a possibility that if the dataset is very large, the machine may become overwhelmed and crash due to lack of memory (still, all files stored in the job_folder will remain intact). If set to FALSE, `PULSE_by_file()` will return nothing after completing the processing of all files in the dataset, and the user must instead manually handle the reading and collating of all processed data in the job_folder.
#'
#' @export
#'
#' @seealso [PULSE()]
#'
#' @examples
#' ##
PULSE_by_file <- function(folder, allow_dir_create = FALSE, chunks = 3, bind_data = TRUE, window_width_secs, window_shift_secs, min_data_points = 0.8, interpolation_freq = 40, bandwidth = 0.2, N = 3, SD = 0.5, raw_v_smoothed = TRUE, correct = TRUE, with_progress = NULL, discard_channels = NULL, msg = TRUE) {

	if (!allow_dir_create) cli::cli_abort("you have not provided explicit permission for PULSE_chunks to create the necessary job folder; please set allow_dir_create = TRUE to proceed")

	paths <- folder %>%
		dir(full.names = TRUE, recursive = TRUE) %>%
		stringr::str_to_lower() %>%
		stringr::str_subset(".csv$")

	if (!all(purrr::map_lgl(paths, is.pulse))) cli::cli_abort("not all files in the target folder are PULSE files")

	job_folder <- file.path(folder, "ongoing_job")
	dir.create(job_folder, showWarnings = FALSE)

	n <- length(paths)
	chunks_split <- tibble::tibble(
		split = split(1:n, rep(1:ceiling(n/chunks), each = chunks)[1:n])
	) %>%
		tibble::rowid_to_column("row") %>%
		dplyr::mutate(row = formatC(row, width = nchar(nrow(.)), flag = "0")) %>%
		dplyr::mutate(fn = file.path(job_folder, paste0("chunk_", row, ".RDS")))

	for (i in 1:nrow(chunks_split)) {
		fn <- chunks_split$fn[i]
		if (!file.exists(fn)) {
			cli::cli_alert(paste(chunks_split$row[i], "/", nrow(chunks_split)))
			x <- PULSE(
				paths = paths[chunks_split$split[[i]]],
				window_width_secs  = window_width_secs,
				window_shift_secs  = window_shift_secs,
				min_data_points    = min_data_points,
				interpolation_freq = interpolation_freq,
				bandwidth          = bandwidth,
				raw_v_smoothed     = raw_v_smoothed,
				N                  = N,
				SD                 = SD,
				correct            = correct,
				with_progress      = with_progress,
				discard_channels   = discard_channels,
				msg                = ifelse(i == 1, msg, FALSE)
			)
			saveRDS(x, fn)
		}
	}

	cli::cli_alert_success("all files read and heart rates computed")
	cli::cli_alert_info(paste("data stored in:", job_folder))
	if (bind_data) {
		purrr::map_dfr(chunks_split$fn, readRDS)
	} else {
		cli::cli_warn("data not read and bound together because user set bind_data to FALSE")
		NULL
	}
}
