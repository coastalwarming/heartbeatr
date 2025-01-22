#' Split `pulse_data` across sequential time windows (`STEP 2`)
#'
#' @description
#' * `step 1` -- [`pulse_read()`]
#' * **`-->>` step 2 -- [`pulse_split()`] `<<--`**
#' * `step 3` -- [`pulse_optimize()`]
#' * `step 4` -- [`pulse_heart()`]
#' * `step 5` -- [`pulse_doublecheck()`]
#'
#' After all raw PULSE data has been imported, the dataset must be split across sequential time windows.
#'
#' `pulse_split()` takes the output from a call to `pulse_read()` and splits data across user-defined time windows. The output of `pulse_split()` can be immediately passed to `pulse_heart()`, or first optimized with `pulse_optimize()` and then passed to `pulse_heart()` (highly recommended).
#'
#' @inheritParams pulse_read
#' @param pulse_data The output from a call to `pulse_read()`, which results in a list with two elements:
#'  * `$data`, a tibble with one column with timestamps (named `time`) and one or more columns of numeric data (the voltage readings from each channel of the PULSE multi-channel system; all channels with unique names).
#'  * `$freq`, a single integer value representing the sampling frequency used (in Hz).
#' @param window_width_secs A single numeric value indicating the width of the time windows (in seconds) over which heart rate frequency will later be computed.
#' @param window_shift_secs A single numeric value indicating by how much each subsequent window is shifted from the preceding one (in seconds).
#' @param min_data_points A single numeric value, expressed as a ratio `[0, 1]`, used as a threshold to discard windows where data is missing (e.g., if the sampling frequency is `20` and `window_width_secs = 30`, each window should include `600` data points, and so if `min_data_points = 0.8`, windows with less than `600 * 0.8 = 480` data points will be rejected).
#'
#' @seealso
#'  * check [progressr::handlers()] to customize the reporting of progress
#'  * check [future::plan()] to optimize parallel processing
#'  * [pulse_read()], [pulse_optimize()], [pulse_heart()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
#'
#' @section Details:
#' `pulse_split()` takes the output from a call to `pulse_read()` and splits data across user-defined time windows.
#'
#' @section Window `width` and `shift`:
#' A good starting point for `window_width_secs` is to set it to between `30` and `60` seconds.
#'
#' As a rule of thumb, use lower values for data collected from animals with naturally faster heart rates and/or that have been subjected to treatments conducive to faster heart rates still (e.g., thermal performance ramps). In such cases, lower values will result in higher temporal resolution, which may be crucial if experimental conditions are changing rapidly. Conversely, experiments using animals with naturally slower heart rates and/or subjected to treatments that may cause heart rates to stabilize or even slow (e.g., control or cold treatments) may require the use of higher values for `window_width_secs`, as the resulting windows should encompass no less than 5-7 heartbeat cycles.
#'
#' As for `window_shift_secs`, set it to a value:
#' * smaller than `window_width_secs` if overlap between windows is desired (not usually recommended) (if `window_width_secs = 30` and `window_shift_secs = 15`, the first 3 windows will go from `[0, 30)`, `[15, 45)` and `[30, 60)`)
#' * equal to `window_width_secs` to process all data avaliable (if `window_width_secs = 30` and `window_shift_secs = 30`, the first 3 windows will go from `[0, 30)`, `[30, 60)` and `[60, 90)`)
#' * larger than `window_width_secs` to skip data (ideal for speeding up the processing of large datasets) (if `window_width_secs = 30` and `window_shift_secs = 60`, the first 3 windows will go from `[0, 30)`, `[60, 90)` and `[120, 150)`)
#'
#' In addition, also consider that lower values for the `window_...` parameters may lead to oversampling and a cascade of statistical issues, the resolution of which may end up negating any advantage gained.
#'
#' @section Handling gaps in the dataset:
#' `min_data_points` shouldn't be set too low, otherwise only nearly empty windows will be rejected.
#'
#' @section Speeding up for large datasets:
#' Processing large PULSE multi-channel datasets can take a long time. All main `pulse_...()` functions are built to allow parallelization based on the `futures` package. To engage parallel computing, simply set an appropriate `future` strategy before running `pulse_split()` (or at the begining of the PULSE data processing pipeline to make it available for all `pulse_...()` functions). Unless explicitly modified by the user, `future::plan()` defaults to `sequential`, i.e., not pararellized. Call `future::plan("multisession")` to use one of the most common parallelized `future` strategies. Call `?future::plan` for additional details.
#'
#' @return
#' A tibble with three columns. Column $`i` stores the order of each time window. Column $`smoothed` is a logical vector flagging smoothed data (at this point defaulting to `FALSE`, but later if [`pulse_optimize`] is used, values can change to `TRUE`. Column $`data` is a list with all the valid time windows (i.e., complying with `min_data_points`), each window being a subset of `pulse_data` (a tibble with at least 2 columns (time + one or more channels) containing PULSE data with timestamps within that time window)
#'
#' @export
#'
#' @examples
#' ## Begin prepare data ----
#' pulse_data_sub <- pulse_data
#' pulse_data_sub$data <- pulse_data_sub$data[,1:5]
#' ## End prepare data ----
#'
#' pulse_data_split <- pulse_split(
#'    pulse_data_sub,
#'    window_width_secs = 30,
#'    window_shift_secs = 60,
#'    min_data_points = 0.8,
#'    with_progress = TRUE)
#'
#' \dontrun{
#' # --> WITH parallel computation
#'   require(future)
#'   # check current future plan
#'   future::plan()
#'   # set a parallelized future plan (and save the previous)
#'   old_plan <- future::plan("multisession")
#'
#'   pulse_data_split <- pulse_split(...)
#'
#'   # reset future plan to the original value
#'   future::plan(old_plan)
#'   # confirm that the current future plan is set to the original value
#'   future::plan()}
pulse_split <- function(pulse_data, window_width_secs, window_shift_secs, min_data_points, with_progress = NULL, msg = TRUE) {
  ## CHECKS INITIATED ## ------------------- ##
  stopifnot(identical(names(pulse_data), c("data", "multi", "vrsn", "freq")))
  stopifnot(is.pulse.tbl(pulse_data$data))
  stopifnot(is.numeric(pulse_data$freq))
  stopifnot(is.numeric(window_width_secs))
  stopifnot(length(window_width_secs) == 1)
  stopifnot(is.numeric(window_shift_secs))
  stopifnot(length(window_shift_secs) == 1)
  stopifnot(is.numeric(min_data_points))
  stopifnot(dplyr::between(min_data_points, 0, 1))
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

  freq <- pulse_data$freq
  data <- pulse_data$data

  # define the target time windows
  t0 <- dplyr::first(data$time) %>%
    stringr::str_sub(1, 17) %>%
    stringr::str_c("00") %>%
    as.POSIXct() %>%
    magrittr::add(60)
  t1 <- dplyr::last(data$time)

  window_t0 <- seq(
    t0,
    t1,
    by = window_shift_secs)

  # set progress reporting strategy
  if (!is.null(with_progress)) {
    if (with_progress) {
      old_handlers <- progressr::handlers("cli")
      progressr::handlers(global = TRUE)
    } else {
      old_handlers <- progressr::handlers("void")
    }
    on.exit(if (is.null(old_handlers)) progressr::handlers("void") else progressr::handlers(old_handlers), add = TRUE)
  }

  # split data
  bar <- progressr::progressor(along = window_t0)

  pulse_data_split <- future.apply::future_lapply(
    window_t0,
    function(x) {
      bar(message = "splitting data across time windows |")
      dplyr::filter(data, dplyr::between(time, x, x + window_width_secs))
    })

  # discard windows that don't have enough data points
  #   used to skip data gaps (such as when the PULSE system is momentarily disconnected but the experiment resumes afterwards)
  min_data_points <- window_width_secs * freq * min_data_points
  nrows <- purrr::map_dbl(pulse_data_split, nrow)
  pulse_data_split  <- pulse_data_split[nrows >= min_data_points]

  # return
  tibble::tibble(
    i = seq_along(pulse_data_split),
    smoothed = FALSE,
    data = pulse_data_split
  )
}
