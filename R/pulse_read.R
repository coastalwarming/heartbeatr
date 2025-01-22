add_line <- function(msg1, msg2) {
  stringr::str_c(msg1, "\n - ", msg2)
}

read_field <- function(header, field, force_numeric = FALSE) {
  x <- header %>%
    stringr::str_subset(field) %>%
    stringr::str_split_1(",") %>%
    "["(2) %>%
    stringr::str_trim()
  if (force_numeric) x <- stringr::str_remove_all(x, "[^0-9.-]") %>% as.numeric()
  x
}

pulse_read_details <- function(paths, pulse_multi) {
  x <- tibble::tibble(
    path  = paths,
    multi = pulse_multi
    ) %>%
    tibble::rowid_to_column("i") %>%
    tibble::add_column(v = 0, skip = 0, freq = 0) %>%
    dplyr::mutate(header = purrr::map(path, ~readr::read_lines(.x, n_max = 100) %>% stringr::str_to_lower()))
  xt <- dplyr::filter(x,  multi)
  xf <- dplyr::filter(x, !multi)

  if (nrow(xt)) {
    xt$v <- xt$header %>%
      purrr::map_dbl(~.x %>%
                       stringr::str_subset("pulse version") %>%
                       stringr::str_replace_all("[^0-9.]", "") %>%
                       as.numeric()
      )

    xt$skip <- xt$header %>%
      purrr::map_dbl(~.x %>%
                       stringr::str_which("----------------") %>%
                       max()
      )

    xt$freq <- xt$header %>%
      purrr::map_dbl(~.x %>%
                       stringr::str_subset("rate_hz") %>%
                       stringr::str_replace_all("[^0-9.]", "") %>%
                       as.numeric()
      )
  }

  if (nrow(xf)) {
    xf$v <- xf$header %>%
      purrr::map_dbl(~.x %>%
                       stringr::str_subset("pulse version") %>%
                       stringr::str_replace_all("[^0-9.]", "") %>%
                       as.numeric()
      )

    xf$skip <- xf$header %>%
      purrr::map_dbl(~.x %>%
                       stringr::str_which("----------------") %>%
                       max()
      )

    xf$freq <- purrr::map2_dbl(xf$header, xf$skip, ~tail(.x, -.y - 1) %>%
                    stringr::str_split(",") %>%
                      purrr::map_chr(1) %>%
                      as.POSIXct(tz = "UTC") %>%
                      diff() %>%
                      as.numeric() %>%
                      mean() %>%
                      "/"(1, .) %>%
                      round()
    )
  }

  dplyr::bind_rows(xt, xf) %>%
    dplyr::arrange(i)

}

pulse_read_channels <- function(path) {
  path %>%
    readr::read_lines(n_max = 50) %>%
    stringr::str_to_lower() %>%
    stringr::str_remove("\n") %>%
    stringr::str_subset(pattern = "^s[:digit:]{1,2},") %>%
    stringr::str_split(",") %>%
    purrr::map_chr(2)
}

#' heartbeatr utility function
#' @description
#' `heartbeatr-package` utility function
#'
#' @param path file path
#' @param skip numeric
#' @param cols colnames
#' @param multi logical
#' @export
pulse_read_data <- function(path, skip, cols, multi) {
  if (multi) {
    lines <- path %>%
      readr::read_lines(skip = skip + 1) %>%
      stringr::str_remove("\n")
    lines <- lines[lines != ""]
    lines <- stringr::str_split(lines, ",")

    # check that timestamps have the correct length
    valid1 <- purrr::map_lgl(lines, ~nchar(.x[1]) > 10)

    # check that lines have the correct number of elements
    valid2 <- purrr::map_lgl(lines, ~length(.x) == if (multi) 11 else 2)

    # only keep lines that pass both tests
    lines <- lines[valid1 & valid2]

    # rebuild data
    data <- do.call(rbind, lines) %>%
      tibble::as_tibble(.name_repair = "minimal")
    colnames(data) <- c("time", cols)

    data <- data %>%
      dplyr::mutate(time = as.POSIXct(time, tz = "UTC")) %>%
      dplyr::mutate(dplyr::across(-time, ~as.numeric(.x)))

    return(data)
  } else {
    header <- readr::read_lines(path, n_max = skip) %>% stringr::str_to_lower()

    data <- readr::read_csv(path, skip = skip, show_col_types = FALSE)

    data <- tibble::tibble(
      sp   = read_field(header, "^sp,"),
      exp  = read_field(header, "^exp,"),
      lvl  = read_field(header, "^lvl,"),
      wat  = read_field(header, "^wat,"),
      dim  = read_field(header, "^dim \\(mm\\),", TRUE),
      temp = read_field(header, "^temperature", TRUE),
      time = data$time[1],
      data = list(data),
      lat  = read_field(header, "^lat,", TRUE),
      lon  = read_field(header, "^long,", TRUE),
      comm = read_field(header, "^comments,")
    )
  }
  data
}

#' heartbeatr utility function
#' @description
#' `heartbeatr-package` utility function
#'
#' @param paths file paths
#' @export
pulse_read_checks <- function(paths) {
  ok  <- FALSE
  out <- list()

  # check that all 'paths' exist
  if (!all(file.exists(paths))) {
    msg <- "\n  --> [x] all 'paths' must exist"
  } else {

    # check that all 'paths' are files (not folders)
    if (!all(!file.info(paths)$isdir)) {
      msg <- "\n  --> [x] 'paths' must point to files (not folders)"
    } else {

      # check that all paths point to PULSE files
      all_pulse <- paths %>%
        purrr::map_lgl(is.pulse) %>%
        all()

      if (!all_pulse) {
        msg <- "\n  --> [x] 'paths' must only include PULSE files"
      } else {
        # check that all paths point to files from either the PULSE multi-channel system
        # or the PULSE one-channel, but not both
        pulse_multi <- purrr::map_lgl(paths, is.pulse.multi)

        if (length(unique(pulse_multi)) != 1) {
          msg <- "\n  --> [x] 'paths' must point to only one type of PULSE files (multi or one-channel)"
        } else {
          headers <- pulse_read_details(paths, pulse_multi)

          freq <- unique(headers$freq)
          skip <- unique(headers$skip)
          vrsn <- unique(headers$v)

          if (pulse_multi[1]) {
            # check that all PULSE files share the same header
            #   (i.e., are from the same experiment)
            # this is done by ensuring that the names of the channels are
            #   the same in all files, as well as 'Rate_Hz' and the number of
            #   lines in the header
            cols <- purrr::map(paths, pulse_read_channels)
            COLS <- stringr::str_c("c", 1:length(cols[[1]]))
            cols <- do.call(rbind, cols)
            colnames(cols) <- COLS
            cols <- cols %>%
              tibble::as_tibble() %>%
              dplyr::distinct()

            lgl_head <- nrow(cols)   != 1
            min_vrsn <- 2.1
          } else {
            cols     <- c("time", "val")
            lgl_head <- FALSE
            min_vrsn <- 3.4
          }

          lgl_freq <- length(freq) != 1
          lgl_skip <- length(skip) != 1
          lgl_vrsn <- length(vrsn) != 1

          vrsn_min <- min(vrsn)
          old_vrsn <- vrsn_min < min_vrsn

          cols <- as.character(cols)

          if (lgl_head | lgl_freq | lgl_skip | lgl_vrsn | old_vrsn) {
            msg <- "\n  --> [x] found differences in the headers across the PULSE CSV files\n  --> [i] inconsistencies detected:"
            if (lgl_head) msg <- stringr::str_c(msg, "\n           - names of the channels (even unused channels must match)")
            if (lgl_freq) msg <- stringr::str_c(msg, "\n           - sampling rate 'Rate_Hz' ")
            if (lgl_skip) msg <- stringr::str_c(msg, "\n           - number of lines in headers")
            if (lgl_vrsn) msg <- stringr::str_c(msg, "\n           - multiple PULSE system versions (detected = ", stringr::str_c(vrsn, collapse = ", "), ")")
            if (old_vrsn) msg <- stringr::str_c(msg, "\n           - unsupported PULSE system versions (detected = ", vrsn_min, ", min = ", min_vrsn, ")")
            msg <- stringr::str_c(msg, "\n  --> [?] are you sure that all files originated from a single experiment?\n  --> [i] if yes, edit the headers manually to fix all inconsistencies and re-run the code")
          } else {
            # all checks passed
            ok <- TRUE
            out <- list(
              cols = cols,
              freq = freq,
              skip = skip,
              vrsn = vrsn
            )
            msg <- "  --> [i] 'paths' points to valid PULSE files"
          }
        }
      }
    }
  }
  list(ok = ok, multi = pulse_multi[1], msg = msg, out = out)
}

#' Read data from all PULSE files in the target folder (`STEP 1`)
#'
#' @description
#' * **`-->>` step 1 -- [`pulse_read()`] `<<--`**
#' * `step 2` -- [`pulse_split()`]
#' * `step 3` -- [`pulse_optimize()`]
#' * `step 4` -- [`pulse_heart()`]
#' * `step 5` -- [`pulse_doublecheck()`]
#'
#' Importing data from PULSE `'.csv'` files is the first step of the analysis of PULSE data.
#'
#' `pulse_read()` checks that the paths provided by the user conform to certain expectations and then reads the data from all files and merges into a single tibble. Only data from the same experiment should be read at the same time (i.e., with the same channel names, collected with the same sampling frequency, and produced using a PULSE multi-channel or a PULSE one-channel system running the same firmware version throughout the experiment). To put it differently, one call to `pulse_read()` can only read files where the header is absolutely invariant, and only the data portion of the files differs. The output of `pulse_read()` can be directly passed on to `pulse_split()`.
#'
#' @param paths File paths to CSV files produced by a PULSE system during a single experiment.
#' @param with_progress One of `TRUE`, `FALSE` or `NULL` (default) to choose whether to show progress bars or not (based on the `progressr` package). `TRUE` prints a `cli`-style progress bar; `FALSE` disables progress bars altogether; if set to `NULL`, the behavior is controlled by the user from outside this function (by setting the desired `handlers()`; in addition, setting `handlers(global = TRUE)` ensures the same behavior is used across the entire session).
#' @param msg A logical to decide if non-crucial messages (but not errors) are shown (defaults to `TRUE`; mainly for use from within the wrapper function `PULSE()`, where it is set to `FALSE` to avoid the repetition of identical messages)
#'
#' @section Time zones:
#' PULSE systems **ALWAYS** record data using **UTC +0**. This is intentional! If data were to be recorded using local time zones, issues with daylight saving time, leap seconds, etc. could spoil the dataset. Worse, should the information about which time zone had been used get lost or accidentally modified, the validity of the entire dataset could be compromised. By always using UTC +0 all these issues are minimized and the processing pipeline becomes vastly easier and more efficient. Still, this means that after the data has been processed using the [`heartbeatr-package`], the user may need to adjust the time zone of all timestamps so that the timing matches other information relative to the experiment.
#'
#' @seealso
#'  * check [progressr::handlers()] to customize the reporting of progress
#'  * [pulse_split()], [pulse_optimize()], [pulse_heart()] and [pulse_doublecheck()] are the other functions needed for the complete PULSE processing workflow
#'  * [PULSE()] is a wrapper function that executes all the steps needed to process PULSE data at once
#'
#' @return
#' A list with two elements:
#' * `$data`, A tibble containing all data from all PULSE files
#' * `$multi`, A logical indicating if the data is from a multi-channel system (TRUE) or from a one-channel system (FALSE)
#' * `$vrsn`, A numeric value with the version number of the PULSE system where the data was generated
#' * `$freq`, A numeric value with the sampling frequency used (in Hz)
#'
#' @export
#'
#' @examples
#' ## Begin prepare data ----
#' paths <- pulse_example("RAW_original_")
#' ## End prepare data ----
#'
#' pulse_read(paths, with_progress = TRUE)
pulse_read <- function(paths, with_progress = NULL, msg = TRUE) {
  if (length(paths) == 1) if (file.info(paths)$isdir) paths <- dir(paths, full.names = TRUE) %>% tolower() %>% stringr::str_subset(".csv$")

  checks <- pulse_read_checks(paths)
  if (!checks$ok) {
    stop(checks$msg)
  } else {
    if (msg) message(checks$msg)
  }

  cols  <- checks$out$cols
  freq  <- checks$out$freq
  skip  <- checks$out$skip
  vrsn  <- checks$out$vrsn
  multi <- checks$multi

  # set progress reporting strategy
  if (!is.null(with_progress)) {
    if (with_progress) {
      old_handlers <- progressr::handlers("cli")
    } else {
      old_handlers <- progressr::handlers("void")
    }
    on.exit(if (is.null(old_handlers)) progressr::handlers("void") else progressr::handlers(old_handlers), add = TRUE)
  }

  # read PULSE files
  bar <- progressr::progressor(along = paths)
  pulse_data <- list()
  for (i in seq_along(paths)) {
    pulse_data[[i]] <- pulse_read_data(paths[i], skip = skip, cols = cols, multi = multi)
    bar(message = "reading PULSE files |")
  }

  # merge
  pulse_data <- pulse_data %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(time)

  # return
  list(data = pulse_data, multi = multi, vrsn = vrsn, freq = freq)
}
