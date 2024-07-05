#' PULSE multi-channel example data
#'
#' A subset of data from an experiment monitoring the heartbeat of four *Patella vulgata* limpets (channels S5-S10 were not monitored)
#'
#' @format ##
#' A list with two elements:
#'  * `$data`, a tibble with 29,978 rows and 11 columns - one column with timestamps (named `time`) and several columns of numeric data (the voltage readings from each channel of the PULSE multi-channel system; all channels with unique names)
#'  * `$freq`, a single integer value representing the sampling frequency used (in Hz)
"pulse_data"
