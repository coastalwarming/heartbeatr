#' PULSE multi-channel example data
#'
#' A subset of data from an experiment monitoring the heartbeat of four 'Patella vulgata' limpets (channels S5-S10 were not monitored)
#'
#' @format ##
#' A list with two elements:
#'  * `$data`, a tibble with 29,978 rows and 11 columns - one column with timestamps (named `time`) and several columns of numeric data (the voltage readings from each channel of the PULSE multi-channel system; all channels with unique names)
#'  * `$freq`, a single integer value representing the sampling frequency used (in Hz)
#' \describe{
#'   \item{time}{timestamp}
#'   \item{limpet_1-limpet_4}{voltage readings}
#'   \item{S5-S10}{unused channels}
#'   ...
#' }
"pulse_data"
