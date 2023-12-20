
<!-- README.md is generated from README.Rmd. Please edit that file -->

# heartbeatr

<!-- badges: start -->
<!-- badges: end -->

An R package for processing data and automatically assessing cardiac
frequency, specifically designed for use with PULSE systems
(www.electricblue.eu/pulse).

## Installation

You can install the development version of **heartbeatr** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("coastalwarming/heartbeatr")
```

## Examples

List PULSE files to be read:

``` r
library(heartbeatr)
# but make sure they correspond to a single experiment/device
# ...here we use the package's example data
paths <- pulse_example("RAW_original_")
paths
#> [1] "/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/heartbeatr/extdata/RAW_original_20221229_1350.CSV"
#> [2] "/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/heartbeatr/extdata/RAW_original_20221229_1400.CSV"
```

There are two ways to read and process those data:

``` r
# step by step
pulse_data_sub   <- pulse_read(paths, msg = FALSE)
pulse_data_split <- pulse_split(
   pulse_data_sub,
   window_width_secs = 30,
   window_shift_secs = 60,
   min_data_points = 0.8, 
   msg = FALSE
   )
pulse_data_split <- pulse_optimize(pulse_data_split, target_freq = 40, bandwidth = 0.2)
heart_rates <- pulse_heart(pulse_data_split, msg = FALSE)

# or calling a single wrapper function
heart_rates <- PULSE(
  paths,
  discard_channels  = paste0("s", 5:10), # channels s5 to s10 are empty in the example data
  window_width_secs = 30,
  window_shift_secs = 60,
  min_data_points   = 0.8,
  target_freq = 40,
  bandwidth   = 0.2,
  msg = FALSE
  )
```

Once processed, PULSE data is stored as a tibble with an average heart
rate frequency for each channel/split window. The time is relative to
the mid-point of the window. Frequencies are expressed in Hz and have
the number of identified beats and standard deviations associated, which
can be used to classify or filter the data (see below).

``` r
heart_rates
#> # A tibble: 95 × 6
#>    id       time                data                     n    hz    sd
#>    <fct>    <dttm>              <list>               <int> <dbl> <dbl>
#>  1 limpet_1 2022-12-29 13:51:15 <tibble [1,200 × 3]>    24 0.789 0.032
#>  2 limpet_1 2022-12-29 13:52:15 <tibble [1,200 × 3]>    23 0.779 0.012
#>  3 limpet_1 2022-12-29 13:53:15 <tibble [1,200 × 3]>    24 0.781 0.022
#>  4 limpet_1 2022-12-29 13:54:15 <tibble [1,200 × 3]>    25 0.811 0.059
#>  5 limpet_1 2022-12-29 13:55:15 <tibble [1,200 × 3]>    25 0.817 0.011
#>  6 limpet_1 2022-12-29 13:56:15 <tibble [1,200 × 3]>    24 0.81  0.122
#>  7 limpet_1 2022-12-29 13:57:15 <tibble [1,200 × 3]>    26 0.848 0.05 
#>  8 limpet_1 2022-12-29 13:58:15 <tibble [1,200 × 3]>    26 0.834 0.022
#>  9 limpet_1 2022-12-29 13:59:15 <tibble [1,200 × 3]>    27 0.869 0.052
#> 10 limpet_1 2022-12-29 14:00:15 <tibble [1,200 × 3]>    25 0.845 0.027
#> # ℹ 85 more rows
```

You can easily use parallel computing with **heartbeatr** - just
configure your R session properly **BEFORE** applying the PULSE
workflow:

``` r
# this shows how your session is currently configured 
#   (typically defaults to "sequential", i.e., not parallelized)
future::plan()
#> sequential:
#> - args: function (..., envir = parent.frame())
#> - tweaked: FALSE
#> - call: NULL

# to make use of parallel computing (highly recommended)
future::plan("multisession")
future::plan()
#> multisession:
#> - args: function (..., workers = availableCores(), lazy = FALSE, rscript_libs = .libPaths(), envir = parent.frame())
#> - tweaked: FALSE
#> - call: future::plan("multisession")
```

The raw data underlying the heart rate frequency estimate (hz) can be
inspected:

``` r
# the 5th split window for channel "limpet_1" (the target --> i = 5) is shown in the center
# - 2 more windows are shown before and after the target (hence, range =2)
# - red dots show where the algorithm detected a peak
pulse_plot_raw(heart_rates, ID = "limpet_1", i = 5, range = 2) 
```

<img src="man/figures/README-plot1-1.png" width="100%" />

A quick overview of the result of the analysis:

``` r
pulse_plot_all(heart_rates)
```

<img src="man/figures/README-plot2-1.png" width="100%" />

The number of data points can be reduced:

``` r
heart_rates_binned <- pulse_summarise(heart_rates, fun = mean, span_mins = 3, min_data_points = 0.8)
pulse_plot_all(heart_rates_binned)
```

<img src="man/figures/README-plot3-1.png" width="100%" />

A more detailed view of channel 1, named “limpet_1”

``` r
# selecting channel 1
HR_limpet1 <- dplyr::filter(heart_rates, id=="limpet_1")

# gathering heart beat data
BPM_limpet1 <- cbind(HR_limpet1$hz*60, HR_limpet1$hz*60 - HR_limpet1$sd*1.96*60, HR_limpet1$hz*60 + HR_limpet1$sd*1.96*60)
colnames(BPM_limpet1) <- c("BPM", "CI_lower", "CI_upper")

# plot the time series for channel 1 with heart frequency expressed as beats per minute (BPM) +/- 95% confidence intervals
suppressMessages(library(xts))
xts_limpet1 <- xts(BPM_limpet1, HR_limpet1$time)
plot(xts_limpet1, lty=c(1,2,2), col=c(1,2,2), lwd=c(2,1,1), ylim=c(25,85), main = "Limpet 1", ylab = "Beats per minute")
```

<img src="man/figures/README-select one channel and plot-1.png" width="100%" />

Removing data points exceeding an arbitrary standard deviation. This is
useful when dealing with long series of continuous data which were
already collected with the intention to discard portions of data with
higher variability which may be indicative of lower quality.

``` r
# arbritary threshold
max_sd <- 0.04

# make a copy of the object and delete high-variability points
thinned_xts_limpet1 <- xts_limpet1
thinned_xts_limpet1[HR_limpet1$sd >= max_sd,] <- NA

#plot
plot(thinned_xts_limpet1$BPM, lty=1, col=1, lwd=2, ylim=c(25,85), main = "Limpet 1", ylab = "Beats per minute")
points(thinned_xts_limpet1$BPM)
```

<img src="man/figures/README-thinning data and plotting-1.png" width="100%" />
