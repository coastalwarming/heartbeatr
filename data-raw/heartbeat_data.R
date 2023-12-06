## code to prepare `pulse_data` dataset goes here

pulse_data <- "RAW_or" %>%
  pulse_example() %>%
  pulse_read()

usethis::use_data(pulse_data, overwrite = TRUE)
