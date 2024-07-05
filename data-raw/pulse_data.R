## code to prepare `pulse_data` dataset goes here

pulse_data <- pulse_read(pulse_example("RAW_or"))

usethis::use_data(pulse_data, overwrite = TRUE)
