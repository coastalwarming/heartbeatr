# success ----
test_that("returns the correct structure: a list with one or more elements", {
	window_width_secs = 30
	x1 <- pulse_split(pulse_data, window_width_secs = window_width_secs, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x2 <- list(
		data = pulse_data$data %>% dplyr::select(1:5),
		freq = pulse_data$freq
	) %>%
		pulse_split(window_width_secs = window_width_secs, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)

	# one or more elements
	expect_true(nrow(x1) >= 1)
	# all with 11 columns
	expect_true(all(purrr::map_dbl(x1$data, ncol) == 11))
	# all with 5 columns (after dropping unsused channels)
	expect_true(all(purrr::map_dbl(x2$data, ncol) == 5))
	# 1st column always named 'time'
	expect_true(all(purrr::map_chr(x1$data, ~colnames(.x)[1]) == "time"))
	# 1st column always of class POSIXct
	expect_equal(purrr::map(x1$data, ~class(.x$time)) %>% unlist() %>% unique(), c("POSIXct", "POSIXt"))
	# all other columns of class numeric
	expect_true(all(purrr::map_chr(x1$data, ~class(.x[,-1] %>% unlist())) == "numeric"))
	# each window (element of list) has the right number of entries (95-105 %)
	expect_true(all(dplyr::between(purrr::map_dbl(x1$data, nrow) / (pulse_data$freq * window_width_secs), 0.95, 1.05)))
})

test_that("window_width_secs doesn't impact the number of windows", {
	x1 <- pulse_split(pulse_data, window_width_secs = 15, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x2 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x3 <- pulse_split(pulse_data, window_width_secs = 60, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)

	expect_equal(nrow(x1), nrow(x2))
	expect_equal(nrow(x1), nrow(x3))
})

test_that("window_width_secs impacts the width of windows", {
	x1 <- pulse_split(pulse_data, window_width_secs = 15, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x2 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x3 <- pulse_split(pulse_data, window_width_secs = 60, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)

	expect_true(nrow(x1$data[[1]]) < nrow(x2$data[[1]]))
	expect_true(nrow(x2$data[[1]]) < nrow(x3$data[[1]]))
})

test_that("window_shift_secs impacts the number of windows", {
	x1 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 15, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x2 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 30, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x3 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)

	expect_true(nrow(x1) > nrow(x2))
	expect_true(nrow(x2) > nrow(x3))
})

test_that("window_shift_secs doesn't impact the width of windows", {
	x1 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 15, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x2 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 30, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)
	x3 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 60, min_data_points = 0.8, with_progress = FALSE, msg = FALSE)

	expect_equal(nrow(x1$data[[1]]), nrow(x2$data[[1]]))
	expect_equal(nrow(x2$data[[1]]), nrow(x3$data[[1]]))
})

test_that("min_data_points can impact the number of windows", {
	x1 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 60, min_data_points = 0.200, with_progress = FALSE, msg = FALSE)
	x2 <- pulse_split(pulse_data, window_width_secs = 30, window_shift_secs = 60, min_data_points = 0.999, with_progress = FALSE, msg = FALSE)

	expect_true(nrow(x1) > nrow(x2))
})
