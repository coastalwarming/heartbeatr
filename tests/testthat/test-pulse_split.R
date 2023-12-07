# success ----
test_that("returns the correct structure: a list with one or more elements", {
	x1 <- pulse_data %>% pulse_split(30, 60, 0.8, FALSE, FALSE)
	x2 <- list(
		data = pulse_data$data %>% dplyr::select(1:5),
		freq = pulse_data$freq
	) %>%
		pulse_split(30, 60, 0.8, FALSE, FALSE)

	# one or more elements
	expect_true(length(x1) >= 1)
	# all with 11 columns
	expect_true(all(purrr::map_dbl(x1, ncol) == 11))
	# all with 5 columns (after dropping unsused channels)
	expect_true(all(purrr::map_dbl(x2, ncol) == 5))
	# 1st column always named 'time'
	expect_true(all(purrr::map_chr(x1, ~colnames(.x)[1]) == "time"))
	# 1st column always of class POSIXct
	expect_equal(purrr::map(x1, ~class(.x$time)) %>% unlist() %>% unique(), c("POSIXct", "POSIXt"))
	# all other columns of class numeric
	expect_true(all(purrr::map_chr(x1, ~class(.x[,-1] %>% unlist())) == "numeric"))
	# each window (element of list) has the right number of entries (90-110 %)
	expect_true(all(dplyr:: between(purrr::map_dbl(x1, nrow) / (x1$freq * 30), 0.9, 1.1)))
})

test_that("window_width_secs doesn't affect the number of windows", {
	x1 <- pulse_split(pulse_data, 15, 60, 0.8, FALSE, FALSE)
	x2 <- pulse_split(pulse_data, 30, 60, 0.8, FALSE, FALSE)
	x3 <- pulse_split(pulse_data, 60, 60, 0.8, FALSE, FALSE)

	expect_equal(length(x1), length(x2))
	expect_equal(length(x1), length(x3))
})

test_that("window_width_secs affects the width of windows", {
	x1 <- pulse_split(pulse_data, 15, 60, 0.8, FALSE, FALSE)[[1]]
	x2 <- pulse_split(pulse_data, 30, 60, 0.8, FALSE, FALSE)[[1]]
	x3 <- pulse_split(pulse_data, 60, 60, 0.8, FALSE, FALSE)[[1]]

	expect_true(nrow(x1) < nrow(x2))
	expect_true(nrow(x2) < nrow(x3))
})

test_that("window_shift_secs affects the number of windows", {
	x1 <- pulse_split(pulse_data, 30, 15, 0.8, FALSE, FALSE)
	x2 <- pulse_split(pulse_data, 30, 30, 0.8, FALSE, FALSE)
	x3 <- pulse_split(pulse_data, 30, 60, 0.8, FALSE, FALSE)

	expect_true(length(x1) > length(x2))
	expect_true(length(x2) > length(x3))
})

test_that("window_shift_secs doesn't affect the width of windows", {
	x1 <- pulse_split(pulse_data, 30, 15, 0.8, FALSE, FALSE)[[1]]
	x2 <- pulse_split(pulse_data, 30, 30, 0.8, FALSE, FALSE)[[1]]
	x3 <- pulse_split(pulse_data, 30, 60, 0.8, FALSE, FALSE)[[1]]

	expect_equal(nrow(x1), nrow(x2))
	expect_equal(nrow(x2), nrow(x3))
})

test_that("min_data_points can affect the number of windows", {
	x1 <- pulse_split(pulse_data, 30, 60, 0.200, FALSE, FALSE)
	x2 <- pulse_split(pulse_data, 30, 60, 0.999, FALSE, FALSE)

	expect_true(length(x1) > length(x2))
})
