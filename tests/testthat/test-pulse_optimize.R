# success ----
test_that("returns same structure as the input", {
	pulse_data_split <- pulse_split(
		pulse_data,
		window_width_secs = 30,
		window_shift_secs = 60,
		min_data_points = 0.8,
		with_progress = FALSE,
		msg = FALSE)

	p1 <- pulse_data_split$data[[1]]
	p2 <- pulse_interpolate(p1, interpolation_freq = 40)

	expect_equal(class(p1), class(p2))
	expect_equal(ncol(p1), ncol(p2))
	expect_equal(colnames(p1), colnames(p2))
	expect_equal(class(p1$time), class(p2$time))
	expect_equal(class(p1 %>% dplyr::pull(2)), class(p2 %>% dplyr::pull(2)))
})

test_that("higher interpolation_freq leads to more rows of data", {
	pulse_data_split <- pulse_split(
		pulse_data,
		window_width_secs = 30,
		window_shift_secs = 60,
		min_data_points = 0.8,
		with_progress = FALSE,
		msg = FALSE)

	expect_true(
		nrow(pulse_interpolate(pulse_data_split$data[[1]], interpolation_freq = 40)) <
			nrow(pulse_interpolate(pulse_data_split$data[[1]], interpolation_freq = 50))
	)
})

# failure ----

test_that("fails when interpolation_freq too low", {
	pulse_data_split <- pulse_split(
		pulse_data,
		window_width_secs = 30,
		window_shift_secs = 60,
		min_data_points = 0.8,
		with_progress = FALSE,
		msg = FALSE)

	expect_error(pulse_interpolate(pulse_data_split$data[[1]], interpolation_freq = 39))
	expect_no_error(pulse_interpolate(pulse_data_split$data[[1]], interpolation_freq = 40))
})

