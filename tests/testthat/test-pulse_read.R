# success ----
test_that("returns the correct structure: 2 elements with correct names", {
	paths <- testthat::test_path(
		"fixtures",
		c("RAW_original_20221229_1350.CSV", "RAW_original_20221229_1400.CSV"))
	x <- pulse_read(paths, msg = FALSE)

	# has 2 elements
	expect_true(length(x) == 2)
	# with the expected names
	expect_equal(names(x), c("data", "freq"))
})

test_that("returns the correct structure: freq", {
	paths <- testthat::test_path(
		"fixtures",
		c("RAW_original_20221229_1350.CSV", "RAW_original_20221229_1400.CSV"))
	x <- pulse_read(paths, msg = FALSE)

	# is numeric
	expect_true(is.numeric(x$freq))
	# is only one value
	expect_true(length(x$freq) == 1)
})

test_that("returns the correct structure: data", {
	paths <- testthat::test_path(
		"fixtures",
		c("RAW_original_20221229_1350.CSV", "RAW_original_20221229_1400.CSV"))
	x <- pulse_read(paths, msg = FALSE)

	# has 11 columns
	expect_true(ncol(x$data) == 11)
	# 1st column named 'time'
	expect_true(colnames(x$data)[1] == "time")
	# 1st column of class POSIXct
	expect_equal(class(x$data$time), c("POSIXct", "POSIXt"))
	# all other columns of class numeric
	expect_true(all(class(x$data[,-1] %>% unlist()) == "numeric"))
	# same as the stored 'pulse_data' object
	expect_equal(x$data, pulse_data$data)
})

test_that("handles incomplete lines", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_incomplete_line.CSV", "RAW_original_20221229_1400.CSV"))
	paths_ok    <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_short.CSV", "RAW_original_20221229_1400.CSV"))

	data_issue <- pulse_read(paths_issue, msg = FALSE)$data
	data_ok    <- pulse_read(paths_ok, msg = FALSE)$data

	expect_equal(nrow(data_ok), nrow(data_issue) + 1)
	expect_equal(data_ok[-2,], data_issue)
})

# failure ----
test_that("fails with different sampling rates", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_rate_hz.CSV", "RAW_original_20221229_1400.CSV"))

	read_confirmation <- pulse_read(paths_issue[2], msg = FALSE)
	expect_error(pulse_read(paths_issue, msg = FALSE))
})

test_that("fails with different header nrow", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_header_nrow.CSV", "RAW_original_20221229_1400.CSV"))

	expect_error(pulse_read(paths_issue, msg = FALSE))
})

test_that("fails with different PULSE version", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_version.CSV", "RAW_original_20221229_1400.CSV"))

	expect_error(pulse_read(paths_issue, msg = FALSE))
})

test_that("fails with colnames different among files", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_cols.CSV", "RAW_original_20221229_1400.CSV"))

	expect_error(pulse_read(paths_issue, msg = FALSE))
})

test_that("fails when colnames are not unique", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_20221229_1350_cols_not_unique.CSV"))

	expect_error(pulse_read(paths_issue, msg = FALSE))
})

test_that("stops if 'paths' includes non-PULSE multi-channel files", {
	paths_issue <- testthat::test_path(
		"fixtures",
		c("RAW_not_a_pulse_file.txt", "RAW_20221229_1350_short.CSV"))

	expect_error(pulse_read(paths_issue, msg = FALSE))
})
