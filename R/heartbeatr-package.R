#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end
NULL

ignore_unused_imports <- function() {
  cli::cli
}

# load_all()

# paths <- fs::path_package("inst", extdata", "limpets", package = "pulse") %>% dir(full.names = TRUE)
# paths <- fs::path_package("inst", "extdata", "limpets", package = "pulse") %>% dir(full.names = TRUE, pattern = ".csv", ignore.case = TRUE)
# paths <- fs::path_package("inst", "extdata", "limpets", package = "pulse") %>% dir(full.names = TRUE, pattern = ".csv", ignore.case = TRUE) %>% stringr::str_subset("Pulse123", negate = TRUE)

# read_pulse(paths[c(7,8)])

# data <- read_pulse(folder)
# save(data, fs::path_package("inst", "extdata", "limpets", package = "pulse"))
