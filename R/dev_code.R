if (FALSE) {
	### basic dev cycle
	load_all()
	document()
	check()
	install()

	# install.packages("roxygen2")

	### declare files to be ignored during build
	use_build_ignore("dev_code.R")
	use_build_ignore("data-raw")
	# use_git_ignore("dev_code.R")

	### declare intent to use GITHUB
	use_git()

	### set package licence
	use_mit_license()

	### declare use of RMARKDOWN for help files
	use_roxygen_md()

	### generate a readme file for GITHUB
	use_readme_rmd()

	### declare use of tests
	use_testthat()
	use_test("pulse_read")
	use_test("pulse_split")
	use_test("pulse_optimize")

	### declare dependencies
	use_package("cli")
	use_package("dplyr")
	use_package("future")
	use_package("future.apply")
	use_package("ggplot2")
	use_package("lubridate")
	use_package("magrittr")
	use_package("progressr")
	use_package("purrr")
	use_package("readr")
	use_package("stringr")
	use_package("tibble")
	use_package("tidyr")

	### create and edit a new R file
	use_r("pulse_optimize")

	### GENERATE EXAMPLE DATA
	# run code in "data-raw/pulse_data.R"

	### GENERATE EXAMPLE DATA (OUTDATED)
	# paths <- fs::path_package("inst", extdata", "limpets", package = "heartbeatr") %>% dir(full.names = TRUE)
	# paths <- fs::path_package("inst", "extdata", "limpets", package = "heartbeatr") %>% dir(full.names = TRUE, pattern = ".csv", ignore.case = TRUE)
	# paths <- fs::path_package("inst", "extdata", "limpets", package = "heartbeatr") %>% dir(full.names = TRUE, pattern = ".csv", ignore.case = TRUE) %>% stringr::str_subset("Pulse123", negate = TRUE)
	# pulse_read(paths[c(7,8)])
	# data <- pulse_read(folder)
	# save(data, fs::path_package("inst", "extdata", "limpets", package = "heartbeatr"))

	### CONFIGURE GITHUB USER ON RSTUDIO (R)
	# use_git_config(user.name = "ruiseabra", user.email = "ruisea@gmail.com")
	# use_git_config(user.name = "coastalwarming", user.email = "coastalwarming@gmail.com")
	# usethis::create_github_token()
	# gitcreds::gitcreds_set()

	### LINK REPOSITORY RSTUDIO <--> GITHUB (TERMINAL)
	# echo "# heartbeatr" >> README.md
	# git init
	# git add README.md
	# git commit -m "first commit"
	# git branch -M main
	# git remote add origin git@github.com:coastalwarming/heartbeatr.git
	# git push -u origin main

	### GENERATE SSH KEY (TERMINAL)
	# ssh-keygen
	#> Generating public/private rsa key pair.
	#> Enter file in which to save the key (/Users/ruiseabra/.ssh/id_rsa):
	#>	Enter passphrase (empty for no passphrase): <-- can leave empty
	#>	Enter same passphrase again: <-- can leave empty
	#>	Your identification has been saved in /Users/ruiseabra/.ssh/id_rsa
	#> Your public key has been saved in /Users/ruiseabra/.ssh/id_rsa.pub
	# cat /Users/ruiseabra/.ssh/id_rsa
	# cat /Users/ruiseabra/.ssh/id_rsa.pub <-- copy this one and paste on GITHUB
}
