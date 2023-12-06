load_all()
check()
document()

# install.packages("roxygen2")

usethis::use_build_ignore("dev_code.R")
use_git()
use_mit_license()
use_readme_rmd()

use_testthat()
use_test("function")

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

# use_git_config(user.name = "ruiseabra", user.email = "ruisea@gmail.com")
# use_git_config(user.name = "coastalwarming", user.email = "coastalwarming@gmail.com")
# usethis::create_github_token()
# gitcreds::gitcreds_set()

# echo "# heartbeatr" >> README.md
# git init
# git add README.md
# git commit -m "first commit"
# git branch -M main
# git remote add origin git@github.com:coastalwarming/heartbeatr.git
# git push -u origin main

# ssh-keygen
# Generating public/private rsa key pair.
# Enter file in which to save the key (/Users/ruiseabra/.ssh/id_rsa):
# 	Enter passphrase (empty for no passphrase):
# 	Enter same passphrase again:
# 	Your identification has been saved in /Users/ruiseabra/.ssh/id_rsa
# Your public key has been saved in /Users/ruiseabra/.ssh/id_rsa.pub

# cat /Users/ruiseabra/.ssh/id_rsa
# cat /Users/ruiseabra/.ssh/id_rsa.pub
