#creating horseEvo package
library(usethis)
library(roxygen2)
library(devtools)

#check package name availability
library(available)
available("horseEvo")


setwd("~/GitHubRepos")
usethis::create_package("horseEvo")
setwd("~/horseEvo")
usethis::use_git()
usethis::use_readme_md()
usethis::use_data_raw()
# move this file to data-raw
# move all original function files to R dir
# write function descriptions. Then:
roxygen2::roxygenise() #this creates the man folder
# write down gloal function definitions inside functions.
# write some tests: