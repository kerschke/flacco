# FIXME: R CMD check does not run on true batch systems, but maybe it does not have to
# we can check there with 'make test' and run 'make check' locally

library(testthat)
# test_check("ELA")

#FIXME: bad hack
# for some reason using test_package and opening a socked node
# blocks R CMD check
# but we really want to test at least one real parallel mode on cran

# if (!interactive()) {
#   library(BBmisc)
#   library(ELA)
#   source("testthat/helpers.R")
#   source("testthat/helper_sockettest.R")
#   sockettest()
# }
