library(methods)
library(devtools)
library(testthat)
library(BBmisc)
library(mlr)
library(numDeriv)
library(expm)
library(plyr)

load_all(".")

args = commandArgs()
file = args[which(args == "--args") + 1L]
if (length(file) == 0 || is.na(file)) {
  tests = test_dir("tests/testthat", filter = "")
} else {
  catf("Run test for file %s", file.path("tests", "testthat", file))
  tests = test_file(file.path("tests", "testthat", file))
}
tests$group = sapply(strsplit(tests$file, "-"), function(x) x[2])
groups = ddply(tests, "group", summarise,
  failed = sum(failed), error = sum(error), time = sum(real))
save2(file = "testinfo.RData", tests, groups)
