
context("socket mode")

#FIXME: I do NOT know why this blocks in R CMD check
if (interactive()) {

# cran allows socket mode with 2 localhost processes
test_that("socket mode", {
  sockettest()
})

}
