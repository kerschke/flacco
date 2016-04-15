library(testthat)

# test general stuff:
test_check("flacco", filter = "^base")

if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
  test_check("flacco", filter = "^plot")
}