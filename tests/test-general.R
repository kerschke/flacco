library(testthat)

# test general stuff:
test_check("flacco", filter = "^base")
test_check("flacco", filter = "^plot")
