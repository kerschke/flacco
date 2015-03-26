library(testthat)
test_check("flacco", filter = "^base")
test_check("flacco", filter = "^ela")
test_check("flacco", filter = "^cm")
test_check("flacco", filter = "^gcm")
test_check("flacco", filter = "^ic")
# test_check("flacco", filter = "^misc") # travis complains about this as long as no tests match the filter