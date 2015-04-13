library(testthat)

# test features:
test_check("flacco", filter = "^ela")
test_check("flacco", filter = "^cm")
# test_check("flacco", filter = "^gcm") # not needed since it's already included in cm
test_check("flacco", filter = "^ic")
test_check("flacco", filter = "^misc")