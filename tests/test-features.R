library(testthat)

# test features:
test_check("flacco", filter = "^cm")
test_check("flacco", filter = "^ela")
test_check("flacco", filter = "^gcm")
test_check("flacco", filter = "^ic")
test_check("flacco", filter = "^misc")
test_check("flacco", filter = "calculateFeatures")
