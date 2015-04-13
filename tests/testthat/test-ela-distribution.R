context("Features: Distribution")

test_that("Expected Output", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the levelset features
  features = calculateFeatureSet(feat.object, "distribution")
  
  # test return values
  expect_equal(length(features), 5L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), 
    c(rep(c("numeric", "integer"), each = 2), "numeric"))
  
  expect_true( testNumber(features$y_dist.skewness) )
  expect_true( testNumber(features$y_dist.kurtosis) )
  expect_true( testNumber(features$y_dist.number_of_peaks, lower = 1) )
  expect_that( as.integer(features$y_dist.number_of_peaks), 
    equals(features$y_dist.number_of_peaks) )
  expect_identical(features$y_dist.costs_fun_evals, 0L)
  expect_true( testNumber(features$y_dist.costs_runtime, lower = 0) )
  
})