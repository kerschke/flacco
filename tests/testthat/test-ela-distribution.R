context("calculateDistribution")

test_that("Calculation of y-Distribution is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the levelset features
  features = calculateDistribution(feat.object)
  
  # test return values
  expect_is(features, "list")
  expect_true( testNumber(features$y_dist.skewness) )
  expect_true( testNumber(features$y_dist.kurtosis) )
  expect_true( testNumber(features$y_dist.number_of_peaks, lower = 1) )
  expect_that( as.integer(features$y_dist.number_of_peaks), 
    equals(features$y_dist.number_of_peaks) )
})