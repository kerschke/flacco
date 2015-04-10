context("calculateConvexity")

test_that("Calculation of Convexity requires the original function", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the convexity features
  expect_error( calculateConvexity(feat.object) )
  
})

test_that("Calculation of Convexity is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the meta model features
  features = calculateConvexity(feat.object)
  
  # test return value types
  expect_is(features, "list")
  expect_true( testNumber(features$conv.conv_prob) )
  expect_true( testNumber(features$conv.lin_prob) )
  expect_true( testNumber(features$conv.lin_dev) )
  
  # test return values and ranges
  expect_true( testNumber(features$conv.conv_prob, lower=0, upper=1) )
  expect_true( testNumber(features$conv.lin_prob, lower=0, upper=1) )
})