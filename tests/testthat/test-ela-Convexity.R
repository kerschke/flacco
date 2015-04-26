context("Features: Convexity")

test_that("Require original function", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the convexity features
  expect_error(calculateFeatureSet(feat.object, "convexity"))
})

test_that("Expected Output", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "convexity")
  
  # test return value types
  expect_equal(length(features), 6L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 4), "integer", "numeric"))
  
  # test return values and ranges
  expect_true( testNumber(features$conv.conv_prob, lower = 0, upper = 1) )
  expect_true( testNumber(features$conv.lin_prob, lower = 0, upper = 1) )
  expect_true( testNumber(features$conv.lin_dev.orig) )
  expect_true( testNumber(features$conv.lin_dev.abs, lower = 0) )
  expect_true( features$conv.lin_dev.abs >= abs(features$conv.lin_dev.orig) )
  
  expect_true( testNumber(features$conv.costs_fun_evals, lower = 0L) )
  expect_true( testNumber(features$conv.costs_runtime, lower = 0) )
})
