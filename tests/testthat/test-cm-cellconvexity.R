context("Features: Cell Convexity")

test_that("A cell mapping-enabled FeatureObject is required", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  expect_error(calculateCellConvexityFeatures(feat.object))
})

test_that("Number of blocks has to be >2 in at least 1 dimension.", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y, blocks = 2)
  
  expect_error(calculateCellConvexityFeatures(feat.object))
})

test_that("Calculation of Cell Convexity is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y, blocks = c(2, 3, 3, 4, 2))
  
  # (2) compute the cell convexity features
  features = calculateCellConvexityFeatures(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 6L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 4L), "integer", "numeric"))
  expect_true( testNumber(features$cm_conv.convex.hard, lower = 0, upper = 1) )
  expect_true( testNumber(features$cm_conv.convex.soft, lower = 0, upper = 1) )
  expect_true( testNumber(features$cm_conv.concave.hard, lower = 0, upper = 1) )
  expect_true( testNumber(features$cm_conv.concave.soft, lower = 0, upper = 1) )
  expect_identical(features$cm_conv.costs_fun_evals, 0L)
  expect_true( testNumber(features$cm_conv.costs_runtime, lower = 0) )
  
  expect_true( features$cm_conv.convex.hard <= features$cm_conv.convex.soft )
  expect_true( features$cm_conv.concave.hard <= features$cm_conv.concave.soft )
})
