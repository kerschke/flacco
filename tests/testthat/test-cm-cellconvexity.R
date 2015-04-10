context("calculateCellConvexity")

test_that("A cell mapping-enabled FeatureObject is required", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  expect_error(calculateCellConvexity(feat.object))
})

test_that("Number of blocks has to be >2 in at least 1 dimension.", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y, blocks = 2)
  
  expect_error(calculateCellConvexity(feat.object))
})

test_that("Calculation of Cell Convexity is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y, blocks = c(2, 3, 3, 4, 2))
  
  # (2) compute the nearest better features
  features = calculateCellConvexity(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 4)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), rep("numeric", 4))
  expect_true( testNumber(features$convex.hard, lower = 0, upper = 1) )
  expect_true( testNumber(features$convex.soft, lower = 0, upper = 1) )
  expect_true( testNumber(features$concave.hard, lower = 0, upper = 1) )
  expect_true( testNumber(features$concave.soft, lower = 0, upper = 1) )
  
  expect_true( features$convex.hard <= features$convex.soft )
  expect_true( features$concave.hard <= features$concave.soft )
})


test_that("Calculation of Nearest Better based on Minkowski Distance", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the nearest better features
  features = calculateNearestBetter(feat.object,
    control = list(nbf.dist_method = "minkowski"))
  features1 = calculateNearestBetter(feat.object, 
    control = list(nbf.dist_method = "minkowski", nbf.minkowski_p = 10))
  features2 = calculateNearestBetter(feat.object)
  
  # test return value types and ranges
  expect_is(features, "list")
  expect_is(features1, "list")
  expect_is(features2, "list")
  
  expect_identical(features, features2)
  expect_false(identical(features, features1))
  expect_false(identical(features2, features1))
})