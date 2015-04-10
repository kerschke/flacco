context("calculateNearestBetter")

test_that("Calculation of Nearest Better is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the nearest better features
  features = calculateNearestBetter(feat.object)
  
  # test return value types and ranges
  expect_is(features, "list")
  expect_true( testNumber(features$nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features$nn_nb.mean_ratio) )
  expect_true( testNumber(features$nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features$dist_ratio.coeff_var) )
  expect_true( testNumber(features$nb_fitness.cor, lower = -1, upper = 1) )
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