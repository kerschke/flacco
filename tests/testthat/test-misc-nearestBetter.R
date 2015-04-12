context("Features: Nearest Better")

test_that("Calculation of Nearest Better is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1L, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the nearest better features
  features = calculateNearestBetterFeatures(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 7L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 5L), "integer", "numeric"))
  
  expect_true( testNumber(features$nb.nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features$nb.nn_nb.mean_ratio) )
  expect_true( testNumber(features$nb.nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features$nb.dist_ratio.coeff_var) )
  expect_true( testNumber(features$nb.nb_fitness.cor, lower = -1, upper = 1) )
  expect_identical(features$nb.costs_fun_evals, 0L)
  expect_true( testNumber(features$nb.costs_runtime, lower = 0) )
  
})


test_that("Calculation of Nearest Better based on Minkowski Distance", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1L, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the nearest better features
  features = calculateNearestBetterFeatures(feat.object,
    control = list(nbf.dist_method = "minkowski"))
  features1 = calculateNearestBetterFeatures(feat.object, 
    control = list(nbf.dist_method = "minkowski", nbf.minkowski_p = 10L))
  features2 = calculateNearestBetterFeatures(feat.object)
  
  # test return value types and ranges
  expect_is(features, "list")
  expect_is(features1, "list")
  expect_is(features2, "list")
  
  expect_identical(features[-7L], features2[-7L])
  expect_false(identical(features[-7L], features1[-7L]))
  expect_false(identical(features2[-7L], features1[-7L]))
})