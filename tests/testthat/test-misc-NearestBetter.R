context("Features: MISC - Nearest Better")

test_that("Expected Output", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1L, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the nearest better features
  features = calculateFeatureSet(feat.object, "nbc")
  
  # test return value types and ranges
  expect_identical(length(features), 7L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), c(rep("numeric", 5L), "integer", "numeric"))
  
  expect_true( testNumber(features$nbc.nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features$nbc.nn_nb.mean_ratio) )
  expect_true( testNumber(features$nbc.nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features$nbc.dist_ratio.coeff_var) )
  expect_true( testNumber(features$nbc.nb_fitness.cor, lower = -1, upper = 1) )
  expect_identical(features$nbc.costs_fun_evals, 0L)
  expect_true( testNumber(features$nbc.costs_runtime, lower = 0) )
})

test_that("Using Minkowski Distance", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1L, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)

  # (2) compute the nearest better features
  features = calculateFeatureSet(feat.object, "nbc",
    control = list(nbc.dist_method = "minkowski"))
  features1 = calculateFeatureSet(feat.object, "nbc",
    control = list(nbc.dist_method = "minkowski", nbc.minkowski_p = 10L))
  features2 = calculateFeatureSet(feat.object, "nbc")

  # test return value types and ranges
  expect_list(features)
  expect_list(features1)
  expect_list(features2)

  expect_identical(features[-7L], features2[-7L])
  expect_false(identical(features[-7L], features1[-7L]))
  expect_false(identical(features2[-7L], features1[-7L]))
})

test_that("Using Manhattan Distance", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1L, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)

  # (2) compute the nearest better features
  features = calculateFeatureSet(feat.object, "nbc",
    control = list(nbc.dist_method = "manhattan"))

  # test return value types and ranges
  expect_identical(length(features), 7L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), 
    c(rep("numeric", 5L), "integer", "numeric"))

  expect_true( testNumber(features$nbc.nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features$nbc.nn_nb.mean_ratio) )
  expect_true( testNumber(features$nbc.nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features$nbc.dist_ratio.coeff_var) )
  expect_true( testNumber(features$nbc.nb_fitness.cor, lower = -1, upper = 1) )
  expect_identical(features$nbc.costs_fun_evals, 0L)
  expect_true( testNumber(features$nbc.costs_runtime, lower = 0) )
})

test_that("Multiple Global Optima", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1L, function(x) sum(x^2))

  y1 = y2 = y
  y1[sample(seq_along(y), 2)] = min(y) - 10
  y2[sample(seq_along(y), 3)] = min(y) - 10

  feat.object1 = createFeatureObject(X = X, y = y)
  feat.object2 = createFeatureObject(X = X, y = y1)
  feat.object3 = createFeatureObject(X = X, y = y2)

  # (2) compute the nearest better features
  features1 = calculateFeatureSet(feat.object1, "nbc")
  features2 = calculateFeatureSet(feat.object2, "nbc")
  features3 = calculateFeatureSet(feat.object3, "nbc")

  # test return value types and ranges
  expect_equal(length(features1), 7L)
  expect_list(features1)
  expect_identical(as.character(sapply(features1, class)), c(rep("numeric", 5L), "integer", "numeric"))  
  expect_true( testNumber(features1$nbc.nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features1$nbc.nn_nb.mean_ratio) )
  expect_true( testNumber(features1$nbc.nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features1$nbc.dist_ratio.coeff_var) )
  expect_true( testNumber(features1$nbc.nb_fitness.cor, lower = -1, upper = 1) )
  expect_identical(features1$nbc.costs_fun_evals, 0L)
  expect_true( testNumber(features1$nbc.costs_runtime, lower = 0) )

  expect_equal(length(features2), 7L)
  expect_list(features2)
  expect_identical(as.character(sapply(features2, class)), c(rep("numeric", 5L), "integer", "numeric"))  
  expect_true( testNumber(features2$nbc.nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features2$nbc.nn_nb.mean_ratio) )
  expect_true( testNumber(features2$nbc.nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features2$nbc.dist_ratio.coeff_var) )
  expect_true( testNumber(features2$nbc.nb_fitness.cor, lower = -1, upper = 1) )
  expect_identical(features2$nbc.costs_fun_evals, 0L)
  expect_true( testNumber(features2$nbc.costs_runtime, lower = 0) )

  expect_equal(length(features3), 7L)
  expect_list(features3)
  expect_identical(as.character(sapply(features3, class)), c(rep("numeric", 5L), "integer", "numeric"))  
  expect_true( testNumber(features3$nbc.nn_nb.sd_ratio, lower = 0) )
  expect_true( testNumber(features3$nbc.nn_nb.mean_ratio) )
  expect_true( testNumber(features3$nbc.nn_nb.cor, lower = -1, upper = 1) )
  expect_true( testNumber(features3$nbc.dist_ratio.coeff_var) )
  expect_true( testNumber(features3$nbc.nb_fitness.cor, lower = -1, upper = 1) )
  expect_identical(features3$nbc.costs_fun_evals, 0L)
  expect_true( testNumber(features3$nbc.costs_runtime, lower = 0) )
})
