context("Features: Gradient Homogeneity")

test_that("Require Cell Mapping", {
  feat.object = createFeatureObject(init = iris[, -5], objective = "Sepal.Length")
  expect_error(calculateFeatureSet(feat.object, "gradient_homogeneity"))
})
  
test_that("Differing blocks", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = c(10, 5, 5, 8, 4))
  
  features = calculateFeatureSet(feat.object, "gradient_homogeneity", 
    control = list(gradhomo.show_warnings = FALSE))
  
  expect_is(features, "list")
  expect_true( testNumber(features$gradhomo.mean, lower = -1, upper = 1) )
  expect_true( testNumber(features$gradhomo.sd) )
})

test_that("Identical blocks", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = 5)
  
  features = calculateFeatureSet(feat.object, "gradient_homogeneity", 
    control = list(gradhomo.show_warnings = FALSE))
  
  expect_equal(length(features), 4L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 2), "integer", "numeric"))
  
  expect_true( testNumber(features$gradhomo.mean, lower = -1, upper = 1) )
  expect_true( testNumber(features$gradhomo.sd) )
  expect_identical(features$gradhomo.costs_fun_evals, 0L)
  expect_true( testNumber(features$gradhomo.costs_runtime, lower = 0) )
})

test_that("Warning if cells are populated sparsely", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = c(10, 5, 5, 8, 4))

  expect_warning(calculateFeatureSet(feat.object, "gradient_homogeneity",
    control = list(gradhomo.show_warnings = TRUE)), 
    "% of the cells contain less than two observations.")
})

test_that("Using Minkowski Distance", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = 5)
  
  # (2) compute the cell convexity features
  features = calculateFeatureSet(feat.object, "gradient_homogeneity",
    control = list(gradhomo.dist_method = "minkowski"))
  features1 = calculateFeatureSet(feat.object, "gradient_homogeneity",
    control = list(gradhomo.dist_method = "minkowski", gradhomo.minkowski_p = 10L))
  features2 = calculateFeatureSet(feat.object, "gradient_homogeneity")
  
  # test return value types and ranges
  expect_is(features, "list")
  expect_is(features1, "list")
  expect_is(features2, "list")
  
  expect_identical(features[-4L], features2[-4L])
  expect_false(identical(features[-4L], features1[-4L]))
  expect_false(identical(features2[-4L], features1[-4L]))
})


test_that("Using Manhattan Distance", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = 5)
  
  # (2) compute the cell convexity features
  features = calculateFeatureSet(feat.object, "gradient_homogeneity",
    control = list(gradhomo.dist_method = "manhattan"))
  
  # test return value types and ranges
  expect_equal(length(features), 4L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 2), "integer", "numeric"))
  
  expect_true( testNumber(features$gradhomo.mean, lower = -1, upper = 1) )
  expect_true( testNumber(features$gradhomo.sd) )
  expect_identical(features$gradhomo.costs_fun_evals, 0L)
  expect_true( testNumber(features$gradhomo.costs_runtime, lower = 0) )
})