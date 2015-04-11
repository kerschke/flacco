context("calculateGradientHomogeneity")

test_that("A cell mapping-enabled FeatureObject is required", {
  feat.object = createFeatureObject(init = iris[, -5], objective = "Sepal.Length")
  expect_error(calculateGradientHomogeneity(feat.object))
})
  
test_that("GradientHomogeneity Features are calculated (differing blocks)", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = c(10, 5, 5, 8, 4))
  
  features = calculateGradientHomogeneity(feat.object, show.warnings = FALSE)
  
  expect_is(features, "list")
  expect_true( testNumber(features$gradhomo.mean, lower = -1, upper = 1) )
  expect_true( testNumber(features$gradhomo.sd) )
})

test_that("GradientHomogeneity Features are calculated (identical blocks)", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = 5)
  
  features = calculateGradientHomogeneity(feat.object, show.warnings = FALSE)
  
  expect_equal(length(features), 4L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 2), "integer", "numeric"))
  
  expect_true( testNumber(features$gradhomo.mean, lower = -1, upper = 1) )
  expect_true( testNumber(features$gradhomo.sd) )
  expect_identical(features$gradhomo.costs_fun_evals, 0L)
  expect_true( testNumber(features$gradhomo.costs_runtime, lower = 0) )
  
})

test_that("A warning is issued if cells are populated sparsely", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = c(10, 5, 5, 8, 4))

  expect_warning(calculateGradientHomogeneity(feat.object), 
    "% of the cells contain less than two observations.")
})
