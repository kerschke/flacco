context("calculateGradientHomogeneity")

test_that("A cell mapping-enabled FeatureObject is required", {
  featobj = createFeatureObject(init = iris[, -5], objective = "Sepal.Length")
  expect_error(calculateGradientHomogeneity(featobj))
})
  
test_that("GradientHomogeneity Features are calculated", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  featobj = createFeatureObject(X = X, y = y, blocks = c(10, 5, 5, 8, 4))
  
  features = calculateGradientHomogeneity(featobj, show.warnings = FALSE)
  
  expect_is(features, "list")
  expect_true( testNumber(features$gradhomo.mean, lower=-1, upper=1) )
  expect_true( testNumber(features$gradhomo.sd) )
})

test_that("A warning is issued if cells are populated sparsely", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) { sum(x^2) })
  featobj = createFeatureObject(X = X, y = y, blocks = c(10, 5, 5, 8, 4))
  
  expect_warning(calculateGradientHomogeneity(featobj), 
                 "% of the cells contain less than two observations.")
})

