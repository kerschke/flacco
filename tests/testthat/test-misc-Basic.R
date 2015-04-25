context("Features: Basic")

test_that("Non-Cellmapping Objects", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the Basic features
  features = calculateFeatureSet(feat.object, "basic")
  
  # test return value types and ranges
  expect_equal(length(features), 16L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)),
    c("integer", "integer", rep("numeric", 6L), rep("integer", 4L),
      rep("logical", 2L), "integer", "numeric"))
  expect_identical(features$basic.dim, 5L)
  expect_identical(features$basic.observations, 2000L)
  expect_true(features$basic.lower.min >= -10)
  expect_true(features$basic.lower.max >= -10)
  expect_true(features$basic.lower.min <= features$basic.lower.max)
  expect_true(features$basic.upper.min <= 10)
  expect_true(features$basic.upper.max <= 10)
  expect_true(features$basic.upper.min <= features$basic.upper.max)
  expect_true(features$basic.lower.min <= features$basic.upper.min)
  expect_true(features$basic.lower.max <= features$basic.upper.max)
  
  expect_identical(features$basic.blocks.min, 1L)
  expect_identical(features$basic.blocks.max, 1L)
  
  expect_identical(features$basic.cells.total, 1L)
  expect_identical(features$basic.cells.filled, 1L)
  
  expect_identical(features$basic.allows_cm, FALSE)
  expect_identical(features$basic.minimize_fun, TRUE)
  
  expect_identical(features$basic.costs_fun_evals, 0L)
  expect_true( testNumber(features$basic.costs_runtime, lower = 0) )
  
})


test_that("Cellmapping Objects", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y, blocks = c(2, 3, 4, 3, 5))
  
  # (2) compute the Basic features
  features = calculateFeatureSet(feat.object, "basic")
  
  # test return value types and ranges
  expect_equal(length(features), 16L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)),
    c("integer", "integer", rep("numeric", 6L), rep("integer", 4L),
      rep("logical", 2L), "integer", "numeric"))
  expect_identical(features$basic.dim, 5L)
  expect_identical(features$basic.observations, 2000L)
  expect_true(features$basic.lower.min >= -10)
  expect_true(features$basic.lower.max >= -10)
  expect_true(features$basic.lower.min <= features$basic.lower.max)
  expect_true(features$basic.upper.min <= 10)
  expect_true(features$basic.upper.max <= 10)
  expect_true(features$basic.upper.min <= features$basic.upper.max)
  expect_true(features$basic.lower.min <= features$basic.upper.min)
  expect_true(features$basic.lower.max <= features$basic.upper.max)
  
  expect_identical(features$basic.blocks.min, 2L)
  expect_identical(features$basic.blocks.max, 5L)
  
  expect_identical(features$basic.cells.total, as.integer(2 * 3 * 4 * 3 * 5))
  expect_true( testNumber(features$basic.cells.filled, 
    lower = 1, upper = features$basic.cells.total) )
})


test_that("Test Basic Features", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5L, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object1 = createFeatureObject(X = X, y = y)
  feat.object2 = createFeatureObject(X = X, y = y, minimize = FALSE)
  feat.object3 = createFeatureObject(X = X, y = y, blocks = 3L)
  feat.object4 = createFeatureObject(X = X, y = y, blocks = 3L, minimize = FALSE)
  
  # (2) compute the Basic features
  features1 = calculateFeatureSet(feat.object1, "basic")
  features2 = calculateFeatureSet(feat.object2, "basic")
  features3 = calculateFeatureSet(feat.object3, "basic")
  features4 = calculateFeatureSet(feat.object4, "basic")
  
  # test return value types and ranges
  expect_equal(length(features1), 16L)
  expect_equal(length(features2), 16L)
  expect_equal(length(features3), 16L)
  expect_equal(length(features4), 16L)
  expect_is(features1, class = "list")
  expect_is(features2, class = "list")
  expect_is(features3, class = "list")
  expect_is(features4, class = "list")
  expect_equal(as.character(sapply(features1, class)),
    c("integer", "integer", rep("numeric", 6L), rep("integer", 4L),
      rep("logical", 2L), "integer", "numeric"))
  expect_equal(as.character(sapply(features2, class)),
    c("integer", "integer", rep("numeric", 6L), rep("integer", 4L),
      rep("logical", 2L), "integer", "numeric"))
  expect_equal(as.character(sapply(features3, class)),
    c("integer", "integer", rep("numeric", 6L), rep("integer", 4L),
      rep("logical", 2L), "integer", "numeric"))
  expect_equal(as.character(sapply(features4, class)),
    c("integer", "integer", rep("numeric", 6L), rep("integer", 4L),
      rep("logical", 2L), "integer", "numeric"))
  expect_true(features1$basic.minimize_fun)
  expect_true(!features1$basic.allows_cm)
  expect_true(!features2$basic.minimize_fun)
  expect_true(!features2$basic.allows_cm)
  expect_true(features3$basic.minimize_fun)
  expect_true(features3$basic.allows_cm)
  expect_true(!features4$basic.minimize_fun)
  expect_true(features4$basic.allows_cm)
})
