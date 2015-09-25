context("Features: CM - Convexity")

test_that("Require Cell Mapping", {
  set.seed(2015*03*26)

  # (1) Create a Feature Object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2))

  expect_error(calculateFeatureSet(feat.object, "cm_conv"))
})

test_that("Number of blocks has to be >2 in at least 1 dimension.", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2), blocks = 2L)

  expect_error(calculateFeatureSet(feat.object, "cm_conv"))
})

test_that("Number of blocks has to be >2 in all dimensions.", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2),
    blocks = c(2L, 3L, 3L, 4L, 2L))

  expect_error(calculateFeatureSet(feat.object, "cm_conv"))
})

test_that("Calculation of Cell Convexity is possible", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2),
    blocks = c(3L, 3L, 4L, 4L, 3L))

  # (2) compute the cell convexity features
  features = calculateFeatureSet(feat.object, "cm_conv")

  # test return value types and ranges
  expect_identical(length(features), 6L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 4L), "integer", "numeric"))
  expect_true(testNumber(features$cm_conv.convex.hard, lower = 0L, upper = 1L))
  expect_true(testNumber(features$cm_conv.convex.soft, lower = 0L, upper = 1L))
  expect_true(testNumber(features$cm_conv.concave.hard, lower = 0L, upper = 1L))
  expect_true(testNumber(features$cm_conv.concave.soft, lower = 0L, upper = 1L))
  expect_identical(features$cm_conv.costs_fun_evals, 0L)
  expect_true(testNumber(features$cm_conv.costs_runtime, lower = 0L))

  expect_true(features$cm_conv.convex.hard <= features$cm_conv.convex.soft)
  expect_true(features$cm_conv.concave.hard <= features$cm_conv.concave.soft)
})

test_that("Using Minkowski Distance", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2),
    blocks = c(3L, 3L, 4L, 4L, 3L))

  # (2) compute the cell convexity features
  features = calculateFeatureSet(feat.object, "cm_conv",
    control = list(cm_conv.dist_method = "minkowski"))
  features1 = calculateFeatureSet(feat.object, "cm_conv")  

  # test return value types and ranges
  expect_list(features)
  expect_list(features1)

  expect_identical(features[-6L], features1[-6L])
})

test_that("Using Manhattan Distance", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2),
    blocks = c(3L, 3L, 4L, 4L, 3L))

  # (2) compute the nearest better features
  features = calculateFeatureSet(feat.object, "cm_conv",
    control = list(cm_conv.dist_method = "manhattan"))

  # test return value types and ranges
  expect_identical(length(features), 6L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), 
    c(rep("numeric", 4L), "integer", "numeric"))

  expect_true(testNumber(features$cm_conv.convex.hard, lower = 0L, upper = 1L))
  expect_true(testNumber(features$cm_conv.convex.soft, lower = 0L, upper = 1L))
  expect_true(testNumber(features$cm_conv.concave.hard, lower = 0L, upper = 1L))
  expect_true(testNumber(features$cm_conv.concave.soft, lower = 0L, upper = 1L))
  expect_identical(features$cm_conv.costs_fun_evals, 0L)
  expect_true(testNumber(features$cm_conv.costs_runtime, lower = 0L))

  expect_true(features$cm_conv.convex.hard <= features$cm_conv.convex.soft)
  expect_true(features$cm_conv.concave.hard <= features$cm_conv.concave.soft)
})
