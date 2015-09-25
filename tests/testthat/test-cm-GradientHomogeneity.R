context("Features: CM - Gradient Homogeneity")

test_that("Differing blocks", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2),
    blocks = c(10L, 5L, 5L, 8L, 4L))

  features = calculateFeatureSet(feat.object, "cm_grad", 
    control = list(cm_grad.show_warnings = FALSE))

  expect_list(features)
  expect_true(testNumber(features$cm_grad.mean, lower = -1L, upper = 1L))
  expect_true(testNumber(features$cm_grad.sd))
})

test_that("Identical blocks", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2), blocks = 5L)

  features = calculateFeatureSet(feat.object, "cm_grad", 
    control = list(cm_grad.show_warnings = FALSE))

  expect_identical(length(features), 4L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), 
    c(rep("numeric", 2L), "integer", "numeric"))

  expect_true(testNumber(features$cm_grad.mean, lower = -1L, upper = 1L))
  expect_true(testNumber(features$cm_grad.sd))
  expect_identical(features$cm_grad.costs_fun_evals, 0L)
  expect_true(testNumber(features$cm_grad.costs_runtime, lower = 0L))
})

test_that("Only one block", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2), blocks = 1L)

  features = calculateFeatureSet(feat.object, "cm_grad", 
    control = list(cm_grad.show_warnings = FALSE))

  expect_identical(length(features), 4L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), 
    c(rep("numeric", 2L), "integer", "numeric"))

  expect_true(testNumber(features$cm_grad.mean, lower = -1L, upper = 1L))
  expect_identical(features$cm_grad.sd , NA_real_)
  expect_identical(features$cm_grad.costs_fun_evals, 0L)
  expect_true(testNumber(features$cm_grad.costs_runtime, lower = 0L))
})

test_that("Warning if cells are populated sparsely", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2),
    blocks = c(10L, 5L, 5L, 8L, 4L))

  expect_warning(calculateFeatureSet(feat.object, "cm_grad",
    control = list(cm_grad.show_warnings = TRUE)), 
    "% of the cells contain less than two observations.")

  expect_warning(calculateFeatureSet(feat.object, "cm_grad",
    control = list(cm_grad.show_warnings = TRUE, cm_grad.dist_method = "manhattan")), 
    "% of the cells contain less than two observations.")
})

test_that("Using Minkowski Distance", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2), blocks = 5L)

  # (2) compute the cell convexity features
  features = calculateFeatureSet(feat.object, "cm_grad",
    control = list(cm_grad.dist_method = "minkowski"))
  features1 = calculateFeatureSet(feat.object, "cm_grad",
    control = list(cm_grad.dist_method = "minkowski", cm_grad.minkowski_p = 10L))
  features2 = calculateFeatureSet(feat.object, "cm_grad")

  # test return value types and ranges
  expect_list(features)
  expect_list(features1)
  expect_list(features2)

  expect_identical(features[-4L], features2[-4L])
  expect_false(identical(features[-4L], features1[-4L]))
  expect_false(identical(features2[-4L], features1[-4L]))
})

test_that("Using Manhattan Distance", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2), blocks = 5L)

  # (2) compute the cell convexity features
  features = calculateFeatureSet(feat.object, "cm_grad",
    control = list(cm_grad.dist_method = "manhattan"))

  # test return value types and ranges
  expect_identical(length(features), 4L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 2L), "integer", "numeric"))

  expect_true(testNumber(features$cm_grad.mean, lower = -1L, upper = 1L))
  expect_true(testNumber(features$cm_grad.sd))
  expect_identical(features$cm_grad.costs_fun_evals, 0L)
  expect_true(testNumber(features$cm_grad.costs_runtime, lower = 0L))
})
