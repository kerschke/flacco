context("Features: ELA - Convexity")

test_that("Require original function", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2))

  # (2) compute the convexity features
  expect_error(calculateFeatureSet(feat.object, "ela_conv"))
})

test_that("Expected Output", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "ela_conv")

  # test return value types
  expect_identical(length(features), 6L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), 
    c(rep("numeric", 4L), "integer", "numeric"))

  # test return values and ranges
  expect_true(testNumber(features$ela_conv.conv_prob, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_conv.lin_prob, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_conv.lin_dev.orig))
  expect_true(testNumber(features$ela_conv.lin_dev.abs, lower = 0L))
  expect_true(features$ela_conv.lin_dev.abs >= abs(features$ela_conv.lin_dev.orig))

  expect_true(testNumber(features$ela_conv.costs_fun_evals, lower = 0L))
  expect_true(testNumber(features$ela_conv.costs_runtime, lower = 0L))
})
