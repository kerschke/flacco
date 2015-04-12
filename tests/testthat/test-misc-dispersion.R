context("Features: Dispersion")

test_that("calculateDispersionFeatures -- first use case", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the dispersion features
  features = calculateDispersionFeatures(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 18L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 16L), "integer", "numeric"))
  expect_true( testNumber(features$disp.ratio_mean_02) )
  expect_true( testNumber(features$disp.ratio_mean_05) )
  expect_true( testNumber(features$disp.ratio_mean_10) )
  expect_true( testNumber(features$disp.ratio_mean_25) )
  expect_true( testNumber(features$disp.ratio_median_02) )
  expect_true( testNumber(features$disp.ratio_median_05) )
  expect_true( testNumber(features$disp.ratio_median_10) )
  expect_true( testNumber(features$disp.ratio_median_25) )
  expect_true( testNumber(features$disp.diff_mean_02) )
  expect_true( testNumber(features$disp.diff_mean_05) )
  expect_true( testNumber(features$disp.diff_mean_10) )
  expect_true( testNumber(features$disp.diff_mean_25) )
  expect_true( testNumber(features$disp.diff_median_02) )
  expect_true( testNumber(features$disp.diff_median_05) )
  expect_true( testNumber(features$disp.diff_median_10) )
  expect_true( testNumber(features$disp.diff_median_25) )
  expect_identical(features$disp.costs_fun_evals, 0L)
  expect_true( testNumber(features$disp.costs_runtime, lower = 0) )
})


test_that("Calculation Dispersion based on Different Metrics", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the dispersion features
  features = calculateDispersionFeatures(feat.object,
    control = list(disp.dist_method = "minkowski"))
  features1 = calculateDispersionFeatures(feat.object, 
    control = list(disp.dist_method = "manhattan"))
  features2 = calculateDispersionFeatures(feat.object)
  
  # test return value types and ranges
  expect_is(features, "list")
  expect_is(features1, "list")
  expect_is(features2, "list")
  
  expect_equal(features[-18], features2[-18])
  expect_false(identical(features[-18], features1[-18]))
  expect_false(identical(features2[-18], features1[-18]))
})