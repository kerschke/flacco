context("calculateDispersion")

test_that("calculateDispersion -- first use case", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the dispersion features
  features = calculateDispersion(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 16)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), rep("numeric", 16))
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
})
