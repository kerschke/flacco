context("Features: ELA - Distribution")

test_that("Expected Output", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute the levelset features
  features = calculateFeatureSet(feat.object, "ela_distr")

  # test return values
  expect_equal(length(features), 5L)
  expect_list(features)
  expect_equal(as.character(sapply(features, class)), 
    c(rep(c("numeric", "integer"), each = 2L), "numeric"))

  expect_true(testNumber(features$ela_distr.skewness))
  expect_true(testNumber(features$ela_distr.kurtosis))
  expect_true(testNumber(features$ela_distr.number_of_peaks, lower = 1L))
  expect_that(as.integer(features$ela_distr.number_of_peaks), 
    equals(features$ela_distr.number_of_peaks))
  expect_identical(features$ela_distr.costs_fun_evals, 0L)
  expect_true(testNumber(features$ela_distr.costs_runtime, lower = 0L))
})
