context("Features: ELA - Levelset")

checkLevelOutput = function(features) {
  # test return values
  expect_identical(length(features), 20L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 18L), "integer", "numeric"))

  expect_true(testNumber(features$ela_level.mmce_lda_10, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.mmce_qda_10, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.mmce_mda_10, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.lda_qda_10, lower = 0L))
  expect_true(testNumber(features$ela_level.lda_mda_10, lower = 0L))
  expect_true(testNumber(features$ela_level.qda_mda_10, lower = 0L))
  expect_true(testNumber(features$ela_level.mmce_lda_25, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.mmce_qda_25, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.mmce_mda_25, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.lda_qda_25, lower = 0L))
  expect_true(testNumber(features$ela_level.lda_mda_25, lower = 0L))
  expect_true(testNumber(features$ela_level.qda_mda_25, lower = 0L))
  expect_true(testNumber(features$ela_level.mmce_lda_50, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.mmce_qda_50, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.mmce_mda_50, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_level.lda_qda_50, lower = 0L))
  expect_true(testNumber(features$ela_level.lda_mda_50, lower = 0L))
  expect_true(testNumber(features$ela_level.qda_mda_50, lower = 0L))
  expect_identical(features$ela_level.costs_fun_evals, 0L)
  expect_true(testNumber(features$ela_level.costs_runtime, lower = 0L))
}

test_that("Expected Output", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute the levelset features
  features = calculateFeatureSet(feat.object, "ela_level")
})

test_that("Expect Warning", {
  feat.object = createFeatureObject(init = iris[,-5], objective = "Petal.Width")
  expect_warning(calculateFeatureSet(feat.object, "ela_level",
    control = list(ela_level.quantiles = 0.05, ela_level.classif_methods = "lda"))
  )
})


test_that("Parallelization", {
  X = replicate(5, runif(2000))
  f = smoof::makeBBOBFunction(dimension = 5, fid = 20, iid = 1)
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y)

  feats = calculateFeatureSet(feat.object, "ela_level",
    control = list(ela_level.parallelize = TRUE))
  checkLevelOutput(feats)
})
