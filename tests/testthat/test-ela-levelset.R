context("calculateLevelset")

test_that("Calculation of Levelset is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the levelset features
  features = calculateLevelset(feat.object)
  
  # test return values
  expect_equal(length(features), 20L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 18L), "integer", "numeric"))
  
  expect_true( testNumber(features$lvlset.mmce_lda_10, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.mmce_qda_10, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.mmce_mda_10, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.lda_qda_10, lower = 0) )
  expect_true( testNumber(features$lvlset.lda_mda_10, lower = 0) )
  expect_true( testNumber(features$lvlset.qda_mda_10, lower = 0) )
  expect_true( testNumber(features$lvlset.mmce_lda_25, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.mmce_qda_25, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.mmce_mda_25, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.lda_qda_25, lower = 0) )
  expect_true( testNumber(features$lvlset.lda_mda_25, lower = 0) )
  expect_true( testNumber(features$lvlset.qda_mda_25, lower = 0) )
  expect_true( testNumber(features$lvlset.mmce_lda_50, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.mmce_qda_50, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.mmce_mda_50, lower = 0, upper = 1) )
  expect_true( testNumber(features$lvlset.lda_qda_50, lower = 0) )
  expect_true( testNumber(features$lvlset.lda_mda_50, lower = 0) )
  expect_true( testNumber(features$lvlset.qda_mda_50, lower = 0) )
  expect_identical(features$lvlset.costs_fun_evals, 0L)
  expect_true( testNumber(features$lvlset.costs_runtime, lower = 0) )
  
})