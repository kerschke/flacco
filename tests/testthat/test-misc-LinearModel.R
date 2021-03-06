context("Features: MISC - Linear Model")

test_that("Sparse Cells lead to NAs", {
  set.seed(2015*03*26)
  X = replicate(5, runif(100))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = 7L)
  features = calculateFeatureSet(feat.object, "limo")
  expect_identical(unique(unlist(features[1:12])), NA_real_)
})

test_that("Using Initial Design", {
  feat.object = createFeatureObject(init = iris[, -5], objective = "Sepal.Length", blocks = 3)
  features = calculateFeatureSet(feat.object, "limo")
  expect_equal(length(features), 14L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)), c(rep("numeric", 12L), "integer", "numeric"))
  expect_true( testNumber(features$limo.avg_length.reg, lower = 0) )
  expect_true( testNumber(features$limo.avg_length.norm, lower = 0, upper = 1) )
  expect_true( testNumber(features$limo.length.mean, lower = 0) )
  expect_true( testNumber(features$limo.length.sd, lower = 0) )
  expect_true( testNumber(features$limo.cor.reg, lower = -1, upper = 1) )
  expect_true( testNumber(features$limo.cor.norm, lower = -1, upper = 1) )
  expect_true( testNumber(features$limo.ratio.mean) )
  expect_true( testNumber(features$limo.ratio.sd, lower = 0) )
  expect_true( testNumber(features$limo.sd_ratio.reg, lower = 1) )
  expect_true( testNumber(features$limo.sd_ratio.norm, lower = 1) )
  expect_true( testNumber(features$limo.sd_mean.reg, lower = 0) )
  expect_true( testNumber(features$limo.sd_mean.norm, lower = 0) )
  expect_identical(features$limo.costs_fun_evals, 0L)
  expect_true( testNumber(features$limo.costs_runtime, lower = 0) )
})

test_that("Using X and y", {
  A = rbind(c(0, 0), c(0, 3), c(4, 3))
  feat.object = createFeatureObject(X = A, y = 1:3, blocks = 1)
  features = calculateFeatureSet(feat.object, "limo")
  expect_identical(features$limo.length.sd, NA_real_)
  expect_identical(features$limo.cor.reg, NA_real_)
  expect_identical(features$limo.cor.norm, NA_real_)
  expect_identical(features$limo.ratio.sd, NA_real_)
  expect_identical(features$limo.sd_ratio.reg, NA_real_)
  expect_identical(features$limo.sd_ratio.norm, NA_real_)
  expect_identical(features$limo.sd_mean.reg, NA_real_)
  expect_identical(features$limo.sd_mean.norm, NA_real_)
})

test_that("Show Error", {
  feat.object = createFeatureObject(init = iris[, -5], 
    objective = "Sepal.Length", blocks = 3L)
  expect_error(calculateFeatureSet(feat.object, "limo",
    control = list(allow_cellmapping = FALSE)))
})
