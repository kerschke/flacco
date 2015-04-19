context("Features: Angle")

test_that("Require Cell Mapping", {
  feat.object = createFeatureObject(init = iris[, -5], objective = "Sepal.Length")
  expect_error(calculateFeatureSet(feat.object, "angle"))
})

test_that("Using an Initial Design", {
  feat.object = createFeatureObject(init = iris[, -5], 
    objective = "Sepal.Length", blocks = 3)
  angle.feats = suppressWarnings(calculateFeatureSet(feat.object, "angle"))
  expect_equal(length(angle.feats), 10L)
  expect_is(angle.feats, class = "list")
  expect_equal(as.character(sapply(angle.feats, class)), 
    c(rep("numeric", 8), "integer", "numeric"))
})

test_that("Using X and y", {
  A = rbind(c(0, 0), c(0, 3), c(4, 3))
  feat.object = createFeatureObject(X = A, y = 1:3, blocks = 1)
  angle.feats = suppressWarnings(calculateFeatureSet(feat.object, "angle"))
  expect_identical(angle.feats$cm_angle.dist_ctr2best.sd, NA_real_)
  expect_identical(angle.feats$cm_angle.dist_ctr2worst.sd, NA_real_)
  expect_identical(angle.feats$cm_angle.angle.sd, NA_real_)
  expect_identical(angle.feats$cm_angle.y_ratio_best2worst.sd, NA_real_)
  expect_identical(angle.feats$cm_angle.dist_ctr2best.mean, sqrt(sum((c(2, 1.5) - c(0, 0))^2)))
  expect_identical(angle.feats$cm_angle.dist_ctr2worst.mean, sqrt(sum((c(2, 1.5) - c(4, 3))^2)))
  expect_identical(angle.feats$cm_angle.angle.mean, 180)
  expect_identical(angle.feats$cm_angle.y_ratio_best2worst.mean, 1)
  expect_identical(angle.feats$cm_angle.costs_fun_evals, 0L)
  expect_true( testNumber(angle.feats$cm_angle.costs_runtime, lower = 0) )
})