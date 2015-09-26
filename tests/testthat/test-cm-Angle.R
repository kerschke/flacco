context("Features: CM - Angle")

test_that("Using an Initial Design", {
  feat.object = createFeatureObject(init = iris[, -5], 
    objective = "Sepal.Length", blocks = 3L)
  angle_feats = calculateFeatureSet(feat.object, "cm_angle")
  expect_identical(length(angle_feats), 10L)
  expect_list(angle_feats)
  expect_identical(as.character(sapply(angle_feats, class)), 
    c(rep("numeric", 8L), "integer", "numeric"))
})

test_that("Using X and y", {
  A = rbind(c(0L, 0L), c(0L, 3L), c(4L, 3L))
  feat.object = createFeatureObject(X = A, y = 1:3, blocks = 1L)
  angle_feats = calculateFeatureSet(feat.object, "cm_angle")
  expect_identical(angle_feats$cm_angle.dist_ctr2best.sd, NA_real_)
  expect_identical(angle_feats$cm_angle.dist_ctr2worst.sd, NA_real_)
  expect_identical(angle_feats$cm_angle.angle.sd, NA_real_)
  expect_identical(angle_feats$cm_angle.y_ratio_best2worst.sd, NA_real_)
  expect_identical(angle_feats$cm_angle.dist_ctr2best.mean, sqrt(sum((c(2, 1.5) - c(0, 0))^2)))
  expect_identical(angle_feats$cm_angle.dist_ctr2worst.mean, sqrt(sum((c(2, 1.5) - c(4, 3))^2)))
  expect_identical(angle_feats$cm_angle.angle.mean, 180)
  expect_identical(angle_feats$cm_angle.y_ratio_best2worst.mean, 1)
  expect_identical(angle_feats$cm_angle.costs_fun_evals, 0L)
  expect_true(testNumber(angle_feats$cm_angle.costs_runtime, lower = 0L))
})

test_that("Only one block", {
  set.seed(2015*03*26)
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  y = apply(X, 1, function(x) { sum(x^2) })
  feat.object = createFeatureObject(X = X, y = y, blocks = 1L)

  angle_feats = calculateFeatureSet(feat.object, "cm_angle", 
    control = list(angle.show_warnings = FALSE))

  expect_identical(length(angle_feats), 10L)
  expect_list(angle_feats)
  expect_identical(as.character(sapply(angle_feats, class)), 
    c(rep("numeric", 8L), "integer", "numeric"))

  expect_true(testNumber(angle_feats$cm_angle.dist_ctr2best.mean, lower = 0L))
  expect_identical(angle_feats$cm_angle.dist_ctr2best.sd, NA_real_)
  expect_true(testNumber(angle_feats$cm_angle.dist_ctr2worst.mean, lower = 0L))
  expect_identical(angle_feats$cm_angle.dist_ctr2worst.sd, NA_real_)
  expect_true(testNumber(angle_feats$cm_angle.angle.mean, lower = -180L,
    upper = 180L))
  expect_identical(angle_feats$cm_angle.angle.sd, NA_real_)
  expect_true(testNumber(angle_feats$cm_angle.y_ratio_best2worst.mean,
    lower = 0L, upper = 1L))
  expect_identical(angle_feats$cm_angle.y_ratio_best2worst.sd, NA_real_)
  expect_identical(angle_feats$cm_angle.costs_fun_evals, 0L)
  expect_true(testNumber(angle_feats$cm_angle.costs_runtime, lower = 0L))
})

test_that("Show Warning", {
  feat.object = createFeatureObject(init = iris[, -5], 
    objective = "Sepal.Length", blocks = 3L)
  expect_warning(calculateFeatureSet(feat.object, "cm_angle",
    control = list(cm_angle.show_warnings = TRUE)))
})