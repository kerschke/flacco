context("calculateAngle")

test_that("calculateAngle", {
  featobj = createFeatureObject(init = iris[, -5], objective = "Sepal.Length")
  expect_error(calculateAngle(featobj))
  
  featobj = createFeatureObject(init = iris[, -5], objective = "Sepal.Length", blocks = 3)
  angle.feats = suppressWarnings(calculateAngle(featobj))
  expect_equal(length(angle.feats), 8)
  expect_is(angle.feats, class = "list")
  expect_equal(as.character(sapply(angle.feats, class)), rep("numeric", 8))
  
  A = rbind(c(0, 0), c(0, 3), c(4, 3))
  featobj = createFeatureObject(X = A, y = 1:3, blocks = 1)
  angle.feats = suppressWarnings(calculateAngle(featobj))
  expect_identical(angle.feats$dist.ctr2best.sd, NA_real_)
  expect_identical(angle.feats$dist.ctr2worst.sd, NA_real_)
  expect_identical(angle.feats$angle.sd, NA_real_)
  expect_identical(angle.feats$y_ratio.best2worst.sd, NA_real_)
  expect_identical(angle.feats$dist.ctr2best.mean, sqrt(sum((c(2, 1.5) - c(0, 0))^2)))
  expect_identical(angle.feats$dist.ctr2worst.mean, sqrt(sum((c(2, 1.5) - c(4, 3))^2)))
  expect_identical(angle.feats$angle.mean, 180)
  expect_identical(angle.feats$y_ratio.best2worst.mean, 1)
})
