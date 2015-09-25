context("Features: ELA - Meta Model")

test_that("Expected Output", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2))

  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "ela_meta")

  # test return value types and ranges
  expect_identical(length(features), 11L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 9L), "integer", "numeric"))

  expect_true(testNumber(features$ela_meta.lin_simple.adj_r2, lower = -1L, upper = 1L))
  expect_true(testNumber(features$ela_meta.lin_simple.intercept))
  expect_true(testNumber(features$ela_meta.lin_simple.coef.min))
  expect_true(testNumber(features$ela_meta.lin_simple.coef.max))
  expect_true(testNumber(features$ela_meta.lin_simple.coef.max_by_min))
  expect_true(testNumber(features$ela_meta.lin_w_interact.adj_r2, lower = -1L, upper = 1L))
  expect_true(testNumber(features$ela_meta.quad_simple.adj_r2, lower = -1L, upper = 1L))
  expect_true(testNumber(features$ela_meta.quad_simple.cond))
  expect_true(testNumber(features$ela_meta.quad_w_interact.adj_r2, lower = -1L, upper = 1L))
  expect_identical(features$ela_meta.costs_fun_evals, 0L)
  expect_true(testNumber(features$ela_meta.costs_runtime, lower = 0L))
  
  # test return values
  expect_equal(features$ela_meta.lin_simple.coef.max_by_min, 
    features$ela_meta.lin_simple.coef.max / features$ela_meta.lin_simple.coef.min, 
    tolerance = 0.00001)
})
