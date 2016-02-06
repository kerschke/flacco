context("Features: ELA - Local Search")

test_that("Require original function", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2))

  # (2) compute the local search features
  expect_error(calculateFeatureSet(feat.object, "ela_local"))
})

test_that("Expected output - minimization", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "ela_local")

  # test return values
  expect_identical(length(features), 16L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c("integer", rep("numeric", 6L), "integer", rep("numeric", 4L), 
    rep(c("integer", "numeric"), 2L)))

  expect_true(testNumber(features$ela_local.n_loc_opt.abs, lower = 0L))
  expect_true(testNumber(features$ela_local.n_loc_opt.rel, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.best2mean_contr.orig))
  expect_true(testNumber(features$ela_local.best2mean_contr.ratio, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.basin_sizes.avg_best, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.basin_sizes.avg_non_best, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.basin_sizes.avg_worst, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.fun_evals.min, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.lq, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.mean, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.med, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.uq, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.max, lower = 0L))
  expect_true(testNumber(features$ela_local.costs_fun_evals, lower = 0L))
  expect_true(testNumber(features$ela_local.costs_runtime, lower = 0L))

  # test order of function evaluation features
  fun_eval_features = features[grep("fun_evals", names(features))]
  x = unlist(fun_eval_features[grep("min|lq|med|uq|max|costs", names(fun_eval_features))])
  expect_true(all(diff(x) >= 0))
  x = unlist(fun_eval_features[grep("min|mean|max|costs", names(fun_eval_features))])
  expect_true(all(diff(x) >= 0))
})

test_that("Expected output - maximization", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) -sum(x^2), minimize = FALSE)

  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "ela_local")

  # test return values
  expect_identical(length(features), 16L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c("integer", rep("numeric", 6L), "integer", rep("numeric", 4L), 
      rep(c("integer", "numeric"), 2L)))

  expect_true(testNumber(features$ela_local.n_loc_opt.abs, lower = 0L))
  expect_true(testNumber(features$ela_local.n_loc_opt.rel, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.best2mean_contr.orig))
  expect_true(testNumber(features$ela_local.best2mean_contr.ratio, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.basin_sizes.avg_best, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.basin_sizes.avg_non_best, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.basin_sizes.avg_worst, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_local.fun_evals.min, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.lq, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.mean, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.med, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.uq, lower = 0L))
  expect_true(testNumber(features$ela_local.fun_evals.max, lower = 0L))
  expect_true(testNumber(features$ela_local.costs_fun_evals, lower = 0L))
  expect_true(testNumber(features$ela_local.costs_runtime, lower = 0L))

  # test order of function evaluation features
  fun_eval_features = features[grep("fun_evals", names(features))]
  x = unlist(fun_eval_features[grep("min|lq|med|uq|max|costs", names(fun_eval_features))])
  expect_true(all(diff(x) >= 0))
  x = unlist(fun_eval_features[grep("min|mean|max|costs", names(fun_eval_features))])
  expect_true(all(diff(x) >= 0))
})
