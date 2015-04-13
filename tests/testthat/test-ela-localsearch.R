context("Features: Local Search")

test_that("Require original function", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the local search features
  expect_error(calculateFeatureSet(feat.object, "local_search"))
  
})

test_that("Expected output", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "local_search")
  
  # test return values
  expect_equal(length(features), 15L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)),
    c("integer", rep("numeric", 6L), "integer", rep("numeric", 4L), "integer", "integer", "numeric"))
  
  expect_true( testNumber(features$ls.n_loc_opt.abs, lower = 0) )
  expect_true( testNumber(features$ls.n_loc_opt.rel, lower = 0, upper = 1) )
  expect_true( testNumber(features$ls.best2mean_contr.orig) )
  expect_true( testNumber(features$ls.best2mean_contr.ratio, lower = 0, upper = 1) )
  expect_true( testNumber(features$ls.basin_sizes.avg_best, lower = 0, upper = 1) )
  expect_true( testNumber(features$ls.basin_sizes.avg_non_best, lower = 0, upper = 1) )
  expect_true( testNumber(features$ls.basin_sizes.avg_worst, lower = 0, upper = 1) )
  expect_true( testNumber(features$ls.fun_evals.min, lower = 0) )
  expect_true( testNumber(features$ls.fun_evals.lq, lower = 0) )
  expect_true( testNumber(features$ls.fun_evals.mean, lower = 0) )
  expect_true( testNumber(features$ls.fun_evals.med, lower = 0) )
  expect_true( testNumber(features$ls.fun_evals.uq, lower = 0) )
  expect_true( testNumber(features$ls.fun_evals.max, lower = 0) )
  expect_true( testNumber(features$ls.costs_fun_evals, lower = 0L) )
  expect_true( testNumber(features$ls.costs_runtime, lower = 0) )
  
  # test order of function evaluation features
  fun_eval_features = features[grep("fun_evals", names(features))]
  x = unlist(fun_eval_features[grep("min|lq|med|uq|max|costs", names(fun_eval_features))])
  expect_true( all(diff(x) >= 0) )
  x = unlist(fun_eval_features[grep("min|mean|max|fun", names(fun_eval_features))])
  expect_true( all(diff(x) >= 0) )
})