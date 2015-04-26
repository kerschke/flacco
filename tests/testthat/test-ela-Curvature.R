context("Features: Curvature")

test_that("Require original function", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the curvature features
  expect_error(calculateFeatureSet(feat.object, "curvature"))
})

test_that("Expected Output", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "curvature")
  
  # test return values
  expect_identical(length(features), 23L)
  expect_is(features, class = "list")
  expect_identical(as.character(sapply(features, class)), c(rep("numeric", 21L), "integer", "numeric"))
  
  expect_true( testNumber(features$curv.grad_norm.min, lower = 0) )
  expect_true( testNumber(features$curv.grad_norm.lq, lower = 0) )
  expect_true( testNumber(features$curv.grad_norm.mean, lower = 0) )
  expect_true( testNumber(features$curv.grad_norm.med, lower = 0) )
  expect_true( testNumber(features$curv.grad_norm.uq, lower = 0) )
  expect_true( testNumber(features$curv.grad_norm.max, lower = 0) )
  expect_true( testNumber(features$curv.grad_norm.sd, lower = 0) )
  expect_true( testNumber(features$curv.grad_scale.min, lower = 1) )
  expect_true( testNumber(features$curv.grad_scale.lq, lower = 1) )
  expect_true( testNumber(features$curv.grad_scale.mean, lower = 1) )
  expect_true( testNumber(features$curv.grad_scale.med, lower = 1) )
  expect_true( testNumber(features$curv.grad_scale.uq, lower = 1) )
  expect_true( testNumber(features$curv.grad_scale.max, lower = 1) )
  expect_true( testNumber(features$curv.grad_scale.sd, lower = 0) )
  expect_true( testNumber(features$curv.hessian_cond.min, lower = 1) )
  expect_true( testNumber(features$curv.hessian_cond.lq, lower = 1) )
  expect_true( testNumber(features$curv.hessian_cond.mean, lower = 1) )
  expect_true( testNumber(features$curv.hessian_cond.med, lower = 1) )
  expect_true( testNumber(features$curv.hessian_cond.uq, lower = 1) )
  expect_true( testNumber(features$curv.hessian_cond.max, lower = 1) )
  expect_true( testNumber(features$curv.hessian_cond.sd, lower = 0) )  
  expect_true( testNumber(features$curv.costs_fun_evals, lower = 0L) )
  expect_true( testNumber(features$curv.costs_runtime, lower = 0) )
  
  # test order of features
  grad_norm_features = features[grep("grad_norm", names(features))]
  x = unlist(grad_norm_features[grep("min|lq|med|uq|max", names(grad_norm_features))])
  expect_true( all(diff(x) >= 0) )
  x = unlist(grad_norm_features[grep("min|mean|max", names(grad_norm_features))])
  expect_true( all(diff(x) >= 0) )
  
  grad_scale_features = features[grep("grad_scale", names(features))]
  x = unlist(grad_scale_features[grep("min|lq|med|uq|max", names(grad_scale_features))])
  expect_true( all(diff(x) >= 0) )
  x = unlist(grad_scale_features[grep("min|mean|max", names(grad_scale_features))])
  expect_true( all(diff(x) >= 0) )
  
  hessian_features = features[grep("hessian", names(features))]
  x = unlist(hessian_features[grep("min|lq|med|uq|max", names(hessian_features))])
  expect_true( all(diff(x) >= 0) )
  x = unlist(hessian_features[grep("min|mean|max", names(hessian_features))])
  expect_true( all(diff(x) >= 0) )
})
