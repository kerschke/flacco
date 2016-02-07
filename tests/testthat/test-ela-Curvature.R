context("Features: ELA - Curvature")

test_that("Require original function", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2))

  # (2) error because of missing function
  expect_error(calculateFeatureSet(feat.object, "ela_curv"))
})

test_that("Required sample size is too big for the given data", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 100, expr = runif(n = 3L, min = -10L, max = 10L)))
  f = function(x) sum(x^2)
  feat.object = createFeatureObject(X = X, fun = f)
  
  # (2) error because of missing function
  expect_error(calculateFeatureSet(feat.object, "ela_curv"))
})

test_that("Expected Output", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "ela_curv")

  # test return values
  expect_identical(length(features), 26L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 24L), "integer", "numeric"))

  expect_true(testNumber(features$ela_curv.grad_norm.min, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.lq, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.mean, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.med, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.uq, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.max, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.sd, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.nas, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.min, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.lq, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.mean, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.med, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.uq, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.max, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.sd, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_scale.nas, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.min, lower = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.lq, lower = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.mean, lower = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.med, lower = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.uq, lower = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.max, lower = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.sd, lower = 0L))
  expect_true(testNumber(features$ela_curv.hessian_cond.nas, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_curv.costs_fun_evals, lower = 0L))
  expect_true(testNumber(features$ela_curv.costs_runtime, lower = 0L))

  # test order of features
  grad_norm_features = features[grep("grad_norm", names(features))]
  x = unlist(grad_norm_features[grep("min|lq|med|uq|max", names(grad_norm_features))])
  expect_true(all(diff(x) >= 0L))
  x = unlist(grad_norm_features[grep("min|mean|max", names(grad_norm_features))])
  expect_true(all(diff(x) >= 0L))

  grad_scale_features = features[grep("grad_scale", names(features))]
  x = unlist(grad_scale_features[grep("min|lq|med|uq|max", names(grad_scale_features))])
  expect_true(all(diff(x) >= 0L))
  x = unlist(grad_scale_features[grep("min|mean|max", names(grad_scale_features))])
  expect_true(all(diff(x) >= 0L))

  hessian_features = features[grep("hessian", names(features))]
  x = unlist(hessian_features[grep("min|lq|med|uq|max", names(hessian_features))])
  expect_true(all(diff(x) >= 0L))
  x = unlist(hessian_features[grep("min|mean|max", names(hessian_features))])
  expect_true(all(diff(x) >= 0L))
})


test_that("Expected Output on Bounds", {
  # (1) create a feature object:
  X = cbind(x1 = rep(c(0, 10), 2), x2 = rep(c(0, 10), each = 2))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute the meta model features
  features = calculateFeatureSet(feat.object, "ela_curv", control = list(ela_curv.sample_size = 4))

  # test return values
  expect_identical(length(features), 26L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 24L), "integer", "numeric"))

  hess_feats = unlist(features[setdiff(grep("hessian", names(features)),
    grep("hessian_cond.nas", names(features)))])
  comp_feats = vapply(names(hess_feats), function(x) NA_real_, double(1L))
  expect_identical(hess_feats, comp_feats)

  expect_true(testNumber(features$ela_curv.grad_norm.lq, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.mean, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.med, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.uq, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.max, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.sd, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_norm.nas, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.min, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.lq, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.mean, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.med, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.uq, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.max, lower = 1L))
  expect_true(testNumber(features$ela_curv.grad_scale.sd, lower = 0L))
  expect_true(testNumber(features$ela_curv.grad_scale.nas, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_curv.hessian_cond.min, lower = 1L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.lq, lower = 1L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.mean, lower = 1L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.med, lower = 1L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.uq, lower = 1L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.max, lower = 1L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.sd, lower = 0L, na.ok = TRUE))
  expect_true(testNumber(features$ela_curv.hessian_cond.nas, lower = 0L, upper = 1L))
  expect_true(testNumber(features$ela_curv.costs_fun_evals, lower = 0L))
  expect_true(testNumber(features$ela_curv.costs_runtime, lower = 0L))

  # test order of features
  grad_norm_features = features[grep("grad_norm", names(features))]
  x = unlist(grad_norm_features[grep("min|lq|med|uq|max", names(grad_norm_features))])
  expect_true(all(diff(x) >= 0L))
  x = unlist(grad_norm_features[grep("min|mean|max", names(grad_norm_features))])
  expect_true(all(diff(x) >= 0L))
  
  grad_scale_features = features[grep("grad_scale", names(features))]
  x = unlist(grad_scale_features[grep("min|lq|med|uq|max", names(grad_scale_features))])
  expect_true(all(diff(x) >= 0L))
  x = unlist(grad_scale_features[grep("min|mean|max", names(grad_scale_features))])
  expect_true(all(diff(x) >= 0L))
})
