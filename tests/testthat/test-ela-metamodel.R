context("Features: Meta Model")

test_that("Calculation of MetaModel is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the meta model features
  features = calculateMetaModelFeatures(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 11L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 9L), "integer", "numeric"))
  
  expect_true( testNumber(features$meta.lin.simple.adj_r2, lower=-1, upper=1) )
  expect_true( testNumber(features$meta.lin.simple.intercept) )
  expect_true( testNumber(features$meta.lin.simple.coef.min) )
  expect_true( testNumber(features$meta.lin.simple.coef.max) )
  expect_true( testNumber(features$meta.lin.simple.coef.max_by_min) )
  expect_true( testNumber(features$meta.lin.w_interact.adj_r2, lower=-1, upper=1) )
  expect_true( testNumber(features$meta.quad.simple.adj_r2, lower=-1, upper=1) )
  expect_true( testNumber(features$meta.quad.simple.cond) )
  expect_true( testNumber(features$meta.quad.w_interact.adj_r2, lower=-1, upper=1) )
  expect_identical(features$meta.costs_fun_evals, 0L)
  expect_true( testNumber(features$meta.costs_runtime, lower = 0) )
  
  # test return values
  expect_equal( features$meta.lin.simple.coef.max_by_min, 
                features$meta.lin.simple.coef.max / features$meta.lin.simple.coef.min, 
                tolerance = 0.00001 )
})