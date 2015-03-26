context("calculateMetaModel")

test_that("Calculation of MetaModel is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the meta model features
  features = calculateMetaModel(feat.object)
  
  # test return value types
  expect_is(features, "list")
  expect_true( testNumber(features$appr.lin.simple.adj_r2) )
  expect_true( testNumber(features$appr.lin.simple.intercept) )
  expect_true( testNumber(features$appr.lin.simple.coef.min) )
  expect_true( testNumber(features$appr.lin.simple.coef.max) )
  expect_true( testNumber(features$appr.lin.simple.coef.max_by_min) )
  expect_true( testNumber(features$appr.lin.w_interact.adj_r2) )
  expect_true( testNumber(features$appr.quad.simple.adj_r2) )
  expect_true( testNumber(features$appr.quad.simple.cond) )
  expect_true( testNumber(features$appr.quad.w_interact.adj_r2) )
  
  # test return values and ranges
  expect_equal( features$appr.lin.simple.coef.max_by_min, 
                features$appr.lin.simple.coef.max / features$appr.lin.simple.coef.min, 
                tolerance = 0.00001 )
  expect_true( testNumber(features$appr.lin.simple.adj_r2, lower=-1, upper=1) )
  expect_true( testNumber(features$appr.lin.w_interact.adj_r2, lower=-1, upper=1) )
  expect_true( testNumber(features$appr.quad.simple.adj_r2, lower=-1, upper=1) )
  expect_true( testNumber(features$appr.quad.w_interact.adj_r2, lower=-1, upper=1) )
})