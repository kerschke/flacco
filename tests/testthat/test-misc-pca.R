context("Features: Principal Component")


test_that("Calculation of PCA is possible", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute the PCA features
  features = calculatePrincipalComponentFeatures(feat.object)
  
  # test return value types and ranges
  expect_equal(length(features), 10L)
  expect_is(features, class = "list")
  expect_equal(as.character(sapply(features, class)), c(rep("numeric", 8L), "integer", "numeric"))
  expect_true( testNumber(features$pc.expl_var.cov_x, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var.cor_x, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var.cov_init, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var.cor_init, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var_PC1.cov_x, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var_PC1.cor_x, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var_PC1.cov_init, lower = 0, upper = 1) )
  expect_true( testNumber(features$pc.expl_var_PC1.cor_init, lower = 0, upper = 1) ) 
  expect_identical(features$pc.costs_fun_evals, 0L)
  expect_true( testNumber(features$pc.costs_runtime, lower = 0) )
  
})
