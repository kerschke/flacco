context("Calculate ALL Features")

test_that("Non-Cellmapping Objects", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y)
  
  # (2) compute all non-cellmapping and non-expensive features
  features = calculateFeatures(feat.object, allow.cellmapping = FALSE, 
    allow.additional_costs = FALSE)
  
  # test return value types and ranges
  expect_equal(length(features), 121L)
  expect_is(features, class = "list")

  # all objects are either NA, logical or a number
  expect_true( all(sapply(features, function(x) is.na(x) || is.logical(x) || assertNumber(x))) )
  
  # all features were computed without additional function evaluations
  expect_true( all(unlist(features[grep("costs_fun_evals", names(features))]) == 0) )
  
  # since the feature object was a non-cellmapping feature object, the following tests should pass
  expect_identical(features$basic.blocks.min, 1L)
  expect_identical(features$basic.blocks.max, 1L)
  expect_identical(features$basic.cells.total, 1L)
  expect_identical(features$basic.cells.filled, 1L)
  expect_identical(features$basic.allows_cm, FALSE)
  
  # (3) do the same, but blacklist the expensive features
  features = calculateFeatures(feat.object, allow.cellmapping = FALSE, allow.additional_costs = TRUE,
    blacklist = c("local_search", "curvature", "convexity"), control = list(show_progress = FALSE))
  
  # test return value types and ranges
  expect_identical(length(features), 121L)
  expect_is(features, class = "list")
  
  # all objects are either NA, logical or a number
  expect_true( all(sapply(features, function(x) is.na(x) || is.logical(x) || assertNumber(x))) )
  
  # all features were computed without additional function evaluations
  expect_true( all(unlist(features[grep("costs_fun_evals", names(features))]) == 0) )
  
  # as the feature object was a non-cellmapping feature object, the following tests should pass
  expect_identical(features$basic.blocks.min, 1L)
  expect_identical(features$basic.blocks.max, 1L)
  expect_identical(features$basic.cells.total, 1L)
  expect_identical(features$basic.cells.filled, 1L)
  expect_identical(features$basic.allows_cm, FALSE)
  
  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object))
  expect_error(calculateFeatures(feat.object, allow.additional_costs = FALSE))
  expect_error(calculateFeatures(feat.object, allow.cellmapping = FALSE))
  expect_error(calculateFeatures(feat.object, subset = c("test123"), 
    allow.additional_costs = FALSE, allow.cellmapping = FALSE))
})

test_that("Cellmapping Objects", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
  y = apply(X, 1, function(x) sum(x^2))
  feat.object = createFeatureObject(X = X, y = y, blocks = c(2, 3, 4, 3, 5))
  
  # (2) compute the non-expensive features
  features = calculateFeatures(feat.object, allow.additional_costs = FALSE, 
    control = list(show_progress = FALSE, angle.show_warnings = FALSE))
  
  # test return value types and ranges
  expect_identical(length(features), 211L)
  expect_is(features, class = "list")
  
  # all objects are either NA, logical or a number
  expect_true( all(sapply(features, function(x) is.na(x) || is.logical(x) || assertNumber(x))) )
  
  # all features were computed without additional function evaluations
  expect_true( all(unlist(features[grep("costs_fun_evals", names(features))]) == 0) )
  
  # as the feature object was a cellmapping feature object, the following tests should pass
  expect_true( assertInteger(features$basic.blocks.min) )
  expect_true( assertInteger(features$basic.blocks.max) )
  expect_true( assertInteger(features$basic.cells.total) )
  expect_true( assertInteger(features$basic.cells.filled) )
  expect_true( features$basic.allows_cm )
  
  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object))
  expect_error(calculateFeatures(feat.object, allow.cellmapping = FALSE))
  expect_error(calculateFeatures(feat.object, subset = c("test123"), 
    allow.additional_costs = FALSE, allow.cellmapping = FALSE))
})

test_that("Underlying Functions Available (non-cellmapping)", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5L, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
  
  # (2) compute all non-cm features:
  features = calculateFeatures(feat.object, allow.cellmapping = FALSE, 
    control = list(show_progress = FALSE))
  
  # test return value types and ranges
  expect_identical(length(features), 165L)
  expect_is(features, class = "list")
  
  # all objects are either NA, logical or a number
  expect_true( all(sapply(features, function(x) is.na(x) || is.logical(x) || assertNumber(x))) )
  
  # additional function evaluations
  x = unlist(features[grep("costs_fun_evals", names(features))])
  expect_true( all(x >= 0) )
  expensive = x[grep("conv|curv|ls.", names(x))]
  expensive = expensive[!grepl("lvlset", names(expensive))]
  expect_true( all(expensive > 0) )
  expect_true( all(x[setdiff(names(x), names(expensive))] == 0) )
  
  # as the feature object was a non-cellmapping feature object, the following tests should pass
  expect_identical(features$basic.blocks.min, 1L)
  expect_identical(features$basic.blocks.max, 1L)
  expect_identical(features$basic.cells.total, 1L)
  expect_identical(features$basic.cells.filled, 1L)
  expect_identical(features$basic.allows_cm, FALSE)
  
  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object))
  expect_error(calculateFeatures(feat.object, allow.additional_costs = FALSE))
  expect_error(calculateFeatures(feat.object, subset = c("test123"), 
    allow.additional_costs = FALSE, allow.cellmapping = FALSE))
})

test_that("Underlying Functions Available (cellmapping)", {
  set.seed(2015*03*26)
  
  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5L, min = -10, max = 10)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2), blocks = 3)
  
  # (2) compute all non-cm features:
  features = calculateFeatures(feat.object, control = list(show_progress = FALSE))
  
  # test return value types and ranges
  expect_identical(length(features), 255L)
  expect_is(features, class = "list")
  
  # all objects are either NA, logical or a number
  expect_true( all(sapply(features, function(x) is.na(x) || is.logical(x) || assertNumber(x))) )
  
  # additional function evaluations
  x = unlist(features[grep("costs_fun_evals", names(features))])
  expect_true( all(x >= 0) )
  expensive = x[grep("conv|curv|ls.", names(x))]
  expensive = expensive[!grepl("lvlset", names(expensive))]
  expensive = expensive[!grepl("cm_conv", names(expensive))]
  expect_true( all(expensive > 0) )
  expect_true( all(x[setdiff(names(x), names(expensive))] == 0) )
  
  # as the feature object was a cellmapping feature object, the following tests should pass
  expect_true( assertInteger(features$basic.blocks.min) )
  expect_true( assertInteger(features$basic.blocks.max) )
  expect_true( assertInteger(features$basic.cells.total) )
  expect_true( assertInteger(features$basic.cells.filled) )
  expect_true( features$basic.allows_cm )
  
  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object, subset = c("test123"), 
    allow.additional_costs = FALSE, allow.cellmapping = FALSE))
})
