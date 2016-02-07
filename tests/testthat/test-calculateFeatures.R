context("Calculate All Features")

test_that("Non-Cellmapping Objects", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, y = rowSums(X^2))

  # (2) compute all non-cellmapping and non-expensive features
  features = calculateFeatures(feat.object,
    control = list(allow_cellmapping = FALSE, allow_costs = FALSE))

  # test return value types and ranges
  expect_identical(length(features), 122L)
  expect_list(features)

  # all objects are either NA, logical or a number
  expect_true(all(sapply(features, function(x) is.na(x) || is.logical(x) || testNumber(x))))

  # all features were computed without additional function evaluations
  expect_true(all(unlist(features[grep("costs_fun_evals", names(features))]) == 0L))

  # since the feature object was a non-cellmapping feature object, the following tests should pass
  expect_identical(features$basic.blocks_min, 1L)
  expect_identical(features$basic.blocks_max, 1L)
  expect_identical(features$basic.cells_total, 1L)
  expect_identical(features$basic.cells_filled, 1L)
  expect_identical(features$basic.allows_cm, FALSE)

  # (3) do the same, but blacklist the expensive features
  features = calculateFeatures(feat.object, control = list(
    allow_cellmapping = FALSE, allow_costs = TRUE, show_progress = FALSE,
    blacklist = c("ela_local", "ela_curv", "ela_conv")))

  # test return value types and ranges
  expect_identical(length(features), 122L)
  expect_list(features)

  # all objects are either NA, logical or a number
  expect_true(all(sapply(features, function(x) is.na(x) || is.logical(x) || testNumber(x))))

  # all features were computed without additional function evaluations
  expect_true(all(unlist(features[grep("costs_fun_evals", names(features))]) == 0))

  # as the feature object was a non-cellmapping feature object, the following tests should pass
  expect_identical(features$basic.blocks_min, 1L)
  expect_identical(features$basic.blocks_max, 1L)
  expect_identical(features$basic.cells_total, 1L)
  expect_identical(features$basic.cells_filled, 1L)
  expect_identical(features$basic.allows_cm, FALSE)

  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object))
  expect_error(calculateFeatures(feat.object, allow_costs = FALSE))
  expect_error(calculateFeatures(feat.object, allow_cellmapping = FALSE))
  expect_error(calculateFeatures(feat.object, control = list(
    subset = c("test123"), allow_costs = FALSE, allow_cellmapping = FALSE)))
})

test_that("Cellmapping Objects", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  y = rowSums(X^2)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(4, 3, 4, 3, 5))

  # (2) compute the non-expensive features
  features = calculateFeatures(feat.object, control = list(allow_costs = FALSE,
    show_progress = FALSE, cm_angle.show_warnings = FALSE))

  # test return value types and ranges
  expect_identical(length(features), 203L)
  expect_list(features)

  # all objects are either NA, logical or a number
  expect_true(all(sapply(features, function(x) is.na(x) || is.logical(x) || testNumber(x))))

  # all features were computed without additional function evaluations
  expect_true(all(unlist(features[grep("costs_fun_evals", names(features))]) == 0))

  # as the feature object was a cellmapping feature object, the following tests should pass
  expect_true(testInteger(features$basic.blocks_min))
  expect_true(testInteger(features$basic.blocks_max))
  expect_true(testInteger(features$basic.cells_total))
  expect_true(testInteger(features$basic.cells_filled))
  expect_true(features$basic.allows_cm)

  # (3) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object,
    control = list(subset = c("test123"), allow_costs = FALSE, allow_cellmapping = FALSE)))

  # (4) create a 2d-feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 2L, min = -10L, max = 10L)))
  y = rowSums(X^2)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(4, 3))
  
  # (5) compute the non-expensive features
  features = calculateFeatures(feat.object, control = list(allow_costs = FALSE,
    show_progress = FALSE, cm_angle.show_warnings = FALSE))
  
  # test return value types and ranges
  expect_identical(length(features), 296L)
  expect_true(all(vapply(features, length, integer(1L)) ==  1L))
  expect_list(features)
  
  # all objects are either NA, logical or a number
  expect_true(all(sapply(features, function(x) is.na(x) || is.logical(x) || testNumber(x))))
  
  # all features were computed without additional function evaluations
  expect_true(all(unlist(features[grep("costs_fun_evals", names(features))]) == 0))
  
  # as the feature object was a cellmapping feature object, the following tests should pass
  expect_true(testInteger(features$basic.blocks_min))
  expect_true(testInteger(features$basic.blocks_max))
  expect_true(testInteger(features$basic.cells_total))
  expect_true(testInteger(features$basic.cells_filled))
  expect_true(features$basic.allows_cm)
})

test_that("Underlying Functions Available (non-cellmapping)", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000L, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))

  # (2) compute all non-cm features:
  features = calculateFeatures(feat.object,
    control = list(allow_cellmapping = FALSE, show_progress = FALSE))

  # test return value types and ranges
  expect_identical(length(features), 170L)
  expect_list(features)

  # all objects are either NA, logical or a number
  expect_true(all(sapply(features, function(x) is.na(x) || is.logical(x) || testNumber(x))))

  # additional function evaluations
  x = unlist(features[grep("costs_fun_evals", names(features))])
  expect_true(all(x >= 0))
  expensive = x[grep("ela_conv|ela_curv|ela_local.", names(x))]
  expect_true(all(expensive > 0))
  expect_true(all(x[setdiff(names(x), names(expensive))] == 0))

  # as the feature object was a non-cellmapping feature object, the following tests should pass
  expect_identical(features$basic.blocks_min, 1L)
  expect_identical(features$basic.blocks_max, 1L)
  expect_identical(features$basic.cells_total, 1L)
  expect_identical(features$basic.cells_filled, 1L)
  expect_identical(features$basic.allows_cm, FALSE)

  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object))
  expect_error(calculateFeatures(feat.object, control = list(allow_costs = FALSE)))
  expect_error(calculateFeatures(feat.object, control = list(subset = c("test123"), 
    allow_costs = FALSE, allow_cellmapping = FALSE)))
})

test_that("Underlying Functions Available (cellmapping)", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = t(replicate(n = 2000, expr = runif(n = 5L, min = -10L, max = 10L)))
  feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2), blocks = 3L)

  # (2) compute all non-cm features:
  features = calculateFeatures(feat.object,
    control = list(show_progress = FALSE))

  # test return value types and ranges
  expect_identical(length(features), 251L)
  expect_list(features)

  # all objects are either NA, logical or a number
  expect_true(all(sapply(features, function(x) is.na(x) || is.logical(x) || testNumber(x))))

  # additional function evaluations
  x = unlist(features[grep("costs_fun_evals", names(features))])
  expect_true(all(x >= 0))
  expensive = x[grep("ela_conv|ela_curv|ela_local.", names(x))]
  expect_true(all(expensive > 0))
  expect_true(all(x[setdiff(names(x), names(expensive))] == 0))

  # as the feature object was a cellmapping feature object, the following tests should pass
  expect_true(testInteger(features$basic.blocks_min))
  expect_true(testInteger(features$basic.blocks_max))
  expect_true(testInteger(features$basic.cells_total))
  expect_true(testInteger(features$basic.cells_filled))
  expect_true(features$basic.allows_cm)

  # (4) test, whether an incorrect input causes an error:
  expect_error(calculateFeatures(feat.object, control = list(subset = c("test123"), 
    allow_costs = FALSE, allow_cellmapping = FALSE)))
})
