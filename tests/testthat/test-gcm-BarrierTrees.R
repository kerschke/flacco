context("Features: Barrier Tree")

test_that("GCM-based Barrier Tree features are computed", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -10, 10))
  f = smoof::makeAckleyFunction(dimensions = 2)
  f = smoof::makeBBOBFunction(dimension = 2, fid = 23, iid = 1)
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -10, upper = 10, blocks = c(4, 6))
  features = calculateFeatureSet(feat.object, "bt")
  expect_list(features)
  expect_true(all(vapply(features, length, integer(1L)) ==  1L))
  expect_identical(length(features), 93L)
})

test_that("GCM-based Barrier Tree fallback is computed for boring barrier trees", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -10, 10))
  y = rowSums(X^2)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -10, upper = 10, blocks = c(5, 3))
  features = calculateFeatureSet(feat.object, "bt")
  nm = names(features)
  fts1 = features[grep("basin_ratio|basin_intersection.m", nm)]
  expect_true(all(unlist(fts1) == 1))
  fts0 = features[grep("attractor_dists.m|fun_evals", nm)]
  expect_true(all(unlist(fts0) == 0))
  ftsNA = features[grep("depth_levels_ratio|diffs|.sd", nm)]
  expect_true(all(is.na(unlist(ftsNA))))
  expect_true(all(vapply(features, length, integer(1L)) ==  1L))
})

test_that("Barrier Trees require 2D-cellmapping objects", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -10, 10))
  y = rowSums(X^2)
  feat.object = createFeatureObject(X = X, y = y, lower = -10, upper = 10)
  expect_error(calculateFeatureSet(feat.object, "bt"))

  set.seed(2015 * 03 * 25)
  X = replicate(3, runif(1500, -10, 10))
  y = rowSums(X^2)
  feat.object = createFeatureObject(X = X, y = y, lower = -10, upper = 10, blocks = 4)
  expect_error(calculateFeatureSet(feat.object, "bt"))
})

test_that("Dealing with plateaus", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = expand.grid(1:4, 1:3)
  y = c(1, 1, 3, 2, 1, 1, 4, 2.5, 1.5, 2.5, 3.5, 0)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(4, 3),
    lower = c(0.5, 0.5), upper = c(4.5, 3.5))

  # (2) compute all non-cm features:
  features = calculateFeatureSet(feat.object, "bt")

  # (3) check for correct y-diff computation
  ydiffs = c(2, 0.5, 0.5, 2.5)
  expect_equal(features$bt.min.diffs.min, min(ydiffs))
  expect_equal(features$bt.min.diffs.mean, mean(ydiffs))
  expect_equal(features$bt.min.diffs.median, median(ydiffs))
  expect_equal(features$bt.min.diffs.max, max(ydiffs))
  expect_equal(features$bt.min.diffs.sd, sd(ydiffs))

  expect_equal(features$bt.mean.diffs.min, min(ydiffs))
  expect_equal(features$bt.mean.diffs.mean, mean(ydiffs))
  expect_equal(features$bt.mean.diffs.median, median(ydiffs))
  expect_equal(features$bt.mean.diffs.max, max(ydiffs))
  expect_equal(features$bt.mean.diffs.sd, sd(ydiffs))

  expect_equal(features$bt.near.diffs.min, min(ydiffs))
  expect_equal(features$bt.near.diffs.mean, mean(ydiffs))
  expect_equal(features$bt.near.diffs.median, median(ydiffs))
  expect_equal(features$bt.near.diffs.max, max(ydiffs))
  expect_equal(features$bt.near.diffs.sd, sd(ydiffs))
})
