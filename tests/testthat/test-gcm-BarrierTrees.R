context("Features: Barrier Tree")

test_that("GCM-based Barrier Tree features are computed", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -10, 10))
  f = smoof::makeAckleyFunction(dimensions = 2)
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -10, upper = 10, blocks = c(4, 6))
  features = calculateFeatureSet(feat.object, "bt")
  expect_list(features)
  expect_true(all(vapply(features, length, integer(1L)) ==  1L))
  expect_identical(length(features), 93L)

  set.seed(2015 * 03 * 25)
  X = createInitialSample(n.obs = 1000L, dim = 4L,
    control = list(init_sample.lower = -5, init_sample.upper = 5L))
  f = smoof::makeBBOBFunction(dimension = 4, fid = 23, iid = 1)
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    blocks = c(4, 3, 2, 3))
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

test_that("Barrier Trees require blocks", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -10, 10))
  y = rowSums(X^2)
  feat.object = createFeatureObject(X = X, y = y, lower = -10, upper = 10)
  expect_error(calculateFeatureSet(feat.object, "bt"))
})

test_that("Dealing with plateaus", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = expand.grid(1:4, 1:3)
  y = c(1, 1, 3, 2, 1, 1, 4, 2.5, 1.5, 2.5, 3.5, 0)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(4, 3),
    lower = c(0.5, 0.5), upper = c(4.5, 3.5))

  # (2) manually compute the barrier tree for the min-approach
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  control = list(gcm.approaches = "min", gcm.cf_power = 256L)
  gcm.control = list(cf.power = 256L)
  yvals = getObjectivesByApproach(feat.object, "min")
  expect_identical(yvals, y)
  sparse.matrix = calculateSparseMatrix(feat.object, yvals)
  expect_identical(sparse.matrix[c(1, 2, 5, 6), c(1, 2, 5, 6)], matrix(0.25, 4, 4))
  canonical.list = computeCanonical(sparse.matrix)
  expect_identical(canonical.list$no.attractors, 6L)
  expect_identical(canonical.list$canonical.form[1:6, 1:6],
    rbind(cbind(matrix(0.25, 4, 4), 0, 0), cbind(0, 0, 0, 0, diag(2))))
  expect_identical(dim(canonical.list$canonical.form), c(12L, 12L))
  fundamental.list = computeFundamental(
    canonical.list = canonical.list,
    gcm.control = gcm.control)
  expect_identical(fundamental.list$fundamental.mat[1:6,],
    rbind(cbind(matrix(0.25, 4, 4), 0, 0), cbind(0, 0, 0, 0, diag(2))))
  expect_identical(dim(fundamental.list$fundamental.mat), c(12L, 6L))
  expect_identical(fundamental.list$permutation.index, canonical.list$permutation.index)
  expect_identical(fundamental.list$seq.closed.classes, 1:6)
  barrier.tree = createBarrierTree(feat.object, fundamental.list,
    canonical.list, yvals = getObjectivesByApproach(feat.object, "min"),
    control)
  expect_identical(barrier.tree$root, 3L)
  expect_identical(barrier.tree$tree.nodes, c(3L, 1L, 8L, 4L, 12L))
  ydiffs = c(2, 0.5, 2.5, 0.5)
  expect_identical(barrier.tree$diffs, ydiffs)

  # (3) compute all non-cm features:
  features = calculateFeatureSet(feat.object, "bt")

  # (4) check for correct y-diff computation
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

  ## (5) Test on a complete plateau
  set.seed(2015*03*26)
  X = expand.grid(1:3, 1:2)
  y = rep(2, 6)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(3, 2),
    lower = c(0.5, 0.5), upper = c(3.5, 2.5))
  expect_error(calculateFeatureSet(feat.object, "bt"))

  ## (6) Test on a plateau with one peak
  set.seed(2015*03*26)
  X = expand.grid(1:3, 1:2)
  y = c(rep(2, 5), 10)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(3, 2),
    lower = c(0.5, 0.5), upper = c(3.5, 2.5))
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  control = list(gcm.approaches = "min", gcm.cf_power = 256L)
  gcm.control = list(cf.power = 256L)
  yvals = getObjectivesByApproach(feat.object, "min")
  expect_identical(yvals, y)
  sparse.matrix = calculateSparseMatrix(feat.object, yvals)
  expect_identical(sparse.matrix[,6], rep(0, 6))
  expect_identical(sparse.matrix[,-6],
    rbind(
      rep(c(0.25, 0, 0.25), c(2, 1, 2)), rep(1/5, 5),
      c(0, 1/3, 1/3, 0, 1/3),
      rep(c(0.25, 0, 0.25), c(2, 1, 2)), rep(1/5, 5),
      c(0, 1/3, 1/3, 0, 1/3)
    ))
  canonical.list = computeCanonical(sparse.matrix)
  expect_identical(canonical.list$no.attractors, 5L)
  expect_identical(canonical.list$canonical.form, sparse.matrix)
  expect_identical(dim(canonical.list$canonical.form), c(6L, 6L))
  fundamental.list = computeFundamental(
    canonical.list = canonical.list,
    gcm.control = gcm.control)
  expect_equal(fundamental.list$fundamental.mat, t(replicate(6, c(4, 5, 3, 4, 5) / 21)))
  expect_identical(dim(fundamental.list$fundamental.mat), c(6L, 5L))
  expect_identical(fundamental.list$permutation.index, canonical.list$permutation.index)
  expect_identical(fundamental.list$seq.closed.classes, 1:5)
  barrier.tree = createBarrierTree(feat.object, fundamental.list,
    canonical.list, yvals = getObjectivesByApproach(feat.object, "min"),
    control)
  expect_identical(barrier.tree$root, 1L)
  expect_identical(barrier.tree$tree.nodes, 1L)
  expect_identical(barrier.tree$max.levels, 0L)

  ## (7) Test a landscape with plateau barrier
  set.seed(2015*03*26)
  X = expand.grid(1:3, 1:2)
  y = c(3, 7, 2, 3, 7, 5)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(3, 2),
    lower = c(0.5, 0.5), upper = c(3.5, 2.5))
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  control = list(gcm.approaches = "min", gcm.cf_power = 256L)
  gcm.control = list(cf.power = 256L)
  yvals = getObjectivesByApproach(feat.object, "min")
  expect_identical(yvals, y)
  sparse.matrix = calculateSparseMatrix(feat.object, yvals)
  expect_identical(sparse.matrix[, c(2, 5)], matrix(0, ncol = 2, nrow = 6))
  expect_identical(sparse.matrix[-c(2, 5), -c(2, 5)],
    matrix(c(1, 0, 1, 0, 0, 2, 0, 0, 1, 0, 1, 0, 0, 2, 0, 0) / 2, 4, 4, byrow = TRUE))
  canonical.list = computeCanonical(sparse.matrix)
  expect_identical(canonical.list$no.attractors, 3L)
  expect_identical(dim(canonical.list$canonical.form), c(6L, 6L))
  fundamental.list = computeFundamental(
    canonical.list = canonical.list,
    gcm.control = gcm.control)
  expect_identical(dim(fundamental.list$fundamental.mat), c(6L, 3L))
  expect_identical(fundamental.list$permutation.index, canonical.list$permutation.index)
  expect_identical(fundamental.list$seq.closed.classes, 1:3)
  barrier.tree = createBarrierTree(feat.object, fundamental.list,
    canonical.list, yvals = getObjectivesByApproach(feat.object, "min"),
    control)
  expect_identical(barrier.tree$root, 2L)
  expect_identical(barrier.tree$tree.nodes, c(2L, 1L, 3L))
  expect_identical(barrier.tree$max.levels, 1L)
})

test_that("Show Error", {
  set.seed(2015*03*26)

  # (1) create a feature object:
  X = expand.grid(1:4, 1:3)
  y = c(1, 1, 3, 2, 1, 1, 4, 2.5, 1.5, 2.5, 3.5, 0)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(4, 3),
    lower = c(0.5, 0.5), upper = c(4.5, 3.5))

  expect_error(calculateFeatureSet(feat.object, "bt",
    control = list(allow_cellmapping = FALSE)))
  feat.object = createFeatureObject(X = X, y = y,
    lower = c(0.5, 0.5), upper = c(4.5, 3.5))
  expect_error(calculateFeatureSet(feat.object, "bt"))
})
