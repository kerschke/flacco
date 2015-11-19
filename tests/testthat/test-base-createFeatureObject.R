context("Base: Create Feature Object")

test_that("Basic FeatureObject ", {
  feat.object = createFeatureObject(iris, objective = "Species")
  expect_true(feat.object$minimize)
  expect_equal(feat.object$lower, apply(iris[, -5L], 2L, min))
  expect_equal(feat.object$upper, apply(iris[, -5L], 2L, max))
  expect_equal(feat.object$dim, ncol(iris) - 1L)
  expect_equal(feat.object$n.obs, nrow(iris))
  expect_equal(feat.object$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(feat.object$objective.name, "Species")
  expect_equal(feat.object$blocks, rep(1L, ncol(iris) - 1L))
  expect_false(feat.object$allows.cellmapping)
  expect_identical(feat.object$init.grid, data.frame(iris, cell.ID = 1L))
  expect_identical(feat.object$cell.centers, 
    data.frame(t(colMeans(rbind(apply(iris[, -5L], 2L, max), 
      apply(iris[, -5L], 2L, min)))), cell.ID = 1L))
  expect_identical(feat.object$cell.size, 
    apply(iris[, -5L], 2L, function(x) diff(range(x))))
  expect_equal(feat.object$env$init, iris)
  expect_is(feat.object, class = "FeatureObject")
})
 
test_that("FeatureObject for a maximization problem", {
  feat.object = createFeatureObject(iris, objective = "Species", minimize = FALSE)
  expect_false(feat.object$minimize)
})
  
test_that("FeatureObject with a custom lower bound", {
  feat.object = createFeatureObject(iris, objective = "Species", lower = -20)
  expect_equal(feat.object$lower, rep(-20, ncol(iris) - 1L))
})

test_that("Error-Handling", {
  # FeatureObject with upper < lower
  expect_error(createFeatureObject(iris, objective = "Species",
    lower = -20L, upper = -30L))

  # FeatureObject with lower > upper
  expect_error(createFeatureObject(iris, objective = "Species", lower = 30L))

  # FeatureObject without defining an objective
  expect_error(createFeatureObject(iris))

  # differing rows of features and variables
  expect_error(createFeatureObject(X = matrix(1L, nrow = 2L), y = 1L))

  # FeatureObject with differing dimensions for the lower bound
  expect_error(createFeatureObject(X = matrix(1L, nrow = 2L, ncol = 3L),
    y = rep(1L, 2L), lower = -c(1L, 1L), upper = 10L, blocks = 5L))

  # FeatureObject with differing dimensions for the upper bound
  expect_error(createFeatureObject(X = matrix(1L, nrow = 2L, ncol = 3L),
    y = rep(1L, 2L), lower = -1L, upper = c(10L, 10L), blocks = 5L))

  # FeatureObject with differing dimensions for the blocks
  expect_error(createFeatureObject(X = matrix(1L, nrow = 2L, ncol = 3L),
    y = rep(1L, 2L), lower = -1L, upper = 10L, blocks = c(5L, 5L)))
  
})

test_that("FeatureObject with enabled Cell-Mapping", {
  feat.object = createFeatureObject(iris, objective = "Species", blocks = 5L)
  expect_true(feat.object$allows.cellmapping)
  expect_equal(dim(feat.object$init.grid), dim(iris) + c(0L, 1L))
  expect_true(all(feat.object$init.grid$cell.ID <= 5^4))  

  # with a single cell"
  feat.object = createFeatureObject(iris, objective = "Species", blocks = 1L)
  expect_equal(feat.object$init.grid$cell.ID, rep(1L, nrow(iris)))
  expect_is(feat.object, class = "FeatureObject")
})

test_that("Initial data can be composed from X and y", {
  feat.object = createFeatureObject(X = iris[, -5], y = iris$Species)
  expect_equal(feat.object$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(feat.object$objective.name, "y")
})

test_that("Objective values can be computed from X and fun", {
  values = matrix(c(1:10, 10:1), ncol = 2L)
  values = data.frame(values)
  colnames(values) = c("a1", "a2")

  feat.object = createFeatureObject(X = values, fun = function(a) a[1] + a[2])
  expect_equal(feat.object$feature.names, c("a1", "a2"))
  expect_equal(feat.object$objective.name, "y")

  y = extractObjective(feat.object)
  expect_true(all(y == 11L))
})

test_that("Derive Boundaries from Initial Sample", {
  ctrl = list(
    init_sample.lower = c(-4, 2, 1),
    init_sample.upper = c(10, 12, 2)
  )
  X = createInitialSample(n.obs = 400, dim = 3, control = ctrl)
  y = rnorm(400)
  feat.object = createFeatureObject(X = X, y = y)
  expect_identical(feat.object$lower, ctrl$init_sample.lower)
  expect_identical(feat.object$upper, ctrl$init_sample.upper)

  feat.object2 = createFeatureObject(X = X, y = y, lower = -10)
  expect_true(any(feat.object2$lower != ctrl$init_sample.lower))
  expect_identical(feat.object2$upper, ctrl$init_sample.upper)

  feat.object3 = createFeatureObject(X = X, y = y, lower = c(-5, 2, 1), upper = 13)
  expect_true(any(feat.object3$lower != ctrl$init_sample.lower))
  expect_true(any(feat.object3$upper != ctrl$init_sample.upper))
})
