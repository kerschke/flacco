context("Base: Create Feature Object")

test_that("Basic FeatureObject ", {
  feat.object = createFeatureObject(iris, objective = "Species")
  expect_true(feat.object$minimize)
  expect_equal(feat.object$lower, apply(iris[, -5], 2, min))
  expect_equal(feat.object$upper, apply(iris[, -5], 2, max))
  expect_equal(feat.object$dim, ncol(iris) - 1L)
  expect_equal(feat.object$n.obs, nrow(iris))
  expect_equal(feat.object$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(feat.object$objective.name, "Species")
  expect_equal(feat.object$blocks, rep(1L, ncol(iris) - 1L))
  expect_false(feat.object$allows.cellmapping)
  expect_identical(feat.object$init.grid, data.frame(iris, cell.ID = 1L))
  expect_identical(feat.object$cell.centers, 
    data.frame(t(colMeans(rbind(apply(iris[, -5], 2, max), 
      apply(iris[, -5], 2, min)))), cell.ID = 1L))
  expect_identical(feat.object$cell.size, 
    apply(iris[, -5], 2, function(x) diff(range(x))))
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

test_that("Erroneous FeatureObject with upper < lower", {
  expect_error(createFeatureObject(iris, objective = "Species", lower = -20, upper = -30))
})

test_that("Erroneous FeatureObject with lower > upper", {
  expect_error(createFeatureObject(iris, objective = "Species", lower = 30))
})

test_that("Erroneous FeatureObject without defining an objective", {
  expect_error(createFeatureObject(iris))
})

test_that("Cellmapping-enabled FeatureObject", {
  feat.object = createFeatureObject(iris, objective = "Species", blocks = 5)
  expect_true(feat.object$allows.cellmapping)
  expect_equal(dim(feat.object$init.grid), dim(iris) + c(0L, 1L))
  expect_true(all(feat.object$init.grid$cell.ID <= 5^4))  
})

test_that("Cellmapping-enabled FeatureObject with a single cell", {
  feat.object = createFeatureObject(iris, objective = "Species", blocks = 1)
  expect_equal(feat.object$init.grid$cell.ID, rep(1L, nrow(iris)))
  expect_is(feat.object, class = "FeatureObject")
})

test_that("Initial data in FeatureObject can be composed from X and y", {
  feat.object = createFeatureObject(X = iris[, -5], y = iris$Species)
  expect_equal(feat.object$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(feat.object$objective.name, "y")
})

test_that("Objective values in FeatureObject can be computed from X and fun", {
  values = matrix(c(1:10, 10:1), ncol = 2)
  values = data.frame(values)
  colnames(values) = c("a1", "a2")
  
  feat.object = createFeatureObject( X = values, fun = function(a) { a[1]+a[2] } )
  expect_equal(feat.object$feature.names, c("a1", "a2"))
  expect_equal(feat.object$objective.name, "y")
  
  y = extractObjective(feat.object)
  expect_true(all(y == 11))
})

test_that("Erroneous FeatureObject with differing rows of features and variables", {
  expect_error(
    createFeatureObject( X = matrix(1, nrow=2), y = c(1) )
  )
})

test_that("Erroneous FeatureObject with differing dimensions for the lower bound", {
  expect_error(
    createFeatureObject(X = matrix(1, nrow = 2, ncol = 3), y = rep(1, 2),
      lower = c(-1, -1), # two dimensions; expected three
      upper = 10, blocks = 5)
  )
})

test_that("Erroneous FeatureObject with differing dimensions for the upper bound", {
  expect_error(
    createFeatureObject(X = matrix(1, nrow=2, ncol=3), y = rep(1, 2),
      lower = -1, upper = c(10, 10), # two dimensions; expected three
      blocks = 5)
  )
})

test_that("Erroneous FeatureObject with differing dimensions for the lower bound", {
  expect_error(
    createFeatureObject(X = matrix(1, nrow = 2, ncol = 3), y = rep(1, 2),
      lower = -1, upper = 10, blocks = c(5, 5)) # two dimensions; expected three
  )
})
