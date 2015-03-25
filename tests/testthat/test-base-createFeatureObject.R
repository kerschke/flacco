context("createFeatureObject")

test_that("Basic FeatureObject ", {
  featobj1 = createFeatureObject(iris, objective = "Species")
  expect_true(featobj1$minimize)
  expect_equal(featobj1$lower, apply(iris[, -5], 2, min))
  expect_equal(featobj1$upper, apply(iris[, -5], 2, max))
  expect_equal(featobj1$dim, ncol(iris) - 1L)
  expect_equal(featobj1$n.obs, nrow(iris))
  expect_equal(featobj1$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(featobj1$objective.name, "Species")
  expect_equal(featobj1$blocks, rep(NA, ncol(iris) - 1L))
  expect_false(featobj1$allows.cellmapping)
  expect_null(featobj1$init.grid)
  expect_null(featobj1$cell.centers)
  expect_null(featobj1$cell.size)
  expect_equal(featobj1$env$init, iris)
  expect_is(featobj1, class = "FeatureObject")
})
 
test_that("FeatureObject for a maximization problem", {
  featobj2 = createFeatureObject(iris, objective = "Species", minimize = FALSE)
  expect_false(featobj2$minimize)
})

  
test_that("FeatureObject with a custom lower bound", {
  featobj3 = createFeatureObject(iris, objective = "Species", lower = -20)
  expect_equal(featobj3$lower, rep(-20, ncol(iris) - 1L))
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
  featobj4 = createFeatureObject(iris, objective = "Species", blocks = 5)
  expect_true(featobj4$allows.cellmapping)
  expect_equal(dim(featobj4$init.grid), dim(iris) + c(0L, 1L))
  expect_true(all(featobj4$init.grid$cell.ID <= 5^4))  
})
  
test_that("Cellmapping-enabled FeatureObject with a single cell", {
  featobj5 = createFeatureObject(iris, objective = "Species", blocks = 1)
  expect_equal(featobj5$init.grid$cell.ID, rep(1L, nrow(iris)))
  expect_is(featobj5, class = "FeatureObject")
})

test_that("Initial data in FeatureObject can be composed from X and y", {
  featobj6 = createFeatureObject( X = iris[, -5], y = iris$Species )
  expect_equal(featobj6$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(featobj6$objective.name, "y")
})

test_that("Objective values in FeatureObject can be computed from X and fun", {
  values = matrix( c(seq(1,10), seq(10,1)), ncol = 2 )
  values = data.frame(values)
  colnames(values) = c("a1", "a2")
  
  featobj7 = createFeatureObject( X = values, fun = function(a) { a[1]+a[2] } )
  expect_equal(featobj7$feature.names, c("a1", "a2"))
  expect_equal(featobj7$objective.name, "y")
  
  y = extractObjective(featobj7)
  expect_true(all(y == 11))
})

test_that("Erroneous FeatureObject with differing rows of features and variables", {
  expect_error(
    createFeatureObject( X = matrix(1, nrow=2), y = c(1) )
  )
})

