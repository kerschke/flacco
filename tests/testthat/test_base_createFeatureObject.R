context("createFeatureObject")

test_that("createFeatureObject", {
  featobj1 = createFeatureObject(iris, objective = "Species")
  expect_true(featobj1$minimize)
  expect_equal(featobj1$lower, apply(iris[, -5], 2, min))
  expect_equal(featobj1$upper, apply(iris[, -5], 2, max))
  expect_equal(featobj1$dim, ncol(iris) - 1L)
  expect_equal(featobj1$n.obs, nrow(iris))
  expect_equal(featobj1$feature.names, setdiff(colnames(iris), "Species"))
  expect_equal(featobj1$objective.name, "Species")
  expect_equal(featobj1$blocks, rep(NA, ncol(iris) - 1L))
  expect_true(!featobj1$allows.cellmapping)
  expect_null(featobj1$init.grid)
  expect_null(featobj1$cell.centers)
  expect_null(featobj1$cell.size)
  expect_equal(featobj1$env$init, iris)
  expect_is(featobj1, class = "FeatureObject")
  
  featobj2 = createFeatureObject(iris, objective = "Species", minimize = FALSE)
  expect_true(!featobj2$minimize)
  
  featobj3 = createFeatureObject(iris, objective = "Species", lower = -20)
  expect_equal(featobj3$lower, rep(-20, ncol(iris) - 1L))
  
  expect_error(createFeatureObject(iris, objective = "Species", lower = -20, upper = -30))
  expect_error(createFeatureObject(iris, objective = "Species", lower = 30))
  expect_error(createFeatureObject(iris))
  
  featobj4 = createFeatureObject(iris, objective = "Species", blocks = 5)
  expect_true(featobj4$allows.cellmapping)
  expect_equal(dim(featobj4$init.grid), dim(iris) + c(0L, 1L))
  expect_true(all(featobj4$init.grid$cell.ID <= 5^4))  
  
  featobj5 = createFeatureObject(iris, objective = "Species", blocks = 1)
  expect_equal(featobj5$init.grid$cell.ID, rep(1L, nrow(iris)))
  expect_is(featobj5, class = "FeatureObject")
})
