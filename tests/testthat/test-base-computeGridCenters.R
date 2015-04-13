context("Base: Compute Grid Centers")

test_that("computeGridCenters", {
  bl = c(1, 2, 4, 5, 10, 20)
  grid.centers = computeGridCenters(lower = -10, upper = 10, blocks = bl)
  expect_equal(ncol(grid.centers), 6L + 1L)
  expect_equal(nrow(grid.centers), prod(bl))
  expect_equal(as.integer(sapply(grid.centers, function(x) length(unique(x)))), 
    c(bl, prod(bl)))
  expect_equal(sort(unique(grid.centers[, 1])), 0L)
  expect_equal(sort(unique(grid.centers[, 2])), c(-5L, 5L))
  expect_equal(sort(unique(grid.centers[, 3])), c(-7.5, -2.5, 2.5, 7.5))
  expect_equal(sort(unique(grid.centers[, 4])), c(-8, -4, 0, 4, 8))
  expect_equal(sort(unique(grid.centers[, 5])), seq(-9, 9, 2))
  expect_equal(sort(unique(grid.centers[, 6])), seq(-9.5, 9.5, 1))
})
