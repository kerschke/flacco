context("Base: Compute Grid Centers")

test_that("computeGridCenters", {
  bl = c(1L, 2L, 4L, 5L, 10L, 20L)
  grid.centers = computeGridCenters(lower = -10L, upper = 10L, blocks = bl)
  expect_equal(ncol(grid.centers), 6L + 1L)
  expect_equal(nrow(grid.centers), prod(bl))
  expect_equal(as.integer(sapply(grid.centers, function(x) length(unique(x)))), 
    c(bl, prod(bl)))
  expect_equal(sort(unique(grid.centers[, 1])), 0L)
  expect_equal(sort(unique(grid.centers[, 2])), c(-5L, 5L))
  expect_equal(sort(unique(grid.centers[, 3])), c(-7.5, -2.5, 2.5, 7.5))
  expect_equal(sort(unique(grid.centers[, 4])), c(-8L, -4L, 0L, 4L, 8L))
  expect_equal(sort(unique(grid.centers[, 5])), seq(-9L, 9L, 2L))
  expect_equal(sort(unique(grid.centers[, 6])), seq(-9.5, 9.5, 1))
})
