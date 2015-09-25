context("Base: Convert Initial Design into a Grid")

test_that("ConvertInitDesignToGrid", {
  expect_equal(convertInitDesignToGrid(
    iris, lower = -10L, upper = 10L, blocks = 1L)$cell.ID, rep(1L, 150L))
  grid = convertInitDesignToGrid(iris, blocks = 2L)
  expect_true(all(grid$cell.ID >= 1L))
  expect_true(all(grid$cell.ID <= 2^4))
})

test_that("One Observation per Cell", {
  X = expand.grid(-9.5:9.5, -9.5:9.5, -9.5:9.5)

  # (2) compute the cell mapping grid
  grid = convertInitDesignToGrid(init = data.frame(X, y = rowSums(X^2)),
    lower = -10L, upper = 10L, blocks = 20L)

  # cell.IDs are in expected range
  expect_true(testNumeric(grid$cell.ID, lower = 1L, upper = as.integer(20^3)))

  # only one observation per cell
  expect_true(testVector(grid$cell.ID, any.missing = FALSE, unique = TRUE))  
})

test_that("Two Observations per Cell", {
  X = expand.grid(-9.5:9.5, -9.5:9.5, -9.5:9.5)

  # (2) compute the cell mapping grid
  grid = convertInitDesignToGrid(init = data.frame(X, y = rowSums(X^2)),
    lower = -10L, upper = 10L, blocks = c(10L, 20L, 20L))

  # cell.IDs are in expected range
  expect_true(testNumeric(grid$cell.ID, lower = 1L, upper = as.integer(20^3)))

  # exactly two observations per cell
  freq = table(grid$cell.ID)
  expect_true(all(freq == 2L))
})

test_that("Assume 10 Blocks per Dimension", {
  # (1) create an initial design:
  X = expand.grid(seq(-9.5, 9.5, 2), seq(-9.5, 9.5, 2), seq(-9.5, 9.5, 2))

  # (2) compute the cell mapping grid
  grid = convertInitDesignToGrid(init = data.frame(X, y = rowSums(X^2)),
    lower = -10L, upper = 10L) # default: blocks = 10L

  # cell.IDs are in expected range
  expect_true(testNumeric(grid$cell.ID, lower = 1L, upper = as.integer(20^3)))

  # only one observation per cell
  expect_true(testVector(grid$cell.ID, any.missing = FALSE, unique = TRUE))
})
