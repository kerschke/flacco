context("convertInitDesignToGrid")

test_that("Converting an initial design into a grid with one observation per cell", {
  X = expand.grid( -9.5:9.5, -9.5:9.5, -9.5:9.5 )
  y = apply(X = X, MARGIN = 1, FUN = function(x) sum(x^2))
  init = data.frame(X, y = y)
  
  # (2) compute the cell mapping grid
  grid = convertInitDesignToGrid(init = init, lower = -10, upper = 10, blocks = 20)
  
  # cell.IDs are in expected range
  expect_true( testNumeric(grid$cell.ID, lower=1, upper=20^3) )
  
  # only one observation per cell
  expect_true( testVector(grid$cell.ID, any.missing = FALSE, unique = TRUE) )
  
})

test_that("Converting an initial design into a grid with two observations per cell", {
  X = expand.grid( -9.5:9.5, -9.5:9.5, -9.5:9.5 )
  y = apply(X = X, MARGIN = 1, FUN = function(x) sum(x^2))
  init = data.frame(X, y = y)
  
  # (2) compute the cell mapping grid
  grid = convertInitDesignToGrid(init = init, lower = -10, upper = 10, blocks = c(10, 20, 20))
  
  # cell.IDs are in expected range
  expect_true( testNumeric(grid$cell.ID, lower=1, upper=20^3) )
  
  # exactly two observations per cell
  freq = table(grid$cell.ID)
  expect_true( all(freq == 2) )
  
})


test_that("Converting an initial design into a grid, assuming a default block count of 10 in each dimension", {
  # (1) create an initial design:
  X = expand.grid( seq(-9.5, 9.5, 2), seq(-9.5, 9.5, 2), seq(-9.5, 9.5, 2) )
  y = apply(X = X, MARGIN = 1, FUN = function(x) sum(x^2))
  init = data.frame(X, y = y)
  
  # (2) compute the cell mapping grid
  grid = convertInitDesignToGrid(init = init, lower = -10, upper = 10) # default: blocks = 10L
  
  # cell.IDs are in expected range
  expect_true( testNumeric(grid$cell.ID, lower=1, upper=20^3) )
  
  # only one observation per cell
  expect_true( testVector(grid$cell.ID, any.missing = FALSE, unique = TRUE) )
})
