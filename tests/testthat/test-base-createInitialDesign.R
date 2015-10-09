context("Base: Create Initial Design")

test_that("Basic Initial Designs", {
  # random
  X = createInitialDesign(200, 4)
  expect_matrix(X, mode = "numeric", any.missing = FALSE, nrows = 200, ncols = 4)
  expect_true(all(X <= 1))
  expect_true(all(X >= 0))

  # lhs
  X = createInitialDesign(200, 4, control = list(init_design.type = "lhs"))
  expect_matrix(X, mode = "numeric", any.missing = FALSE, nrows = 200, ncols = 4)
  expect_true(all(X <= 1))
  expect_true(all(X >= 0))
})

test_that("Detect Incorrect Input", {
  expect_error(createInitialDesign(200, 4, list(init_design.lower = 4)))
  expect_error(createInitialDesign(200, 4, list(init_design.upper = -4)))
  expect_error(createInitialDesign(200, 4, list(init_design.lower = c(0, -1, -2, 2))))
  expect_error(createInitialDesign(200, 4, list(init_design.lower = c(0, -1))))
  expect_error(createInitialDesign(200, 4, list(init_design.upper = c(3, 71, 12))))
})

test_that("Create Initial Design with Custom Bounds", {
  ctrl = list(init_design.lower = c(3, -5, 0), init_design.upper = 6:8)
  X = createInitialDesign(150, 3, ctrl)
  expect_true(all(X[,1] >= 3))
  expect_true(all(X[,2] >= -5))
  expect_true(all(X[,3] >= 0))
  expect_true(all(X[,1] <= 6))
  expect_true(all(X[,2] <= 7))
  expect_true(all(X[,3] <= 8))
  expect_matrix(X, mode = "numeric", any.missing = FALSE, nrows = 150, ncols = 3)
})