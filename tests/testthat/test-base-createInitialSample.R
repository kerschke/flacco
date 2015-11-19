context("Base: Create Initial Sample")

test_that("Basic Initial Samples", {
  # random
  X = createInitialSample(200, 4)
  expect_matrix(X, mode = "numeric", any.missing = FALSE, nrows = 200, ncols = 4)
  expect_true(all(X <= 1))
  expect_true(all(X >= 0))

  # lhs
  X = createInitialSample(200, 4, control = list(init_sample.type = "lhs"))
  expect_matrix(X, mode = "numeric", any.missing = FALSE, nrows = 200, ncols = 4)
  expect_true(all(X <= 1))
  expect_true(all(X >= 0))
})

test_that("Detect Incorrect Input", {
  expect_error(createInitialSample(200, 4, list(init_sample.lower = 4)))
  expect_error(createInitialSample(200, 4, list(init_sample.upper = -4)))
  expect_error(createInitialSample(200, 4, list(init_sample.lower = c(0, -1, -2, 2))))
  expect_error(createInitialSample(200, 4, list(init_sample.lower = c(0, -1))))
  expect_error(createInitialSample(200, 4, list(init_sample.upper = c(3, 71, 12))))
})

test_that("Create Initial Sample with Custom Bounds", {
  ctrl = list(init_sample.lower = c(3, -5, 0), init_sample.upper = 6:8)
  X = createInitialSample(150, 3, ctrl)
  expect_true(all(X[,1] >= 3))
  expect_true(all(X[,2] >= -5))
  expect_true(all(X[,3] >= 0))
  expect_true(all(X[,1] <= 6))
  expect_true(all(X[,2] <= 7))
  expect_true(all(X[,3] <= 8))
  expect_matrix(X, mode = "numeric", any.missing = FALSE, nrows = 150, ncols = 3)
})