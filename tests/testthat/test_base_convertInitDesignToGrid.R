context("convertInitDesignToGrid")

test_that("ConvertInitDesignToGrid", {
  expect_equal(convertInitDesignToGrid(iris, lower = -10, upper = 10, blocks = 1)$cell.ID, rep(1L, 150L))
  grid = convertInitDesignToGrid(iris, blocks = 2)
  expect_true(all(grid$cell.ID >= 1L))
  expect_true(all(grid$cell.ID <= 2^4))
})
