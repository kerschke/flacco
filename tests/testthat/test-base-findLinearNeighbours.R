context("Base: Find Linear Neighbours")

test_that("Without Diagonal", {
  cell.ids = c(4, 5, 9, 29)
  blocks = c(5, 4, 7)
  # (1) linear neighbours
  nbs = findLinearNeighbours(cell.ids = cell.ids, blocks = blocks, diag = FALSE)
  expect_equal(length(nbs), 6)
  expect_is(nbs, class = "list")
  expect_equal(as.character(sapply(nbs, class)), rep("integer", 6))
  expect_equal(findLinearNeighbours(cell.ids = cell.ids[1], blocks = blocks),
    list(3:5))
  expect_equal(findLinearNeighbours(cell.ids = cell.ids[2], blocks = blocks), 
    list())
  expect_equal(findLinearNeighbours(cell.ids = cell.ids[3], blocks = blocks), 
    list(8:10, c(4, 9, 14)))
  expect_equal(findLinearNeighbours(cell.ids = cell.ids[4], blocks = blocks), 
    list(28:30, c(24, 29, 34), c(9, 29, 49)))
})


test_that("With Diagonal", {
  cell.ids = c(4, 5, 9, 29)
  blocks = c(5, 4, 7)
  # (1) linear neighbours
  nbs = findLinearNeighbours(cell.ids = cell.ids, blocks = blocks, diag = TRUE)
  expect_equal(length(nbs), 18)
  expect_is(nbs, class = "list")
  expect_equal(as.character(sapply(nbs, class)), rep("integer", 18))
  expect_equal(findLinearNeighbours(cell.ids[1], blocks, TRUE),
    list(3:5))
  expect_equal(findLinearNeighbours(cell.ids[2], blocks, TRUE), 
    list())
  expect_equal(findLinearNeighbours(cell.ids[3], blocks, TRUE),
    list(c(3, 9, 15), c(4, 9, 14), c(5, 9, 13), 8:10))
  expect_equal(findLinearNeighbours(cell.ids[4], blocks, TRUE), 
    list(c(3, 29, 55), c(4, 29, 54), c(5, 29, 53),
      c(8, 29, 50), c(9, 29, 49), c(10, 29, 48),
      c(13, 29, 45), c(14, 29, 44), c(15, 29, 43),
      c(23, 29, 35), c(24, 29, 34), c(25, 29, 33),
      28:30))
})
