context("Base: Find Linear Neighbours")

test_that("Without Diagonal", {
  cell.ids = c(4L, 5L, 9L, 29L)
  blocks = c(5L, 4L, 7L)

  # (1) Linear Neighbours
  nbs = findLinearNeighbours(cell.ids = cell.ids, blocks = blocks, diag = FALSE)
  expect_equal(length(nbs), 6L)
  expect_list(nbs)
  expect_equal(as.character(sapply(nbs, class)), rep("integer", 6L))
  expect_equal(findLinearNeighbours(cell.ids[1], blocks), list(3:5))
  expect_equal(findLinearNeighbours(cell.ids[2], blocks), list())
  expect_equal(findLinearNeighbours(cell.ids[3], blocks), list(8:10, c(4L, 9L, 14L)))
  expect_equal(findLinearNeighbours(cell.ids[4], blocks),
    list(28:30, c(24L, 29L, 34L), c(9L, 29L, 49L)))
})

test_that("With Diagonal", {
  cell.ids = c(4L, 5L, 9L, 29L)
  blocks = c(5L, 4L, 7L)
  # (1) Linear Neighbours
  nbs = findLinearNeighbours(cell.ids = cell.ids, blocks = blocks, diag = TRUE)
  expect_equal(length(nbs), 18L)
  expect_list(nbs)
  expect_equal(as.character(sapply(nbs, class)), rep("integer", 18L))
  expect_equal(findLinearNeighbours(cell.ids[1], blocks, TRUE), list(3:5))
  expect_equal(findLinearNeighbours(cell.ids[2], blocks, TRUE), list())
  expect_equal(findLinearNeighbours(cell.ids[3], blocks, TRUE),
    list(c(3L, 9L, 15L), c(4L, 9L, 14L), c(5L, 9L, 13L), 8:10))
  expect_equal(findLinearNeighbours(cell.ids[4], blocks, TRUE), 
    list(c(3L, 29L, 55L), c(4L, 29L, 54L), c(5L, 29L, 53L),
      c(8L, 29L, 50L), c(9L, 29L, 49L), c(10L, 29L, 48L),
      c(13L, 29L, 45L), c(14L, 29L, 44L), c(15L, 29L, 43L),
      c(23L, 29L, 35L), c(24L, 29L, 34L), c(25L, 29L, 33L),
      28:30))
})
