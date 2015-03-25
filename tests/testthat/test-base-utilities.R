context("Utility Functions (main and GCM)")

test_that("findAllNeighbours", {
  # TODO
})

test_that("isNeighbourInvalid", {
  # TODO
})

test_that("ztocell", {
  N = c(10, 10)
  expect_equal(ztocell(c(1, 1), N), 1)
  expect_equal(ztocell(c(10, 1), N), 10)
  expect_equal(ztocell(c(1, 2), N), 11)
  expect_equal(ztocell(c(9, 10), N), 99)
  
  N = c(5, 10)
  expect_equal(ztocell(c(1, 1), N), 1)
  expect_equal(ztocell(c(5, 2), N), 10)
  expect_equal(ztocell(c(1, 3), N), 11)
  expect_equal(ztocell(c(5, 10), N), 50)
  
  N = c(4,4,4,4)
  expect_equal(ztocell(c(4,3,2,2), N), 92)
  
})

test_that("celltoz", {
  N = c(10, 10)
  expect_equal(celltoz(1, N), c(1, 1))
  expect_equal(celltoz(10, N), c(10, 1))
  expect_equal(celltoz(11, N), c(1, 2))
  expect_equal(celltoz(99, N), c(9, 10))
  
  expect_equal(celltoz(101, N), c(1, 1)) # Wraparound
  
  N = c(5, 10)
  expect_equal(celltoz(1, N), c(1, 1))
  expect_equal(celltoz(10, N), c(5, 2))
  expect_equal(celltoz(11, N), c(1, 3))
  
  N = c(4,4,4,4)
  expect_equal(celltoz(92, N), c(4,3,2,2))
  expect_equal(celltoz(348, N), c(4,3,2,2)) # Wraparound
  
})

test_that("ztox", {
  # TODO
})