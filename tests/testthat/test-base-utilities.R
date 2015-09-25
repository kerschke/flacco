context("Base: Utilities")

# test_that("findAllNeighbours", {
#   h = c(1L, 1L)
#   N = rep(h[1], length(h))
#   expect_equal(findAllNeighbours(N, c(1L, 1L), h),
#     as.matrix(expand.grid(0:2, 0:2)))
# 
#   h = c(2L, 1L)
#   N = rep(h[1], length(h))
#   expect_equal(findAllNeighbours(N, c(2L, 2L), h),
#     as.matrix(expand.grid(1:3, 1:3)))
# 
#   h = c(1L, 1L, 1L)
#   N = rep(h[1], length(h))
#   expect_equal(findAllNeighbours(N, rep(5L, 3L), h),
#     as.matrix(expand.grid(4:6, 4:6, 4:6)))
# })

test_that("isInvalidNeighbour", {
  N = rep(10L, 3L)

  # neighbour of itself
  current = rep(1L, 3L)
  expect_true(isInvalidNeighbour(current, current, N))

  # out-of-bounds => too low
  expect_true(isInvalidNeighbour(current, c(0L, 1L, 1L), N))
  expect_true(isInvalidNeighbour(current, c(1L, 0L, 1L), N))
  expect_true(isInvalidNeighbour(current, c(1L, 1L, 0L), N))

  # out-of-bounds => too high
  expect_true(isInvalidNeighbour(current, c(11L, 1L, 1L), N))
  expect_true(isInvalidNeighbour(current, c(1L, 11L, 1L), N))
  expect_true(isInvalidNeighbour(current, c(1L, 1L, 11L), N))

  # within-bounds, but not itself
  expect_false(isInvalidNeighbour(rep(5L, 3L), rep(1L, 3L), N))
  expect_false(isInvalidNeighbour(current, c(1L, 1L, 2L), N))
  expect_false(isInvalidNeighbour(current, rep(10L, 3L), N))
  expect_false(isInvalidNeighbour(current, c(1L, 5L, 6L), N))
})

test_that("ztocell", {
  N = c(10L, 10L)
  expect_equal(ztocell(c(1L, 1L), N), 1L)
  expect_equal(ztocell(c(10L, 1L), N), 10L)
  expect_equal(ztocell(c(1L, 2L), N), 11L)
  expect_equal(ztocell(c(9L, 10L), N), 99L)

  N = c(5L, 10L)
  expect_equal(ztocell(c(1L, 1L), N), 1L)
  expect_equal(ztocell(c(5L, 2L), N), 10L)
  expect_equal(ztocell(c(1L, 3L), N), 11L)
  expect_equal(ztocell(c(5L, 10L), N), 50L)

  N = rep(4L, 4L)
  expect_equal(ztocell(c(4L, 3L, 2L, 2L), N), 92L)
})

test_that("celltoz", {
  N = c(10L, 10L)
  expect_equal(celltoz(1L, N), c(1L, 1L))
  expect_equal(celltoz(10L, N), c(10L, 1L))
  expect_equal(celltoz(11L, N), c(1L, 2L))
  expect_equal(celltoz(99L, N), c(9L, 10L))
  expect_null(celltoz(101L, N))

  N = c(5L, 10L)
  expect_equal(celltoz(1L, N), c(1L, 1L))
  expect_equal(celltoz(10L, N), c(5L, 2L))
  expect_equal(celltoz(11L, N), c(1L, 3L))

  N = rep(4L, 4L)
  expect_equal(celltoz(92L, N), c(4L, 3L, 2L, 2L))
  expect_null(celltoz(348L, N))
})

# test_that("ztox", {
#   lb = c(-5L, -5L)
#   h = c(2L, 1L)
#   expect_equal(ztox(c(1L, 1L), h, lb), c(-4.0, -4.5))
#   expect_equal(ztox(c(10L, 5L), h, lb), c(14, -0.5))
#   expect_equal(ztox(c(10L, 10L), h, lb), c(14, 4.5))
# 
#   lb = -c(5L, 5L)
#   h = c(1L, 2L)
#   expect_equal(ztox(c(1L, 1L), h, lb), c(-4.5, -4.0))
#   expect_equal(ztox(c(10L, 5L), h, lb), c(4.5, 4.0))
#   expect_equal(ztox(c(10L, 10L), h, lb), c(4.5, 14))
#   
#   lb = rep(-5L, 4L)
#   h = rep(2L, 4L)
#   expect_equal(ztox(c(4L, 3L, 9L, 8L), h, lb), c(2L, 0L, 12L, 10L))
# })

test_that("selectMin", {
  x = c(5, 3, 3, 5, 6, 10, 10, 3, 5.4)
  ind = which(x == min(x))
  expect_is(selectMin(x), "integer")
  expect_true(testNumber(selectMin(x), lower = min(ind), upper = max(ind)))
  expect_true(selectMin(x) %in% ind)
  expect_true(testNumber(selectMin(x, tie.breaker = "sample"), 
    lower = min(ind), upper = max(ind)))
  expect_true(selectMin(x, tie.breaker = "sample") %in% ind)
  expect_identical(selectMin(x, tie.breaker = "first"), min(ind))
  expect_identical(selectMin(x, tie.breaker = "last"), max(ind))
})

test_that("selectMax", {
  x = c(5, 3, 3, 5, 6, 10, 10, 3, 5.4)
  ind = which(x == max(x))
  expect_is(selectMax(x), "integer")
  expect_true(testNumber(selectMax(x), lower = min(ind), upper = max(ind)))
  expect_true(selectMax(x) %in% ind)
  expect_true(testNumber(selectMax(x, tie.breaker = "sample"), 
    lower = min(ind), upper = max(ind)))
  expect_true(selectMax(x, tie.breaker = "sample") %in% ind)
  expect_identical(selectMax(x, tie.breaker = "first"), min(ind))
  expect_identical(selectMax(x, tie.breaker = "last"), max(ind))
})
