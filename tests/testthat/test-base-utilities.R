context("Utility Functions (main and GCM)")

test_that("findAllNeighbours", {
  h = c(1, 1)
  N = rep(h[1], length(h))
  expect_equal(
    findAllNeighbours(N, c(1,1), h),
    as.matrix( expand.grid(c(0,1,2), c(0,1,2)) )
  )
  
  h = c(2,1)
  N = rep(h[1], length(h))
  expect_equal(
    findAllNeighbours(N, c(2,2), h),
    as.matrix( expand.grid(c(1,2,3), c(1,2,3)) )
  )
    
  h = c(1, 1, 1)
  N = rep(h[1], length(h))
  expect_equal(
    findAllNeighbours(N, c(5,5,5), h),
    as.matrix( expand.grid( c(4,5,6), c(4,5,6), c(4,5,6) ) )
  )
})

test_that("isNeighbourInvalid", {
  N = c(10, 10, 10)
  
  # neighbour of itself
  current = c(1, 1, 1)
  expect_true( isNeighbourInvalid(current, current, N) )
  
  # out-of-bounds => too low
  expect_true( isNeighbourInvalid(current, c(0, 1, 1), N) )
  expect_true( isNeighbourInvalid(current, c(1, 0, 1), N) )
  expect_true( isNeighbourInvalid(current, c(1, 1, 0), N) )
  
  # out-of-bounds => too high
  expect_true( isNeighbourInvalid(current, c(11, 1,   1), N) )
  expect_true( isNeighbourInvalid(current, c(1,  11,  1), N) )
  expect_true( isNeighbourInvalid(current, c(1,  1,  11), N) )
  
  # within-bounds, but not itself
  expect_false( isNeighbourInvalid(c(5,5,5), c(1,  1,  1 ), N) )
  expect_false( isNeighbourInvalid(current,  c(1,  1,  2 ), N) )
  expect_false( isNeighbourInvalid(current,  c(10, 10, 10), N) )
  expect_false( isNeighbourInvalid(current,  c(1,  5,  6 ), N) )
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
  lb = c(-5, -5)
  h = c(2, 1)
  expect_equal(ztox(c(1, 1), h, lb), c(-4.0, -4.5))
  expect_equal(ztox(c(10, 5), h, lb), c(14, -0.5))
  expect_equal(ztox(c(10, 10), h, lb), c(14, 4.5))
  
  lb = c(-5, -5)
  h = c(1, 2)
  expect_equal(ztox(c(1, 1), h, lb), c(-4.5, -4.0))
  expect_equal(ztox(c(10, 5), h, lb), c(4.5, 4.0))
  expect_equal(ztox(c(10, 10), h, lb), c(4.5, 14))
  
  lb = c(-5, -5, -5, -5)
  h = c(2, 2, 2, 2)
  expect_equal(ztox(c(4,3,9,8), h, lb), c(2, 0, 12, 10))
  
})

test_that("selectMin", {
  x = c(5, 3, 3, 5, 6, 10, 10, 3, 5.4)
  ind = which(x == min(x))
  expect_is(selectMin(x), "integer")
  expect_true( testNumber(selectMin(x), lower = min(ind), upper = max(ind)) )
  expect_true(selectMin(x) %in% ind)
  expect_true( testNumber(selectMin(x, tie.breaker = "sample"), 
    lower = min(ind), upper = max(ind)) )
  expect_true(selectMin(x, tie.breaker = "sample") %in% ind)
  expect_identical(selectMin(x, tie.breaker = "first"), min(ind))
  expect_identical(selectMin(x, tie.breaker = "last"), max(ind))
})


test_that("selectMax", {
  x = c(5, 3, 3, 5, 6, 10, 10, 3, 5.4)
  ind = which(x == max(x))
  expect_is(selectMax(x), "integer")
  expect_true( testNumber(selectMax(x), lower = min(ind), upper = max(ind)) )
  expect_true(selectMax(x) %in% ind)
  expect_true( testNumber(selectMax(x, tie.breaker = "sample"), 
    lower = min(ind), upper = max(ind)) )
  expect_true(selectMax(x, tie.breaker = "sample") %in% ind)
  expect_identical(selectMax(x, tie.breaker = "first"), min(ind))
  expect_identical(selectMax(x, tie.breaker = "last"), max(ind))
})
