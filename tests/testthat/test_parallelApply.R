context("parallel apply")

test_that("parallelLapply", {
  parallelStart(mode="local")

  ys = parallelLapply(1:2, identity)
  expect_equal(ys, as.list(1:2))
  
  parallelStop()
})


test_that("parallelSapply", {
  parallelStart(mode="local")
  
  xs = c("a", "b")
  ys = parallelSapply(xs, identity, use.names=FALSE)
  expect_equal(ys, xs)
  ys = parallelSapply(xs, identity, use.names=TRUE)
  expect_equal(ys, setNames(xs, xs))
  
  expect_equal(
    parallelSapply(1:2, identity),
    sapply(1:2, identity)
  )
  
  parallelStop()
})
