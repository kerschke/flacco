context("local mode")

test_that("local mode", {
  parallelStartLocal()
  partest1()
  partest3()
  partest4(slave.error.test=FALSE)
  partest5()
  partest6(slave.error.test=FALSE)
  parallelStop()
})
