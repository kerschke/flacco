sockettest = function() {

  expect_error(parallelStartSocket(cpus=2, socket.hosts="localhost"), "You cannot set both")
  parallelStop()

  parallelStartSocket(2)
  partest1()
  parallelStop()

  # check with host args as strings too
  parallelStartSocket(socket.hosts=c("localhost", "localhost"))
  partest1()
  parallelStop()

  parallelStartSocket(2, logging=TRUE, storagedir=tempdir())
  partest2(tempdir())
  parallelStop()

  parallelStartSocket(2)
  partest3()
  parallelStop()

  parallelStartSocket(2)
  partest4(slave.error.test=TRUE)
  parallelStop()

  parallelStartSocket(2)
  partest5()
  parallelStop()

  parallelStartSocket(2)
  partest6(slave.error.test=TRUE)
  parallelStop()
}
