# a test for normal functionality of mapping an its options
partest1 = function() {
  # normal lapply
  expect_equal(parallelMap(identity, 1), list(1))
  expect_equal(parallelMap(identity, 1:2), list(1, 2))
  y = c("a", "b"); names(y) = y
  # simplify and names
  expect_equal(parallelMap(identity, y, simplify=TRUE, use.names=TRUE), y)

  # more.args and mapping over 2 vectors
  f = function(x,y) x+y
  expect_equal(parallelMap(f, 1:2, more.args=list(y=1)), list(2, 3))
  expect_equal(parallelMap(f, 1:2, 2:3), list(3, 5))

  # with level
  expect_equal(parallelMap(identity, 1:2), list(1, 2), level="foo")
}

# test that log files are correctly generated
partest2 = function(log.dir) {

  # do log files exist under correct path / name?
  check.exists = function(iter, n) {
    fp = file.path(log.dir, sprintf("parallelMap_logs_%03i", iter))
    sapply(seq_len(n), function(i)
      expect_true(file.exists(file.path(fp, sprintf("%05i.log", i)))))
  }

  # do log files contain the printed output fromn the slave?
  check.contains = function(iter, xs)  {
    fp = file.path(log.dir, sprintf("parallelMap_logs_%03i", iter))
    Map(function(i, x) {
      s = readLines(file.path(fp, sprintf("%05i.log", i)))
      s = collapse(s, sep="\n")
      expect_true(grep(x, s) == 1)
    }, seq_along(xs), xs)
  }

  parallelMap(cat, c("xxx", "yyy"))
  check.exists(iter=1, n=2)
  check.contains(iter=1, c("xxx", "yyy"))

  parallelMap(print, c("xxx", "yyy"))
  check.exists(iter=2, n=2)
  check.contains(iter=2, c("xxx", "yyy"))

  parallelMap(warning, c("xxx", "yyy"))
  check.exists(iter=3, n=2)
  check.contains(iter=3, c("xxx", "yyy"))
}

# test that exported variables exist on slave
partest3 = function() {
  # export nothing, no change
  parallelExport()
  foo = 100
  parallelExport("foo")
  f = function(i)
    i + foo
  expect_equal(parallelMap(f, 1:2), list(101, 102))
  # now test with foo2 defined one level fruther above
  f = function(i)
    i + foo2
  foo2 = 100
  g = function() {
    parallelExport("foo2")
  }
  g()
  expect_equal(parallelMap(f, 1:2), list(101, 102))

  # now test with foo3 defined in global env
  assign("foo3", 100, env=.GlobalEnv)
  f = function(i)
    i + foo3
  parallelExport("foo3")
  expect_equal(parallelMap(f, 1:2), list(101, 102))
}

# test that exported libraries are loaded
partest4 = function(slave.error.test) {
  # testhat is basically the only lib we have in suggests...
  parallelLibrary("testthat")
  f = function(i)
    expect_true
  res = parallelMap(f, 1:2)
  expect_true(is.list(res) && length(res) == 2 && is.function(res[[1]]))
  if (slave.error.test) {
    expect_error(parallelLibrary("foo", master=FALSE),
      "Packages could not be loaded on all slaves: foo.")
    expect_error(parallelLibrary("foo1", "foo2", master=FALSE),
      "Packages could not be loaded on all slaves: foo1,foo2.")
    expect_error(parallelLibrary("testthat", "foo", master=FALSE),
      "Packages could not be loaded on all slaves: foo.")
  }
}

#test error handling
partest5 = function() {
  # exception is thrown on master
  f = function(i) {
  if(i == 1)
    stop("foo")
  else
    i
  }
  y = parallelMap(f, 1:2, impute.error = identity)
  expect_true(is.error(y[[1L]]))
  expect_equal(y[[2L]], 2L)
  y = parallelMap(f, 1:2, impute.error = function(x) 123)
  expect_equal(y[[1L]], 123L)
  expect_equal(y[[2L]], 2L)
  y = parallelMap(f, 1:2, impute.error = 99)
  expect_equal(y[[1L]], 99L)
  expect_equal(y[[2L]], 2L)
}

# test that exported files are sourced
partest6 = function(slave.error.test) {
  # this is all so fucking crappy and broken
  # apparently the name/path changes depending on how I run the test...
  fn = system.file("test_source_file.R", package="parallelMap")
  if (fn == "")
    fn = system.file("inst/test_source_file.R", package="parallelMap")
  parallelSource(fn, master=FALSE)
  f = function(i) i + xxx
  res = parallelMap(f, 1:2, simplify=TRUE)
  expect_equal(res, 124:125)
  if (slave.error.test) {
    expect_error(parallelSource("foo", master=FALSE),
      "Files could not be sourced on all slaves: foo.")
    expect_error(parallelSource("foo1", "foo2", master=FALSE),
      "Files could not be sourced on all slaves: foo1,foo2.")
    expect_error(parallelSource(fn, "foo", master=FALSE),
      "Files could not be sourced on all slaves: foo.")
  }
}


