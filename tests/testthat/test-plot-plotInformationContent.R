context("Plot: Information Content Plot")

test_that("Default InfoContent-Plot", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  plotInformationContent(feat.object)

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})

test_that("Without Partial InfoContent", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  plotInformationContent(feat.object, control = list(ic.plot.partial_ic = FALSE))

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})

test_that("Changing line and point types", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  plotInformationContent(feat.object,
    control = list(
      ic.plot.legend_location = c(-5, 0.9), ic.plot.ylim = c(0, 0.9),
      ic.plot.ic.lty = "dashed", ic.plot.partial_ic.lty = "dotted",
      ic.plot.max_ic.pch = "x", ic.plot.settl_sens.pch = "*",
      ic.plot.partial_ic.pch = "#", ic.plot.half_partial.pch = "+"))

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})
