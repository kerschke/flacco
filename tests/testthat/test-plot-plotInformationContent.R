context("Plot: Information Content Plot")

test_that("Create a plot", {
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
