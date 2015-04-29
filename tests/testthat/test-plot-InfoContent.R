context("Plot: Information Content")

test_that("Create a plot", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
                                    lower = -1000, upper = 1000)  
  
  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "info_content", control =
                                   list(ic.epsilon = c(0, 10^eps), ic.plot = TRUE)
  )
  
  # test return values
  expect_identical(length(features), 6L)
  expect_is(features, class = "list")
  expect_identical(as.character(sapply(features, class)),
                   c(rep("numeric", 4L), "integer", "numeric"))
  
  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})
