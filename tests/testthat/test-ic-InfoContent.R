context("Features: Information Content")

test_that("Information content-based features are computed", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
   lower = -1000, upper = 1000)  
  
  # execution
  features = calculateFeatureSet(feat.object, "info_content", control = 
    list(ic.epsilon = c(0, 10^(seq(-5, 15, length.out = 200))))
  ) # => fewer epsilons for quicker tests
 
  # postconditions
  expect_true( testNumber(features$ic.Hmax, FALSE, 0, 1) )
  # TODO epsilonS
  expect_true( is.na(features$ic.Mzero) ) 
  expect_true( is.na(features$ic.epsilon05) ) 
})

test_that("Information content-based features are computed (Random sorting)", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  features = calculateFeatureSet(feat.object, "info_content", control = 
    list(ic.sorting = "R", ic.epsilon = c(0, 10^(seq(-5, 15, length.out = 200))))
  ) # => fewer epsilons for quicker tests
  
  # postconditions
  expect_true( testNumber(features$ic.Hmax, FALSE, 0, 1) )
  # TODO epsilonS
  expect_true( is.na(features$ic.Mzero) ) 
  expect_true( is.na(features$ic.epsilon05) ) 
})

test_that("Information content-based features (with partial information) are computed", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  features = calculateFeatureSet(feat.object, "info_content", control = 
    list(ic.calculate_partial = TRUE, 
      ic.epsilon = c(0, 10^(seq(-5, 15, length.out = 200))))
    ) # => fewer epsilons for quicker tests
  
  # postconditions
  expect_true( testNumber(features$ic.Mzero, FALSE, 0, 1) )
  # TODO features$ic.epsilon05
})

test_that("Information content-based features are not computed for wrong epsilon", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  expect_error( calculateFeatureSet(feat.object, "info_content",
    control = list(ic.epsilon = c(1,2)) )
  )
})

test_that("Information content-based features are able to compute random samples", {
  set.seed(2015*03*25)
  X = t(replicate(50, runif(2, -1000, 1000)))
  fun = function(x) {sum(sqrt(abs(x)))}
  feat.object = createFeatureObject(X = X, fun = fun, 
                                lower = -1000, upper = 1000)  
  
  # execution
  features = calculateFeatureSet(feat.object, "info_content", 
    control = list(ic.generate_sample = TRUE,
      ic.epsilon = c(0, 10^(seq(-5, 15, length.out = 200))))
    ) # => fewer epsilons for quicker tests
  
  # postconditions (sadly, samples cannot be tested :( ))
  expect_true( testNumber(features$ic.Hmax, FALSE, 0, 1) )
  # TODO epsilonS
  expect_true( is.na(features$ic.Mzero) ) 
  expect_true( is.na(features$ic.epsilon05) ) 
})

test_that("Information content-based features are unable to compute samples without a function", {
  X = matrix(1, ncol=2, nrow=2)
  y = rep(1,2)
  feat.object = createFeatureObject(X = X, y = y)  
  expect_error( calculateFeatureSet(feat.object, "info_content",
    control = list(ic.generate_sample = TRUE)) 
  )
})

test_that("Information content-based features can create a plot", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  features = calculateFeatureSet(feat.object, "info_content", control =
    list(ic.epsilon = c(0, 10^(seq(-5, 15, length.out = 200))), ic.plot = TRUE)
  ) # => fewer epsilons for quicker tests
  
  # since we cannot capture the created plot properly -- not even using 
  # evaluate::evaluate --, we can only test whether no errors were raised.
  
  expect_true(TRUE) # this can only happen if no error has forced the execution to stop.
})

test_that("Information content-based features (with partial information) can create a plot", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  
  
  # execution
  features = calculateFeatureSet(feat.object, "info_content", control = 
    list(ic.epsilon = c(0, 10^(seq(-5, 15, length.out = 200))),
      ic.calculate_partial = TRUE, ic.plot = TRUE)
    ) # => fewer epsilons for quicker tests
  
  # since we cannot capture the created plot properly -- not even using 
  # evaluate::evaluate --, we can only test whether no errors were raised.
  
  expect_true(TRUE) # this can only happen if no error has forced the execution to stop.
})