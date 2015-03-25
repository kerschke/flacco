context("ICoFiS Computations")

test_that("Information content-based features are computed", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  featobj = createFeatureObject(X = X, y = y, 
   lower = -1000, upper = 1000)  
  
  # execution
  features = calculateInformationContent(featobj, control = 
           list(ic.epsilon = c(
             0, 10^(seq(-5,15,length.out=300)) # => fewer epsilons for quicker tests
           )))
 
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
  featobj = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  features = calculateInformationContent(featobj, control = 
           list(ic.sorting = "R",
                ic.epsilon = c(
                  0, 10^(seq(-5,15,length.out=300)) # => fewer epsilons for quicker tests
                )))
  
  # postconditions
  expect_true( testNumber(features$ic.Hmax, FALSE, 0, 1) )
  # TODO epsilonS
  expect_true( is.na(features$ic.Mzero) ) 
  expect_true( is.na(features$ic.epsilon05) ) 
})

test_that("Information content-based partial information features are computed", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  featobj = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  features = calculateInformationContent(featobj, control = 
           list(ic.calculate_partial = TRUE,
                ic.epsilon = c(
                  0, 10^(seq(-5,15,length.out=300)) # => fewer epsilons for quicker tests
                )))
  
  # postconditions
  expect_true( testNumber(features$ic.Mzero, FALSE, 0, 1) )
  # TODO features$ic.epsilon05
})

test_that("Information content-based features are not computed for wrong epsilon", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  featobj = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  expect_error( calculateInformationContent(featobj, control = 
                                           list(ic.epsilon = c(1,2)) )
  )
})