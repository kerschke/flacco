context("Features: GCM")

test_that("GCM features are computed", {
  values = iris[, -5] # strip factor column "Species"
  
  # preconditions
  feat.object = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  
  # execution
  features = calculateFeatureSet(feat.object, "gcm")
 
  # postconditions
  expect_is(features, "list")
})

test_that("GCM features cannot compute on non-cellmapping object", {
  set.seed(2015*03*25)
  X = t(replicate(5000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {sum(x^2)})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  
  
  # preconditions
  #gcm_init(feat.object)
  
  # execution
  expect_error( calculateFeatureSet(feat.object, "gcm") )
})

test_that("GCM features draw three plots for two-dimensional inputs", {
  values = iris[, 1:3] # make 2d + objective
  
  # preconditions
  feat.object = createFeatureObject(values, objective = "Petal.Length", blocks = 5)
  
  # execution
  calculateFeatureSet(feat.object, "gcm", control = list(gcm.plot = TRUE))
  
  # since we cannot capture the created plot properly -- not even using 
  # evaluate::evaluate --, we can only test whether no errors were raised.
  
  expect_true(TRUE) # this can only happen if no error has forced the execution to stop.
})

test_that("GCM features draw three plots with custom colors", {
  values = iris[, 1:3] # make 2d + objective
  
  # preconditions
  feat.object = createFeatureObject(values, objective = "Petal.Length", blocks = 5)
  
  # execution
  calculateFeatureSet(feat.object, "gcm", control = list(gcm.plot = TRUE,
                                                         barrierTree.plot.colors = c("red", "blue")))
  
  # since we cannot capture the created plot properly -- not even using 
  # evaluate::evaluate --, we can only test whether no errors were raised.
  
  expect_true(TRUE) # this can only happen if no error has forced the execution to stop.
})
