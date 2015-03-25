context("GCM Feature Computations")

test_that("GCM features are computed", {
  values = iris[, -5] # strip factor column "Species"
  
  # preconditions
  featobj = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  gcm_init(featobj)
  
  # execution
  features = calculateGCMFeatures(featobj)
 
  # postconditions
  expect_is(features, "list")
  expect_is(features$gcm.min, "list")
  expect_is(features$gcm.mean, "list")
  expect_is(features$gcm.near, "list")
})