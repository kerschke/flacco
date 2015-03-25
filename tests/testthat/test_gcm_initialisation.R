context("Initialisation of GCM Computations")

test_that("GCM initialisation for factor-valued objectives is not possible", {
  featobj = createFeatureObject(iris, objective = "Species", blocks = 5)
  expect_null(featobj$env$gcm.representatives$min)
  expect_null(featobj$env$gcm.canonicalForm$min)
  expect_error( gcm_init(featobj) )
})

test_that("GCM initialisation computations for min are performed", {
  values <- iris[, -5] # strip factor column "Species"
  
  # preconditions
  featobj = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  expect_null(featobj$env$gcm.representatives$min)
  expect_null(featobj$env$gcm.canonicalForm$min)
  
  # execution
  gcm_init(featobj)
  
  # postconditions
  expect_equal(length(featobj$env$gcm.representatives$min), 5^3)
  expect_is(featobj$env$gcm.canonicalForm$min, "list")
})

test_that("GCM initialisation computations for mean are performed", {
  values <- iris[, -5] # strip factor column "Species"
  
  # preconditions
  featobj = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  expect_null(featobj$env$gcm.representatives$mean)
  expect_null(featobj$env$gcm.canonicalForm$mean)
  
  # execution
  gcm_init(featobj)
  
  # postconditions
  expect_equal(length(featobj$env$gcm.representatives$mean), 5^3)
  expect_is(featobj$env$gcm.canonicalForm$mean, "list")
})

test_that("GCM initialisation computations for min are performed", {
  values <- iris[, -5] # strip factor column "Species"
  
  # preconditions
  featobj = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  expect_null(featobj$env$gcm.representatives$near)
  expect_null(featobj$env$gcm.canonicalForm$near)
  
  # execution
  gcm_init(featobj)
  
  # postconditions
  expect_equal(length(featobj$env$gcm.representatives$near), 5^3)
  expect_is(featobj$env$gcm.canonicalForm$near, "list")
})