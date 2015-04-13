context("Base: Initialize GCM")

test_that("GCM initialisation for factor-valued objectives is not possible", {
  feat.object = createFeatureObject(iris, objective = "Species", blocks = 5)
  expect_null(feat.object$env$gcm.representatives$min)
  expect_null(feat.object$env$gcm.canonicalForm$min)
  expect_error( gcm_init(feat.object) )
})

test_that("GCM initialisation computations for min are performed correctly", {
  values = iris[, -5] # strip factor column "Species"
  
  # preconditions
  feat.object = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  expect_null(feat.object$env$gcm.representatives$min)
  expect_null(feat.object$env$gcm.canonicalForm$min)
  
  # execution
  gcm_init(feat.object)
  
  # postconditions
  expect_equal(length(feat.object$env$gcm.representatives$min), 5^3)
  expect_is(feat.object$env$gcm.canonicalForm$min, "list")
  
  # empty cells
  expect_equal(length(which(feat.object$env$gcm.representatives$min == Inf)), 94)
  
  # number of closed classes
  expect_equal(feat.object$env$gcm.canonicalForm$min$closedClassIndex, 92)
  
  # TODO values of feat.object$env$gcm.canonicalForm$min$indexPermutation
  # TODO values of feat.object$env$gcm.canonicalForm$min$canonicalForm
})

test_that("GCM initialisation computations for mean are performed", {
  values = iris[, -5] # strip factor column "Species"
  
  # preconditions
  feat.object = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  expect_null(feat.object$env$gcm.representatives$mean)
  expect_null(feat.object$env$gcm.canonicalForm$mean)
  
  # execution
  gcm_init(feat.object)
  
  # postconditions
  expect_equal(length(feat.object$env$gcm.representatives$mean), 5^3)
  expect_is(feat.object$env$gcm.canonicalForm$mean, "list")
  
  # empty cells
  expect_equal(length(which(feat.object$env$gcm.representatives$mean == Inf)), 94)
  
  # number of closed classes
  expect_equal(feat.object$env$gcm.canonicalForm$mean$closedClassIndex, 91)
  
  # TODO values of feat.object$env$gcm.canonicalForm$mean$indexPermutation
  # TODO values of feat.object$env$gcm.canonicalForm$mean$canonicalForm
})

test_that("GCM initialisation computations for near are performed", {
  values = iris[, -5] # strip factor column "Species"
  
  # preconditions
  feat.object = createFeatureObject(values, objective = "Petal.Width", blocks = 5)
  expect_null(feat.object$env$gcm.representatives$near)
  expect_null(feat.object$env$gcm.canonicalForm$near)
  
  # execution
  set.seed(2015*03*25) #selectMin (used for finding values near to cell center) is not deterministic, so let's force it to be that way!
  gcm_init(feat.object)
  
  # postconditions
  expect_equal(length(feat.object$env$gcm.representatives$near), 5^3)
  expect_is(feat.object$env$gcm.canonicalForm$near, "list")
  
  # empty cells
  expect_equal(length(which(feat.object$env$gcm.representatives$near == Inf)), 0) # ALWAYS for near!!
  
  # number of closed classes
  expect_equal(feat.object$env$gcm.canonicalForm$near$closedClassIndex, 12)
  
  # TODO values of feat.object$env$gcm.canonicalForm$near$indexPermutation
  # TODO values of feat.object$env$gcm.canonicalForm$near$canonicalForm
})