context("Features: Information Content")

testICFeatures = function(features, eps) {
  expect_identical(length(features), 7L)
  expect_list(features)
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 5L), "integer", "numeric"))

  expect_true(testNumber(features$ic.h.max, na.ok = FALSE, lower = 0, upper = 1))
  expect_true(testNumber(features$ic.eps.s, na.ok = TRUE,
    lower = min(eps), upper = max(eps)))
  expect_true(features$ic.eps.s %in% c(eps, NA_real_))
  expect_true(testNumber(features$ic.eps.max, na.ok = FALSE,
    lower = min(eps), upper = max(eps)))
  expect_true(testNumber(features$ic.m0, na.ok = FALSE, lower = 0, upper = 1))
  expect_true( testNumber(features$ic.eps.ratio, na.ok = TRUE,
    lower = min(eps), upper = max(eps)))
  expect_true(features$ic.eps.ratio %in% c(eps, NA_real_))
  expect_identical(features$ic.costs_fun_evals, 0L)
  expect_true(testNumber(features$ic.costs_runtime, lower = 0))
}

test_that("First Test Case", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, lower = -1000L, upper = 1000L)

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "ic",
    control = list(ic.epsilon = c(0, 10^eps)))

  # test return values
  testICFeatures(features, eps)
})

test_that("Use random sorting", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, lower = -1000, upper = 1000)  

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "ic", 
    control = list(ic.sorting = "random", ic.epsilon = c(0, 10^eps)))

  # test return values
  testICFeatures(features, eps)
})

test_that("Error if all epsilon != 0", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, lower = -1000, upper = 1000)  

  expect_error(calculateFeatureSet(feat.object, "ic",
    control = list(ic.epsilon = c(1, 2)))
  )
})

test_that("Check for NAs in output", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, lower = -1000, upper = 1000)

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "ic",
    control = list(ic.settling_sensitivity = 0,
      ic.information_sensitivity = 1, ic.epsilon = c(0, 10^eps))
  )

  # test return values
  testICFeatures(features, eps)
})

test_that("Create random samples using LHD", {
  set.seed(2015*03*25)
  X = t(replicate(50, runif(2, -1000, 1000)))
  fun = function(x) {sum(sqrt(abs(x)))}
  feat.object = createFeatureObject(X = X, fun = fun, 
    lower = -1000, upper = 1000)  

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "ic",
    control = list(ic.generate_sample = TRUE, ic.epsilon = c(0, 10^eps))
  ) 

  # test return values
  testICFeatures(features, eps)
})

test_that("Unable to compute samples without a function", {
  X = matrix(1, ncol=2, nrow=2)
  y = rep(1,2)
  feat.object = createFeatureObject(X = X, y = y)  
  expect_error(calculateFeatureSet(feat.object, "ic",
    control = list(ic.generate_sample = TRUE)) 
  )
})

test_that("Creating a sample", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  feat.object = createFeatureObject(X = X,
    fun = function(x) x[1]^4 + 1000 * (x[1] - 3)^3 + 1000 * x[1] + x[2],
    lower = -1000, upper = 1000)

  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "ic", 
    control = list(ic.sample.generate = TRUE, ic.epsilon = c(0, 10^eps)))

  # test return values
  testICFeatures(features, eps)
})

test_that("Checking for strange events", {
  # NA if ratio is too high
  feat.object = createFeatureObject(init = iris[,-5], objective = "Petal.Width")
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "ic",
    control = list(ic.epsilon = c(0, 10^eps), ic.info_sensitivity = 1))
  expect_identical(features$ic.eps.ratio, NA_real_)

  # warning when initial design contains 1 duplicate
  feat.object = createFeatureObject(init = iris[,-5], objective = "Petal.Width")
  eps = seq(-5, 15, length.out = 200)
  expect_warning(calculateFeatureSet(feat.object, "ic", 
    control = list(ic.epsilon = c(0, 10^eps), ic.show_warnings = TRUE)))

  # warning when initial design contains 2+ duplicates
  feat.object = createFeatureObject(init = iris[c(1:150, 1, 1), -5],
    objective = "Petal.Width")
  eps = seq(-5, 15, length.out = 200)
  expect_warning(calculateFeatureSet(feat.object, "ic", 
    control = list(ic.epsilon = c(0, 10^eps), ic.show_warnings = TRUE)))

  # expect error if all observations are identical
  feat.object = createFeatureObject(init = iris[rep(8L, 200L),-5], objective = "Petal.Width")
  expect_error(calculateFeatureSet(feat.object, "ic"))
})
