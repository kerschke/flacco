context("Features: Information Content")

test_that("First Test Case", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
   lower = -1000, upper = 1000)  
  
  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "info_content", control = 
    list(ic.epsilon = c(0, 10^eps))
  )
 
  # test return values
  expect_identical(length(features), 6L)
  expect_is(features, class = "list")
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 4L), "integer", "numeric"))
  
  expect_true( testNumber(features$ic.max_info_cont, FALSE, 0, 1) )
  expect_true( testNumber(features$ic.settl_sens, 
    na.ok = TRUE, lower = -5, upper = 15) )
  expect_true( features$ic.settl_sens %in% c(eps, NA_real_) )
  expect_true( testNumber(features$ic.init_part_info,
    na.ok = FALSE, lower = 0, upper = 1) )
  expect_true( testNumber(features$ic.half_part_info_sens,
    na.ok = TRUE, lower = -5, upper = 15) )
  expect_true( features$ic.half_part_info_sens %in% c(eps, NA_real_) )
  expect_identical( features$ic.costs_fun_evals, 0L ) 
  expect_true( testNumber(features$ic.costs_runtime, lower = 0) ) 
})

test_that("Use random sorting", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  
  
  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "info_content", 
    control = list(ic.sorting = "random", ic.epsilon = c(0, 10^eps)))
  
  # test return values
  expect_identical(length(features), 6L)
  expect_is(features, class = "list")
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 4L), "integer", "numeric"))
  
  expect_true( testNumber(features$ic.max_info_cont, FALSE, 0, 1) )
  expect_true( testNumber(features$ic.settl_sens, 
    na.ok = TRUE, lower = -5, upper = 15) )
  expect_true( features$ic.settl_sens %in% c(eps, NA_real_) )
  expect_true( testNumber(features$ic.init_part_info,
    na.ok = FALSE, lower = 0, upper = 1) )
  expect_true( testNumber(features$ic.half_part_info_sens,
    na.ok = TRUE, lower = -5, upper = 15) )
  expect_true( features$ic.half_part_info_sens %in% c(eps, NA_real_) )
  expect_identical( features$ic.costs_fun_evals, 0L ) 
  expect_true( testNumber(features$ic.costs_runtime, lower = 0) ) 
})

test_that("Error if all epsilon != 0", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  
  
  expect_error( calculateFeatureSet(feat.object, "info_content",
    control = list(ic.epsilon = c(1,2)) )
  )
})

test_that("Check for NAs in output", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000)  
  
  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "info_content", 
    control = list(ic.settling_sensitivity = 0, 
      ic.information_sensitivity = 1, ic.epsilon = c(0, 10^eps)))
  
  # test return values
  expect_identical(length(features), 6L)
  expect_is(features, class = "list")
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 4L), "integer", "numeric"))
  
  expect_true( testNumber(features$ic.max_info_cont, FALSE, 0, 1) )
  expect_true( is.na(features$ic.settl_sens) )
  expect_true( features$ic.settl_sens %in% c(eps, NA_real_) )
  expect_true( testNumber(features$ic.init_part_info,
    na.ok = FALSE, lower = 0, upper = 1) )
  expect_true( is.na(features$ic.half_part_info_sens) )
  expect_true( features$ic.half_part_info_sens %in% c(eps, NA_real_) )
  expect_identical( features$ic.costs_fun_evals, 0L ) 
  expect_true( testNumber(features$ic.costs_runtime, lower = 0) ) 
})

test_that("Create random samples using LHD", {
  set.seed(2015*03*25)
  X = t(replicate(50, runif(2, -1000, 1000)))
  fun = function(x) {sum(sqrt(abs(x)))}
  feat.object = createFeatureObject(X = X, fun = fun, 
    lower = -1000, upper = 1000)  
  
  # execution (fewer epsilons for quicker tests)
  eps = seq(-5, 15, length.out = 200)
  features = calculateFeatureSet(feat.object, "info_content", 
    control = list(ic.generate_sample = TRUE, 
      ic.epsilon = c(0, 10^eps))
  ) 
  
  # test return values
  expect_identical(length(features), 6L)
  expect_is(features, class = "list")
  expect_identical(as.character(sapply(features, class)),
    c(rep("numeric", 4L), "integer", "numeric"))
  
  expect_true( testNumber(features$ic.max_info_cont, FALSE, 0, 1) )
  expect_true( testNumber(features$ic.settl_sens, 
    na.ok = TRUE, lower = -5, upper = 15) )
  expect_true( features$ic.settl_sens %in% c(eps, NA_real_) )
  expect_true( testNumber(features$ic.init_part_info,
    na.ok = FALSE, lower = 0, upper = 1) )
  expect_true( testNumber(features$ic.half_part_info_sens,
    na.ok = TRUE, lower = -5, upper = 15) )
  expect_true( features$ic.half_part_info_sens %in% c(eps, NA_real_) )
  expect_identical( features$ic.costs_fun_evals, 0L ) 
  expect_true( testNumber(features$ic.costs_runtime, lower = 0) ) 
})

test_that("Unable to compute samples without a function", {
  X = matrix(1, ncol=2, nrow=2)
  y = rep(1,2)
  feat.object = createFeatureObject(X = X, y = y)  
  expect_error( calculateFeatureSet(feat.object, "info_content",
    control = list(ic.generate_sample = TRUE)) 
  )
})
