context("Features: Barrier Tree")

test_that("GCM-based Barrier Tree features are computed", {
  set.seed(2015*03*25)
  X = t(replicate(5000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
   lower = -1000, upper = 1000, blocks = 10)  
  
  # execution
  features = calculateFeatureSet(feat.object, "barrier_tree")
 
  # postconditions
  expect_is(features, "list")
  expect_is(features$barrierTree.min, "list")
  expect_is(features$barrierTree.mean, "list")
  expect_is(features$barrierTree.near, "list")
})

test_that("GCM-based Barrier Tree fallback is computed for boring barrier trees", {
  set.seed(2015*03*25)
  X = t(replicate(5000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {sum(x^2)})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000, blocks = 10)  
  
  # execution
  features = calculateFeatureSet(feat.object, "barrier_tree")
  
  # postconditions: data type
  expect_is(features, "list")
  expect_is(features$barrierTree.min, "list")
  expect_is(features$barrierTree.mean, "list")
  expect_is(features$barrierTree.near, "list")
  
  # postconditions: all values == 0
  sapply( names(features$barrierTree.min), function(name) {
    expect_equal(features$barrierTree.min[[name]], 0, info = paste("name=", name))
  })
  sapply( names(features$barrierTree.mean), function(name) {
    expect_equal(features$barrierTree.mean[[name]], 0, info = paste("name=", name))
  })
  sapply( names(features$barrierTree.near), function(name) {
    expect_equal(features$barrierTree.near[[name]], 0, info = paste("name=", name))
  })
})

test_that("GCM-based Barrier Tree cannot compute on non-cellmapping object", {
  set.seed(2015*03*25)
  X = t(replicate(5000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {sum(x^2)})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000)  
  
  # execution
  expect_error( calculateFeatureSet(feat.object, "barrier_tree") )
  
 
})

test_that("Barrier Trees are plotted for two-dimensional inputs", {
  set.seed(2015*03*25)
  X = t(replicate(5000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000, blocks = 10)  
  
  # execution
  calculateFeatureSet(feat.object, "barrier_tree", 
    control = list(barrierTree.plot = TRUE))
  
  # since we cannot capture the created plot properly -- not even using 
  # evaluate::evaluate --, we can only test whether no errors were raised.
  
  expect_true(TRUE) # this can only happen if no error has forced the execution to stop.
})

test_that("For boring barrier trees, no errors happen when trying to plot two-dimensional inputs", {
  set.seed(2015*03*25)
  X = t(replicate(5000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {sum(x^2)})
  feat.object = createFeatureObject(X = X, y = y, 
                                lower = -1000, upper = 1000, blocks = 10)  
  
  # execution
  calculateFeatureSet(feat.object, "barrier_tree",
    control = list(barrierTree.plot = TRUE))
  
  # since we cannot capture the created plot properly -- not even using 
  # evaluate::evaluate --, we can only test whether no errors were raised.
  
  expect_true(TRUE) # this can only happen if no error has forced the execution to stop.
})
