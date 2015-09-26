context("Plot: Feature Importance Plot")

test_that("Feature Importance Plots", {
  featureList = list(c("Hello", "World", "bla"), c("Test", "bla"),
    c("Test", "World", "bla"), c("bla"), c("bla", "Test"), c("World", "bla"))
  plotFeatureImportance(featureList)
  
  # In order to check whether the plot properly adjusts the size, when
  # the number of features is less than 5 (or not), it is called with
  # a different sized featureList:
  if (length(unique(unlist(featureList))) < 5) {
    featureList[[1]] = c("a", "b", "c", "d", "e", "f")
    plotFeatureImportance(featureList)
  } else {
    tab = table(unlist(featureList))
    tab = sort(tab, decreasing = TRUE)
    featureList = lapply(featureList, function(x) x %in% names(tab)[1:4])
    plotFeatureImportance(featureList)
  }
  
  # since we cannot capture the created plot properly, we can only test whether
  # no errors were raised.
  expect_true(TRUE) # only TRUE if no error has caused an error
})

test_that("Use existing featureList", {
  plotFeatureImportance(featureList)  

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})
