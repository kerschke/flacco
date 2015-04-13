context("Plot: Feature Importance Plot")

test_that("Feature Importance Plots", {
  library(mlr)
  library(mlbench)
  data(Glass)
  
  # (1) Create a classification task:
  classifTask = makeClassifTask(data = Glass, target = "Type")
  
  # (2) Define the model (here, a classification tree):
  lrn = makeLearner(cl = "classif.rpart")
  
  # (3) Define the resampling strategy, which is supposed to be used within 
  # each inner loop of the nested feature selection:
  innerResampling = makeResampleDesc("Holdout")
  
  # (4) What kind of feature selection approach should be used? Here, we use a
  # sequential backward strategy, i.e. starting with a model based on the
  # entire feature set, a feature is iteratively dropped (based on a greedy) 
  # approach:
  ctrl = makeFeatSelControlSequential(method = "sbs")
  
  # (5) Wrap the original model (see (2)) in order to allow feature selection:
  wrappedLearner = makeFeatSelWrapper(learner = lrn,
    resampling = innerResampling, control = ctrl)
     
  # (6) Define a resampling strategy for the outer loop. This is necessary in
  # order to assess whether the selected features depend on the underlying
  # fold:
  outerResampling = makeResampleDesc(method = "CV", iters = 3)
  
  # (7) Perform the feature selection:
  suppressAll((featselResult = resample(learner = wrappedLearner, task = classifTask,
    resampling = outerResampling, models = TRUE)))
  
  # (8) Extract the features, which were selected during each iteration of the
  # outer loop (i.e. during each of the 5 folds of the cross-validation):
  featureList = lapply(featselResult$models, 
    function(mod) getFeatSelResult(mod)$x)
  
  # Now, one could inspect the features using the feature
  # importance plot:
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
