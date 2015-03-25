context("print.FeatureObject")

test_that("FeatureObject Output", {
  featobj = createFeatureObject(iris, objective = "Species")
  
  expect_output(print(featobj), regexp=paste(
    "^Feature Object:",
    "- Number of Observations:\\s\\d+",
    "- Number of Features:\\s\\d+",
    "- Lower Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Upper Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Name of Features: [^,\\s]*(, [^,\\s]*)*",
    "- Optimization problem: (minimize|maximize) [^\\s]+?",
    sep="\\r?\\n")
  )
  
})
 
test_that("FeatureObject Output with more than 5 feature dims", {
  init = matrix(1, ncol=6, nrow=2)
  colnames(init) = c("a1", "a2", "a3", "a4", "a5", "a6")
  featobj = createFeatureObject(init, objective = "a6")
  
  expect_output(print(featobj), regexp=paste(
    "^Feature Object:",
    "- Number of Observations:\\s\\d+",
    "- Number of Features:\\s\\d+",
    "- Lower Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*, ...",
    "- Upper Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*, ...",
    "- Name of Features: [^,\\s]*(, [^,\\s]*)*",
    "- Optimization problem: (minimize|maximize) [^\\s]+?",
    sep="\\r?\\n")
  )
  
})

test_that("FeatureObject Output with a function", {
  init = matrix(1, ncol=6, nrow=2)
  colnames(init) = c("a1", "a2", "a3", "a4", "a5", "a6")
  featobj = createFeatureObject(init, objective = "a6", fun=function (x) sum(x^2))
  
  expect_output(print(featobj), regexp=
    "- Function to be optimized: function \\(x\\) sum\\(x\\^2\\)"
  )
  
})
