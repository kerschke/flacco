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
 
