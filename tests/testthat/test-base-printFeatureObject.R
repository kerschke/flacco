context("Base: Print Feature Object")

test_that("FeatureObject Output", {
  feat.object = createFeatureObject(iris, objective = "Species")
  expect_output(print(feat.object), regexp = paste(
    "^Feature Object:",
    "- Number of Observations:\\s\\d+",
    "- Number of Features:\\s\\d+",
    "- Lower Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Upper Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Name of Features: [^,\\s]*(, [^,\\s]*)*",
    "- Optimization Problem: (minimize|maximize) [^\\s]+?",
    sep="\\r?\\n")
  )
})

test_that("FeatureObject Output with more than 5 feature dims", {
  init = matrix(1, ncol=6, nrow=2)
  colnames(init) = c("a1", "a2", "a3", "a4", "a5", "a6")
  feat.object = createFeatureObject(init, objective = "a6")
  expect_output(print(feat.object), regexp = paste(
    "- Lower Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*, ...",
    "- Upper Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*, ...",
    "- Name of Features: [^,\\s]*(, [^,\\s]*)*, ...",
    sep="\\r?\\n")
  )
})

test_that("FeatureObject Output with a function", {
  init = matrix(1L, ncol = 6L, nrow = 2L)
  colnames(init) = c("a1", "a2", "a3", "a4", "a5", "a6")
  feat.object = createFeatureObject(init, objective = "a6", fun = function (x) sum(x^2))
  expect_output(print(feat.object),
    regexp = "- Function to be Optimized: function \\(x\\) sum\\(x\\^2\\)")
})

test_that("FeatureObject Output with cellmapping and dim < 5", {
  init = matrix(1:10, ncol = 5L, nrow = 10L)
  colnames(init) = c("a1", "a2", "a3", "a4", "a5")
  feat.object = createFeatureObject(init, objective = "a5", blocks = 4L)
  expect_output(print(feat.object), regexp = paste(
    "- Number of Cells per Dimension: \\d+(, \\d+)*",
    "- Size of Cells per Dimension: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Number of Cells:",
    "\\s+- total: \\d+",
    "\\s+- non-empty: \\d+ \\([0-9e+-.]+%\\)",
    "\\s+- empty: \\d+ \\([0-9e+-.]+%\\)",
    "- Average Number of Observations per Cell:",
    "\\s+- total: [0-9e+-.]+",
    "\\s+- non-empty: [0-9e+-.]+",
    sep="\\r?\\n")
  )
})

test_that("FeatureObject Output with cellmapping and dim >= 5", {
  init = matrix(1:10, ncol = 6L, nrow = 10L)
  colnames(init) = c("a1", "a2", "a3", "a4", "a5", "a6")
  feat.object = createFeatureObject(init, objective = "a6", blocks = 4L)
  expect_output(print(feat.object), regexp=paste(
      "- Number of Cells per Dimension: \\d+(, \\d+)*, ...",
      "- Size of Cells per Dimension: [0-9e+.-]+(, [0-9e+-.]+)*, ...",
      sep="\\r?\\n")
  )
})

test_that("Usage of regular and smoof functions", {
  X = createInitialSample(n.obs = 200, dim = 2)
  f1 = smoof::makeBBOBFunction(dimension = 2, fid = 23, iid = 1)
  f2 = function(x) sum(x^2 * sin(x^3))
  y1 = apply(X, 1, f1)
  y2 = apply(X, 1, f2)
  
  feat.object1 = createFeatureObject(X = X, y = y1, fun = f1)
  feat.object2 = createFeatureObject(X = X, y = y2, fun = f2)
  expect_output(print(feat.object1), regexp = paste(
    "^Feature Object:",
    "- Number of Observations:\\s\\d+",
    "- Number of Features:\\s\\d+",
    "- Lower Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Upper Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Name of Features: [^,\\s]*(, [^,\\s]*)*",
    "- Optimization Problem: (minimize|maximize) [^\\s]+?",
    "- Function to be Optimized: smoof-function [(BBOB)]+?",
    sep="\\r?\\n")
  )
  expect_output(print(feat.object2), regexp = paste(
    "^Feature Object:",
    "- Number of Observations:\\s\\d+",
    "- Number of Features:\\s\\d+",
    "- Lower Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Upper Boundaries: [0-9e+.-]+(, [0-9e+-.]+)*",
    "- Name of Features: [^,\\s]*(, [^,\\s]*)*",
    "- Optimization Problem: (minimize|maximize) [^\\s]+?",
    "- Function to be Optimized: function [\\s]*",
    sep="\\r?\\n")
  )
})
