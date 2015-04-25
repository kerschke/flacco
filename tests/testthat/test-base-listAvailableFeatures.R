context("Base: List Available Features")

test_that("listAvailableFeatures", {
  x = listAvailableFeatureSets()
  expect_equal(x, c("angle", "cell_convexity", "gradient_homogeneity",
    "convexity", "curvature", "distribution", "levelset", "local_search",
    "meta_model", "basic", "dispersion", "linear_model", "nearest_better", 
    "principal_component", "barrier_tree", "gcm", "info_content"))
  
  expect_error(listAvailableFeatureSets(subset = c("cell angle")))
  
  x = c("barrier_tree", "linear_model", "curvature")
  expect_equal(listAvailableFeatureSets(subset = x), x)
  expect_equal(listAvailableFeatureSets(
    subset = x, allow.cellmapping = FALSE), c("linear_model", "curvature"))
  expect_equal(listAvailableFeatureSets(
    subset = x, allow.additional_costs = FALSE), c("barrier_tree", "linear_model"))
  expect_equal(listAvailableFeatureSets(subset = x, 
    allow.cellmapping = FALSE, allow.additional_costs = FALSE), "linear_model")
  expect_equal(listAvailableFeatureSets(subset = x,
    allow.cellmapping = FALSE, 
    blacklist = c("curvature", "meta_model", "linear_model")), 
    character())
})
