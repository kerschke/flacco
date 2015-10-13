context("Base: List Available Features")

test_that("listAvailableFeatures", {
  x = listAvailableFeatureSets()
  expect_equal(x, c("cm_angle", "cm_conv", "cm_grad", "ela_conv", "ela_curv",
    "ela_distr", "ela_level", "ela_local", "ela_meta", "basic", "disp", "limo",
    "nbc", "pca", "bt", "gcm", "ic"))

  x = c("bt", "limo", "ela_curv")
  expect_equal(listAvailableFeatureSets(subset = x), x)
  expect_equal(listAvailableFeatureSets(
    subset = x, allow.cellmapping = FALSE), c("limo", "ela_curv"))
  expect_equal(listAvailableFeatureSets(
    subset = x, allow.additional_costs = FALSE), c("bt", "limo"))
  expect_equal(listAvailableFeatureSets(subset = x, 
    allow.cellmapping = FALSE, allow.additional_costs = FALSE), "limo")
  expect_equal(listAvailableFeatureSets(subset = x, allow.cellmapping = FALSE, 
    blacklist = c("ela_curv", "ela_meta", "limo")), character())
})
