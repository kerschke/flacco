#' @title Calculate Features of a Given Feature Set
#' @description
#' Computes features that belong to a specific feature set. 
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.
#' @param set [\code{\link{character}(1)}]\cr
#' Name of the feature set, which contains the features that should be
#' computed. You can list all possible feature sets using
#' \code{\link{listAvailableFeatureSets}}.
#' @param control [\code{\link{list}}]\cr
#' A list object that stores additional configuration parameters.
#' @param ... [any]\cr
#' Further arguments, e.g. handled by \code{optim} (within computation
#' of local search features).
#' @return [\code{\link{list}} of \code{\link{numeric}(1)}].\cr
#' List of features.
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#' 
#' # (2) compute the meta model features
#' calculateFeatureSet(feat.object, set = "meta_model")
#' 
#' # (3) compute the curvature features
#' calculateFeatureSet(feat.object, set = "curvature")
#' 
#' # (4) compute the angle features (note that this requires a feature object)
#' # which allows cell mapping:
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2), blocks = 3)
#' calculateFeatureSet(feat.object, set = "angle", control = list(angle.show_warnings = FALSE))
#' @export 
calculateFeatureSet = function(feat.object, set, control, ...) {
  assertClass(feat.object, "FeatureObject")
  assertCharacter(set, len = 1L)
  assertSubset(set, choices = listAvailableFeatureSets())
  if (missing(control))
    control = list()
  assertList(control)
  switch(set,
    angle = calculateAngleFeatures(feat.object, control),
    cell_convexity = calculateCellConvexityFeatures(feat.object, control, ...),
    gradient_homogeneity = calculateGradientHomogeneityFeatures(feat.object, control),
    convexity = calculateConvexityFeatures(feat.object, control),
    curvature = calculateCurvatureFeatures(feat.object, control),
    distribution = calculateDistributionFeatures(feat.object, control, ...),
    levelset = calculateLevelsetFeatures(feat.object, control),
    local_search = calculateLocalSearchFeatures(feat.object, control, ...),
    meta_model = calculateMetaModelFeatures(feat.object),
    basic = calculateBasicFeatures(feat.object),
    dispersion = calculateDispersionFeatures(feat.object, control),
    linear_model = calculateLinearModelFeatures(feat.object),
    nearest_better = calculateNearestBetterFeatures(feat.object, control),
    principal_component = calculatePrincipalComponentFeatures(feat.object, control),
    barrier_tree = calculateBarrierTrees(feat.object, control),
    gcm = calculateGCMFeatures(feat.object, control),
    info_content = calculateInformationContent(feat.object, control)
  )
}
