#' @rdname features
#' @export 
calculateFeatureSet = function(feat.object, set, control, ...) {
  assertClass(feat.object, "FeatureObject")
  assertCharacter(set, len = 1L)
  assertSubset(set, choices = listAvailableFeatureSets())
  if (missing(control))
    control = list()
  assertList(control)
  switch(set,
    cm_angle = calculateAngleFeatures(feat.object, control),
    cm_conv = calculateCellConvexityFeatures(feat.object, control, ...),
    cm_grad = calculateGradientHomogeneityFeatures(feat.object, control),
    ela_conv = calculateConvexityFeatures(feat.object, control),
    ela_curv = calculateCurvatureFeatures(feat.object, control),
    ela_distr = calculateDistributionFeatures(feat.object, control, ...),
    ela_level = calculateLevelsetFeatures(feat.object, control),
    ela_local = calculateLocalSearchFeatures(feat.object, control, ...),
    ela_meta = calculateMetaModelFeatures(feat.object),
    basic = calculateBasicFeatures(feat.object),
    disp = calculateDispersionFeatures(feat.object, control),
    limo = calculateLinearModelFeatures(feat.object),
    nbc = calculateNearestBetterFeatures(feat.object, control),
    pca = calculatePrincipalComponentFeatures(feat.object, control),
    bt = calculateBarrierTreeFeatures(feat.object, control),
    gcm = calculateGCMFeatures(feat.object, control),
    ic = calculateInformationContentFeatures(feat.object, control)
  )
}
