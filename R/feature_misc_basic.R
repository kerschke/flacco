calculateBasicFeatures = function(feat.object) {
  assertClass(feat.object, "FeatureObject")
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    blocks = unique(feat.object$blocks)
    no.cells = as.integer(prod(feat.object$blocks))
    min.blocks = as.integer(min(blocks))
    max.blocks = as.integer(max(blocks))
    filled.cells = length(unique(feat.object$init.grid$cell.ID))
    return(list(basic.dim = feat.object$dim,
      basic.observations = feat.object$n.obs,
      basic.lower_min = min(feat.object$lower),
      basic.lower_max = max(feat.object$lower),
      basic.upper_min = min(feat.object$upper),
      basic.upper_max = max(feat.object$upper),
      basic.objective_min = min(y),
      basic.objective_max = max(y),
      basic.blocks_min = min.blocks,
      basic.blocks_max = max.blocks,
      basic.cells_total = no.cells,
      basic.cells_filled = filled.cells,
      basic.allows_cm = feat.object$allows.cellmapping,
      basic.minimize_fun = feat.object$minimize))
  }), "basic")
}
