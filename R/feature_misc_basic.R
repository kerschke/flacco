# @title Calculate Basic Features
# 
# @description
# Lists features based on the initial design and objective values.
# @param feat.object [\code{\link{FeatureObject}}]\cr
# A feature object as created by \link{createFeatureObject}.\cr
# @return [\code{\link{list}(16)} of \code{\link{numeric}(1)}].\cr
# List of features.\cr
# For further information, see details.
# @details
# Simple basic information, given by the initial design:\cr
# 
# (1) \code{dim}: dimension of the decision space\cr
# (2) \code{observations}: number of observations\cr
# (3) \code{lower.min}: minimum of all lower bounds\cr
# (4) \code{lower.max}: maximum of all lower bounds\cr
# (5) \code{upper.min}: minimum of all upper bounds\cr
# (6) \code{upper.max}: maximum of all upper bounds\cr
# (7) \code{objective.min}: minimum of objective values (within the initial
# design)\cr
# (8) \code{objective.max}: maximum of objective values (within the initial
# design)\cr
# (9) \code{blocks.min}: minimum number of blocks\cr
# (10) \code{blocks.max}: maximum number of blocks\cr
# (11) \code{cells.total}: total number of cells\cr
# (12) \code{cells.filled}: number of non-empty cells\cr
# (13) \code{allows_cm}: does the feature object support cell mapping?\cr
# (14) \code{minimize_fun}: should the objective be minimized?\cr
# 
# The final two features show the amount of (additional) function
# evaluations and running time (in seconds) that were needed for the
# computation of these features.
# @examples
# # (1) create the initial design:
# X = t(replicate(1000, runif(2, -10, 10)))
# y = apply(X, 1, function(x) sum(x^2))
# feat.object = createFeatureObject(X = X, y = y)
# 
# # (2) compute the basic features:
# calculateBasicFeatures(feat.object)
# @export 
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
      basic.lower.min = min(feat.object$lower),
      basic.lower.max = max(feat.object$lower),
      basic.upper.min = min(feat.object$upper),
      basic.upper.max = max(feat.object$upper),
      basic.objective.min = min(y),
      basic.objective.max = max(y),
      basic.blocks.min = min.blocks,
      basic.blocks.max = max.blocks,
      basic.cells.total = no.cells,
      basic.cells.filled = filled.cells,
      basic.allows_cm = feat.object$allows.cellmapping,
      basic.minimize_fun = feat.object$minimize))
  }), "basic")
}
