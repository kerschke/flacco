#' @title List Available Feature Sets
#'
#' @description Lists all available feature sets w.r.t. certain restrictions.
#' @param subset [\code{\link{character}}]\cr
#' Vector of feature sets, which should be considered. If not defined, all
#' features will be considered.
#' @param allow.cellmapping [\code{\link{logical}(1)}]\cr
#' Should (general) cell mapping features be considered as well? The default is
#' \code{TRUE}.
#' @param allow.additional_costs [\code{\link{logical}(1)}]\cr
#' Should feature sets be considered, which require additional function
#' evaluations? The default is \code{TRUE}.
#' @param blacklist [\code{\link{character}}]\cr
#' Vector of feature sets, which should not be considered. The default is
#' \code{NULL}.
#' @return [\code{\link{character}}].\cr
#' Feature sets, which could be computed - based on the provided input.
#' @export
listAvailableFeatureSets = function(subset, allow.cellmapping, allow.additional_costs, blacklist) {
  allFeats = listAllFeatureSets()
  possible = unlist(allFeats)
  if (missing(subset))
    subset = possible
  assertSubset(subset, choices = possible)
  if (missing(allow.cellmapping))
    allow.cellmapping = TRUE
  assertLogical(allow.cellmapping)
  if (missing(allow.additional_costs))
    allow.additional_costs = TRUE
  assertLogical(allow.additional_costs)
  if (missing(blacklist))
    blacklist = NULL
  assertSubset(blacklist, choices = possible)
  if (!allow.cellmapping)
    subset = setdiff(subset, c(unlist(allFeats[c("cm.feats", "gcm.feats")]), "linear_model"))
  if (!allow.additional_costs)
    subset = setdiff(subset, c("convexity", "curvature", "local_search"))
  return(setdiff(subset, blacklist))
}

listAllFeatureSets = function() {
  list(cm.feats = c("angle", "cell_convexity", "gradient_homogeneity"),
    ela.feats = c("convexity", "curvature", "distribution", "levelset", "local_search", "meta_model"),
    misc.feats = c("basic", "dispersion", "linear_model", "nearest_better", "principal_component"),
    gcm.feats = NULL, ## FIXME: needs to be completed
    ic.feats = NULL ## FIXME: needs to be completed
  )
}
