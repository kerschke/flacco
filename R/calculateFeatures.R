#' @title Calculate Features
#' @description
#' Computes features describing the landscape of a continuous function.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.
#' @param control [\code{\link{list}}]\cr
#' A list object that stores additional configuration parameters.
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
#' @param ... [any]\cr
#' Further arguments, e.g. handled by \code{optim} (within the computation
#' of the local search features).
#' @return [\code{\link{list}} of \code{\link{numeric}(1)}].\cr
#' List of features.
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#' 
#' # (2) compute all non-cellmapping features
#' calculateFeatures(feat.object, allow.cellmapping = FALSE)
#' 
#' # (3) in order to allow the computation of the cell mapping features, one
#' # has to provide a feature object that has knowledge about the number of
#' # cells per dimension:
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2), blocks = 3)
#' calculateFeatures(feat.object)
#' @export 
calculateFeatures = function(feat.object, control, subset, allow.cellmapping, allow.additional_costs, blacklist, ...) {
  assertClass(feat.object, "FeatureObject")
  possible = as.character(unlist(listAllFeatureSets()))
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
  
  pure_cm = setdiff(listAvailableFeatureSets(allow.cellmapping = TRUE),
    listAvailableFeatureSets(allow.cellmapping = FALSE))
  if (allow.cellmapping && !feat.object$allows.cellmapping && any(pure_cm %in% setdiff(subset, blacklist)))
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  
  expensive = setdiff(listAvailableFeatureSets(allow.additional_costs = TRUE),
    listAvailableFeatureSets(allow.additional_costs = FALSE))  
  if (allow.additional_costs & is.null(feat.object$fun) & any(expensive %in% setdiff(subset, blacklist)))
    stop("The local search features require the exact function!")
  sets = listAvailableFeatureSets(subset, allow.cellmapping, allow.additional_costs, blacklist)
  
  if (missing(control))
    control = list()
  assertList(control)
  prog = control_parameter(control, "show_progress", TRUE)
  if (prog) {
    bar = makeProgressBar(min = 0, max = length(sets), label = "")
    no_chars = max(nchar(sets))
    features = lapply(sets, function(set) {
      txt = paste0(set, paste(rep(" ", no_chars - nchar(set)), collapse = ""))
      bar$inc(1L, txt)
      calculateFeatureSet(feat.object, set, control, ...)
    })
  } else {
    features = lapply(sets, function(set) calculateFeatureSet(feat.object, set, control, ...))    
  }
  do.call(c, features)
}
