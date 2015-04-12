#' @title Calculate Cell Mapping Convexity Features
#'
#' @description
#' Computes features that describe the convexity or concavity of neighbouring
#' cells, provided the decision space is divided into a grid of cells.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.
#' @param diag [\code{logical}(1)]\cr
#' Should cells, which are located on the diagonal compared to the current
#' cell, be considered as neighbouring cells? Per default, only cells along
#' the axes are considered as neighbours, i.e. \code{diag = FALSE}.
#' @param ... [any]\cr
#' Further arguments, which are used for the computation of the nearest
#' prototypes (\link{findNearestPrototype}).
#' @return [\code{\link{list}(6)} of \code{\link{numeric}(1)}].\cr
#' List of features.\cr
#' For further information, see details.
#' @details
#' Each cell will be represented by an observation (of the initial design),
#' which is located closest to the cell center. Then, the objectives of three
#' neighbouring cells are compared:\cr
#' 
#' 1) If the objective of the inner prototype is bigger than the outer
#' two, a strong support for convexity is assumed.\cr
#' 2) Obviously, strong concavity is assumed, if the objective of the inner
#' prototype is below the objective of the outer two.\cr
#' 3) If the objective of the inner one is at least above the mean of the outer
#' two, there is at least light support for convexity.\cr
#' 4) Analoguously, if the objective of the inner prototype is below the mean
#' of the outer two, one assumes light support for concavity.\cr
#' 
#' In the end, all combinations of neighbouring cells are analyzed w.r.t. those
#' four cases and averaged over all combinations.\cr
#' 
#' The final two features show the amount of (additional) function
#' evaluations and running time (in seconds) that were needed for the
#' computation of these features.
#' @references
#' See Kerschke et al. (2014), \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis}, 
#'  in EVOLVE-A Bridge between Probability, Set Oriented Numerics, and Evolutionary Computation V,
#'  pp. 115-131 (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 1000, expr = runif(n = 3)))
#' feat.object = createFeatureObject(X = X, 
#'   fun = function(x) sum(x^2), blocks = c(5, 10, 4))
#'   
#' # calculate the convexity ...
#' # ... (2a) without considering diagonal neighbours:
#' calculateCellConvexity(feat.object)
#' 
#' # ... (2b) considering diagonal neighbours:
#' calculateCellConvexity(feat.object, diag = TRUE)
#' @export 
calculateCellConvexity = function(feat.object, diag = FALSE, ...) {
  assertClass(feat.object, "FeatureObject")
  assertLogical(diag)
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  measureTime(expression({
    init.grid = feat.object$init.grid
    cell.centers = feat.object$cell.centers
    obj = feat.object$objective.name
    blocks = feat.object$blocks
    if (all(blocks <= 2L)) {
      stop("The cell convexity features can only be computed when at least one dimension has more than 2 cells.")
    }
    near = findNearestPrototype(feat.object, ...)
    nb.blocks = findLinearNeighbours(near$represented.cell, blocks, diag = diag)
    convexity.counter = vapply(nb.blocks, function(cell.pairs) {
      X = near[near$represented.cell %in% cell.pairs, ]
      yvals = X[order(X$represented.cell), obj]
      counter = c(cm_conv.convex.hard = FALSE, cm_conv.concave.hard = FALSE,
        cm_conv.convex.soft = FALSE, cm_conv.concave.soft = FALSE)
      if (yvals[2] > mean(yvals[c(1, 3)])) {
        counter["cm_conv.concave.soft"] = TRUE
        if (yvals[2] > max(yvals[c(1,3)]))
          counter["cm_conv.concave.hard"] = TRUE
      } else if (yvals[2] < mean(yvals[c(1, 3)])) {
        counter["cm_conv.convex.soft"] = TRUE
        if (yvals[2] < min(yvals[c(1, 3)]))
          counter["cm_conv.convex.hard"] = TRUE
      }
      return(counter)
    }, logical(4))
    return(as.list(rowMeans(convexity.counter)))
  }), "cm_conv")
}
