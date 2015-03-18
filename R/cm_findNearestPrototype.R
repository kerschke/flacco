#' @title Find the Nearest Prototype
#'
#' @description
#' Among the initial design, a representing observation for each of the cells
#' is selected.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.
#' @param ... [any]\cr
#' Further arguments, which might be used within the distance computation
#' (\code{\link{dist}}).
#' @return [\code{\link{data.frame}}].\cr
#' A \code{data.frame}, which consists of one prototype (i.e. a
#' representative observation) per cell. Each prototype consists of the
#' features, the corresponding objective value, its own cell ID and the cell
#' ID of the cell, which it represents.
#' @examples
#' # (1) create the initial design:
#' X = t(replicate(1000, runif(2, -10, 10)))
#' feat.object = createFeatureObject(X = X, 
#'   fun = function(x) sum(x^2), 
#'   lower = -10, upper = 10, blocks = 10)
#' # (2) find the nearest prototypes of all cells:
#' findNearestPrototype(feat.object)
#' @export 
findNearestPrototype = function(feat.object, ...) {
  assertClass(feat.object, "FeatureObject")
  init.grid = feat.object$init.grid
  X = init.grid[, feat.object$feature.names]
  cell.centers = feat.object$cell.centers
  dims = feat.object$dim
  n.cells = nrow(cell.centers)
  n.obs = feat.object$n.obs
  dists = as.matrix(dist(rbind(cell.centers[, 1:dims], X)
    ), ...)[1:n.cells,-(1:n.cells)]
  nearest.grid = init.grid[apply(dists, 1, selectMin), ]
  rownames(nearest.grid) = 1:n.cells
  nearest.grid$represented.cell = cell.centers$cell.ID
  nearest.grid
}
