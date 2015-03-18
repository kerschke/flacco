#' @title Calculate Gradient Homogeneity Features
#'
#' @description
#' Computes features based on the gradient homogeneity, i.e. the sum of the
#' directed and normalized gradients within a cell.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.
#' @param show.warnings [\code{\link{logical}(1)}]\cr
#' Should possible warnings about (almost) empty cells be shown? The default is
#' \code{show.warnings = TRUE}.
#' @return [\code{\link{list}(8)}].\cr
#' List of features.\cr
#' For further information, see details.
#' @details
#' Within a cell of the initial grid, the gradients from each observation to its
#' nearest observation are computed and directed towards the smaller of the two
#' objective values. Each of those directed gradients will be normalized.
#' Then, the length of the sum of all the directed and normalized gradients
#' within a cell is computed. Based on those measurements (one per cell), this
#' function computes two features:\cr
#' 
#' 1) The arithmetic mean of those length's; it should lie within [-1, +1].\cr
#' 2) Their standard deviation.
#' @references
#' See Kerschke et al. (2014), \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis}, 
#'  in EVOLVE-A Bridge between Probability, Set Oriented Numerics, and Evolutionary Computation V,
#'  pp. 115-131 (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 1000, expr = runif(n = 3)))
#' 
#' feat.object = createFeatureObject(X = X, 
#'   fun = function(x) sum(x^2), blocks = c(5, 10, 4))
#'   
#' # (2) compute the gradient homogeneity features:
#' calculateGradientHomogeneity(feat.object = feat.object)
#' @export 
calculateGradientHomogeneity = function(feat.object, show.warnings) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  init.grid = feat.object$init.grid
  dims = feat.object$dim
  cells = split(init.grid, init.grid$cell.ID)
  gradhomo = sapply(cells, function(cell) {
    n.obs = nrow(cell)
    funvals = cell[, dims + 1L]
    # 2 or less points are not helpful
    if(n.obs > 2L) { 
      dists = as.matrix(dist(cell[, 1L:dims], diag = TRUE, upper = TRUE))
      # set diagonal to large (infinite) value
      diag(dists) = Inf
      norm.vectors = sapply(1:n.obs, function(row) {
        nearest = as.integer(which.min(dists[row, ]))
        x = cell[c(row, nearest), 1L:dims]
        # compute normalized vector
        ifelse(funvals[row] > funvals[nearest], -1, 1) * apply(x, 2, diff) /
          as.numeric(dist(x))
      })
      # calculate distance of sum of normalized vectors
      sqrt(sum(rowSums(norm.vectors)^2)) / n.obs
    } else {
      NA
    }                    
  })
  if (missing(show.warnings))
    show.warnings = TRUE
  if (show.warnings && (mean(is.na(gradhomo)) > 0)) {
    warningf("%.2f%% of the cells contain less than two observations.", 
      100 * mean(is.na(gradhomo)))
  }
  return(list(gradhomo.mean = mean(gradhomo, na.rm = TRUE),
    gradhomo.sd = sd(gradhomo, na.rm = TRUE)))
}