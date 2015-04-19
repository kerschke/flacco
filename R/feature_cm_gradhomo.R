# @title Calculate Gradient Homogeneity Features
# 
# @description
# Computes features based on the gradient homogeneity, i.e. the sum of the
# directed and normalized gradients within a cell.
# @param feat.object [\code{\link{FeatureObject}}]\cr
# A feature object as created by \link{createFeatureObject}.
# @param control [\code{\link{list}}]\cr
# A list object that stores additional configuration parameters:\cr
# The element \code{gradhomo.dist_tie_breaker} defines how ties will be broken
# (using \code{selectMin}) when different observations have the same distance
# to an observation. Possible values are \code{sample}, \code{first} and
# \code{last}. The default is \code{sample}.\cr
# The parameter \code{gradhomo.show_warnings} indicates, whether possible
# warnings about (almost) empty cells should be shown? The default is
# \code{FALSE}.
# @return [\code{\link{list}(4)}].\cr
# List of features.\cr
# For further information, see details.
# @details
# Within a cell of the initial grid, the gradients from each observation to its
# nearest observation are computed and directed towards the smaller of the two
# objective values. Each of those directed gradients will be normalized.
# Then, the length of the sum of all the directed and normalized gradients
# within a cell is computed. Based on those measurements (one per cell), this
# function computes two features:\cr
# 
# 1) The arithmetic mean of those length's; it should lie within [-1, +1].\cr
# 2) Their standard deviation.\cr
# 
# The final two features show the amount of (additional) function
# evaluations and running time (in seconds) that were needed for the
# computation of these features.
# @references
# See Kerschke et al. (2014), \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis}, 
#  in EVOLVE-A Bridge between Probability, Set Oriented Numerics, and Evolutionary Computation V,
#  pp. 115-131 (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
# @examples
# # (1) create a feature object:
# X = t(replicate(n = 1000, expr = runif(n = 3)))
# 
# feat.object = createFeatureObject(X = X, 
#   fun = function(x) sum(x^2), blocks = c(5, 10, 4))
#   
# # (2) compute the gradient homogeneity features:
# calculateGradientHomogeneityFeatures(feat.object = feat.object)
# @export 
calculateGradientHomogeneityFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  meth = control_parameter(control, "gradhomo.dist_method", "euclidean")
  mink = control_parameter(control, "gradhomo.minkowski_p", 2)
  if ((meth == "euclidean") || ((meth == "minkowski") & (mink == 2))) {
    calculateGradientHomogeneityQuick(feat.object, control)
  } else {
    calculateGradientHomogeneityFlex(feat.object, control)
  }
}


## flexible version of Gradient Homogeneity computation;
## slower than the quick-version, but able to handle a lot more
## distance metrics
calculateGradientHomogeneityFlex = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  if (missing(control))
    control = list()
  assertClass(control, "list")
  tie = control_parameter(control, "gradhomo.dist_tie_breaker", "sample")
  show.warnings = control_parameter(control, "gradhomo.show_warnings", FALSE)
  meth = control_parameter(control, "gradhomo.dist_method", "euclidean")
  mink = control_parameter(control, "gradhomo.minkowski_p", 2)
  measureTime(expression({
    init.grid = feat.object$init.grid
    dims = feat.object$dim
    cells = split(init.grid, init.grid$cell.ID)
    gradhomo = vapply(cells, function(cell) {
      n.obs = nrow(cell)
      funvals = cell[, dims + 1L]
      # 2 or less points are not helpful
      if (n.obs > 2L) { 
        dists = as.matrix(dist(cell[, 1L:dims], 
          diag = TRUE, upper = TRUE, method = meth, p = mink))
        # set diagonal to large (infinite) value
        diag(dists) = Inf
        norm.vectors = vapply(1:n.obs, function(row) {
          nearest = as.integer(selectMin(dists[row, ], tie.breaker = tie))
          x = cell[c(row, nearest), 1L:dims]
          # compute normalized vector
          ifelse(funvals[row] > funvals[nearest], -1, 1) * apply(x, 2, diff) /
            as.numeric(dist(x))
        }, double(dims))
        # calculate distance of sum of normalized vectors
        sqrt(sum(rowSums(norm.vectors)^2)) / n.obs
      } else {
        NA_real_
      }                    
    }, double(1))
    if (show.warnings && (mean(is.na(gradhomo)) > 0)) {
      warningf("%.2f%% of the cells contain less than two observations.", 
        100 * mean(is.na(gradhomo)))
    }
    return(list(gradhomo.mean = mean(gradhomo, na.rm = TRUE),
      gradhomo.sd = sd(gradhomo, na.rm = TRUE)))
  }), "gradhomo")
}


## Quick Version of Gradient Homogeneity Computation;
## so far, only available for euclidean distances
calculateGradientHomogeneityQuick = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  if (missing(control))
    control = list()
  assertClass(control, "list")
  tie = control_parameter(control, "gradhomo.dist_tie_breaker", "sample")
  show.warnings = control_parameter(control, "gradhomo.show_warnings", FALSE)
  measureTime(expression({
    init.grid = feat.object$init.grid
    dims = feat.object$dim
    cells = split(init.grid, init.grid$cell.ID)
    gradhomo = vapply(cells, function(cell) {
      cell = as.matrix(cell)
      n.obs = nrow(cell)
      funvals = cell[, dims + 1L]
      # 2 or less points are not helpful
      if (n.obs > 2L) {
        nn = RANN::nn2(cell[, 1L:dims], k = 2L)
        norm.vectors = vapply(1:n.obs, function(row) {
          nearest = nn$nn.idx[row, 2L]
          # compute normalized vector
          ifelse(funvals[row] > funvals[nearest], -1, 1) * 
            (cell[nearest, 1L:dims] - cell[row, 1L:dims]) / nn$nn.dists[row, 2L]
        }, double(dims))
        # calculate distance of sum of normalized vectors
        sqrt(sum(rowSums(norm.vectors)^2)) / n.obs
      } else {
        NA_real_
      }                    
    }, double(1))
    if (show.warnings && (mean(is.na(gradhomo)) > 0)) {
      warningf("%.2f%% of the cells contain less than two observations.", 
               100 * mean(is.na(gradhomo)))
    }
    return(list(gradhomo.mean = mean(gradhomo, na.rm = TRUE),
                gradhomo.sd = sd(gradhomo, na.rm = TRUE)))
  }), "gradhomo")
}