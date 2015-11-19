#' @title Find Nearest Prototype
#'
#' @description
#'   For each cell of the initial design, select the closest observation to its
#'   center and use it as a representative for that cell.
#'
#' @template arg_feat_object
#' @param dist_meth [\code{\link{character}(1)}]\cr
#'   Which distance method should be used for computing the distance between
#'   two observations? All methods of \code{\link{dist}} are possible options
#'   with \code{"euclidean"} being the default.
#' @param mink_p [\code{\link{integer}(1)}]\cr
#'   Value of \code{p} in case \code{dist_meth} is \code{"minkowski"}.
#'   The default is \code{2}, i.e. the euclidean distance.
#' @param fast_k [\code{\link{numeric}(1)}]\cr
#'   Percentage of elements that should be considered within the nearest
#'   neighbour computation. The default is \code{0.05}.
#' @param ... [any]\cr
#'   Further arguments, which might be used within the distance computation
#'   (\code{\link{dist}}).
#'
#' @return [\code{\link{data.frame}}].\cr
#'   A \code{data.frame} containing one prototype (i.e. a representative
#'   observation) per cell. Each prototype consists of its values from the
#'   decision space, the corresponding objective value, its own cell ID and the
#'   cell ID of the cell, which it represents.
#'
#' @examples
#' # (1) create the initial sample and feature object:
#' X = createInitialSample(n.obs = 1000, dim = 2,
#'   control = list(init_sample.lower = -10, init_sample.upper = 10))
#' feat.object = createFeatureObject(X = X, 
#'   fun = function(x) sum(x^2), blocks = 10)
#'
#' # (2) find the nearest prototypes of all cells:
#' findNearestPrototype(feat.object)
#' @export 
findNearestPrototype = function(feat.object, dist_meth, mink_p, fast_k, ...) {
  assertClass(feat.object, "FeatureObject")
  if (missing(dist_meth))
    dist_meth = "euclidean"
  if (missing(mink_p))
    mink_p = 2L
  if (missing(fast_k))
    fast_k = 0.05
  assertNumber(fast_k)
  if (fast_k < 1)
    fast_k = ceiling(fast_k * feat.object$n.obs)
  fast_k = max(2L, fast_k)
  assertInt(fast_k, lower = 0L, upper = feat.object$n.obs)
  if ((dist_meth == "euclidean") || ((dist_meth == "minkowski") & (mink_p == 2))) {
    findNearestPrototypeQuick(feat.object, fast_k = fast_k, ...)
  } else {
    findNearestPrototypeFlex(feat.object, ...)
  }
}

findNearestPrototypeFlex = function(feat.object, ...) {
  init.grid = extractInit(feat.object)
  X = extractFeatures(feat.object)
  cell.centers = feat.object$cell.centers
  dims = feat.object$dim
  n.cells = nrow(cell.centers)
  dists = as.matrix(dist(rbind(cell.centers[, 1:dims], X)
    ), ...)[1:n.cells,-(1:n.cells)]
  nearest.grid = init.grid[apply(dists, 1, selectMin), ]
  rownames(nearest.grid) = 1:n.cells
  nearest.grid$represented.cell = cell.centers$cell.ID
  nearest.grid
}

findNearestPrototypeQuick = function(feat.object, fast_k,...) {
  assertClass(feat.object, "FeatureObject")
  init.grid = extractInit(feat.object)
  X = extractFeatures(feat.object)
  cell.centers = feat.object$cell.centers
  dims = feat.object$dim
  n.cells = nrow(cell.centers)
  nn = RANN::nn2(rbind(cell.centers[, 1:dims], X), k = fast_k)$nn.idx
  nn = nn[seq_len(n.cells), ] - n.cells
  nearest.grid = init.grid[vapply(seq_len(n.cells), function(i) {
    x = setdiff(nn[i,], i - n.cells)
    (x[x > 0])[1]
  }, integer(1L)), ]

  ## in case none of the nearest observations is a non-cell-center
  all_centers = apply(nn, 1, function(x) all(x <= 0))
  if (any(all_centers)) {
    n_ctr = sum(all_centers)
    nn_backup = RANN::nn2(rbind(cell.centers[all_centers, 1:dims], X), k = n_ctr + 1L)$nn.idx
    nn_backup = nn_backup[seq_len(n_ctr), -1, drop = FALSE] - n_ctr
    nearest.grid[all_centers, ] = init.grid[apply(nn_backup, 1, function(x) (x[x > 0])[1]), ]
  }

  rownames(nearest.grid) = seq_len(n.cells)
  nearest.grid$represented.cell = cell.centers$cell.ID
  nearest.grid
}
