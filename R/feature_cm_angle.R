# @title Calculate Angle and Distance Features
# 
# @description
# Computes features based on the best and worst observation within a cell,
# provided the decision space is divided into a grid of cells.
# @param feat.object [\code{\link{FeatureObject}}]\cr
# A feature object as created by \link{createFeatureObject}.
# @param control [\code{\link{list}}]\cr
# A list object that stores additional configuration parameters.\cr
# The element \code{angle.show_warnings} indicates whether possible warnings
# about \code{NAs} in the feature computation should be shown?
# The default is \code{TRUE}.
# @return [\code{\link{list}(10)} of \code{\link{numeric}(1)}].\cr
# List of features.\cr
# For further information, see details.
# @details
# The features are based on the location of the worst and best element within
# each cell. To be precise, their distance to the cell center and the angle
# between these three elements (at the center) are the foundation of those
# eight features: \cr
# 
# (1) Arithmetic mean of the distances from the cell center to the best
# observation within the cell (over all cells).\cr
# (2) Standard deviation of the distances from (1).\cr
# (3) Arithmetic mean of the distances from the cell
# center to the worst observations within a cell.\cr
# (4) Standard deviation of the distances from (3).\cr
# (5) Arithmetic mean of the angles (in degree) between worst, center
# and best element of a cell.\cr
# (6) Standard deviation of the angles from (5).\cr
# (7) Arithmetic mean of the ratios between the distance of the worst and
# best element within a cell and the worst and best element in the entire
# initial design. The distances are only based on the objective space.\cr
# (8) Standard deviation of the ratios from (7).\cr
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
# X = t(replicate(1000, runif(2, -10, 10)))
# y = apply(X, 1, function(x) sum(x^2))
# feat.object = createFeatureObject(X = X, y = y, 
#   lower = -10, upper = 10, blocks = 10)
#     
# # (2) compute the angle and distance features:
# library(plyr)
# calculateAngleFeatures(feat.object)
# @export 
calculateAngleFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    show.warnings = control_parameter(control, "angle.show_warnings", TRUE)
    init.grid = feat.object$init.grid
    ft.names = feat.object$feature.names
    cell.centers = as.matrix(feat.object$cell.centers)[, ft.names]
    obj = feat.object$objective.name
    if (!feat.object$minimize)
      init.grid[, obj] = -1 * init.grid[, obj]
    grid.best = as.matrix(plyr::ddply(init.grid, "cell.ID", function(x) x[selectMin(x[, obj]), ]))
    grid.worst = as.matrix(plyr::ddply(init.grid, "cell.ID", function(x) x[selectMax(x[, obj]), ]))
    y.global.worst = max(grid.worst[, obj], na.rm = TRUE)
    y.global.best = min(grid.best[, obj], na.rm = TRUE)
    non_empty = sort.int(unique(grid.best[, "cell.ID"]), method = "quick")
    no_total = as.integer(prod(feat.object$blocks))
    no_empty = as.integer(no_total - length(non_empty))
    if (no_total == 1) {
      cell.centers = t(cell.centers)
    }
    ## only consider non-empty cells
    cell.values = vapply(seq_len(nrow(grid.worst)), function(i) {
      x.center = as.numeric(cell.centers[i, ft.names])
      x.worst = as.numeric(grid.worst[i, ft.names])
      x.best = as.numeric(grid.best[i, ft.names])
      y.local.worst = as.numeric(grid.worst[i, obj])
      y.local.best = as.numeric(grid.best[i, obj])
      b2w.ratio = (y.local.worst - y.local.best) / 
      (y.global.worst - y.global.best)
      c2b.vect = x.best - x.center
      c2b.dist = as.numeric(sqrt(crossprod(as.numeric(c2b.vect))))
      c2w.vect = x.worst - x.center
      c2w.dist = as.numeric(sqrt(crossprod(as.numeric(c2w.vect))))
      x = as.numeric(crossprod(c2b.vect, c2w.vect)) / (c2b.dist * c2w.dist)
      if (all(x.worst == x.best)) {
        angle = 0
      } else {
        angle = acos(x) * 180 / pi
      }      
      return(c(c2b.dist = c2b.dist, c2w.dist = c2w.dist, 
        angle = angle, b2w.ratio = b2w.ratio))
    }, double(4))
    if (show.warnings && (no_empty > 0L)) {
      warningf("%.2f%% of the cells produce NAs during the feature computation.", 
        100 * no_empty / no_total)
    }
    return(list(cm_angle.dist_ctr2best.mean = mean(cell.values["c2b.dist", ]),
      cm_angle.dist_ctr2best.sd = sd(cell.values["c2b.dist", ]),
      cm_angle.dist_ctr2worst.mean = mean(cell.values["c2w.dist", ]),
      cm_angle.dist_ctr2worst.sd = sd(cell.values["c2w.dist", ]),
      cm_angle.angle.mean = mean(cell.values["angle", ]),
      cm_angle.angle.sd = sd(cell.values["angle", ]),
      cm_angle.y_ratio_best2worst.mean = mean(cell.values["b2w.ratio", ]),
      cm_angle.y_ratio_best2worst.sd = sd(cell.values["b2w.ratio", ])))
  }), "cm_angle")
}