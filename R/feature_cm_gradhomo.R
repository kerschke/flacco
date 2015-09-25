calculateGradientHomogeneityFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  meth = control_parameter(control, "cm_grad.dist_method", "euclidean")
  mink = control_parameter(control, "cm_grad.minkowski_p", 2)
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
  if (missing(control))
    control = list()
  assertClass(control, "list")
  tie = control_parameter(control, "cm_grad.dist_tie_breaker", "sample")
  show.warnings = control_parameter(control, "cm_grad.show_warnings", FALSE)
  meth = control_parameter(control, "cm_grad.dist_method", "euclidean")
  mink = control_parameter(control, "cm_grad.minkowski_p", 2)
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
    return(list(cm_grad.mean = mean(gradhomo, na.rm = TRUE),
      cm_grad.sd = sd(gradhomo, na.rm = TRUE)))
  }), "cm_grad")
}

## Quick Version of Gradient Homogeneity Computation;
## so far, only available for euclidean distances
calculateGradientHomogeneityQuick = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertClass(control, "list")
  tie = control_parameter(control, "cm_grad.dist_tie_breaker", "sample")
  show.warnings = control_parameter(control, "cm_grad.show_warnings", FALSE)
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
          if (nn$nn.dists[row, 2L] == 0)
            return(rep(0, length = dims))
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
    return(list(cm_grad.mean = mean(gradhomo, na.rm = TRUE),
      cm_grad.sd = sd(gradhomo, na.rm = TRUE)))
  }), "cm_grad")
}
