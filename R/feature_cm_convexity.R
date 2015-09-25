calculateCellConvexityFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  if (missing(control))
    control = list()
  assertList(control)
  diag = control_parameter(control, "cm_conv.diag", FALSE) 
  meth = control_parameter(control, "cm_conv.dist_method", "euclidean")
  mink = control_parameter(control, "cm_conv.minkowski_p", 2)
  fast_k = control_parameter(control, "cm_conv.fast_k", 0.05)
  measureTime(expression({
    init.grid = feat.object$init.grid
    cell.centers = feat.object$cell.centers
    obj = feat.object$objective.name
    blocks = feat.object$blocks
    if (any(blocks <= 2L)) {
      stop("The cell convexity features can only be computed when all dimensions have more than 2 cells.")
    }
    near = findNearestPrototype(feat.object, dist_meth = meth, mink_p = mink, fast_k = fast_k, ...)
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
