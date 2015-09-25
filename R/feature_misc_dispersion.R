calculateDispersionFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    if (!feat.object$minimize)
      y = -1 * y
    quantiles = control_parameter(control, "disp.quantiles", 
      c(0.02, 0.05, 0.1, 0.25))
    meth = control_parameter(control, "disp.dist_method", "euclidean")
    index = lapply(quantile(y, quantiles), 
      function(quant) which(y <= quant))
    if (meth != "minkowski") {
      dists = lapply(seq_along(index), 
        function(i) as.numeric(dist(X[index[[i]], ], method = meth)))  
      dists.full_sample = as.numeric(dist(X, method = meth))
    } else {
      mink = control_parameter(control, "disp.minkowski_p", 2)
      dists = lapply(seq_along(index), 
        function(i) as.numeric(dist(X[index[[i]], ], method = meth, p = mink)))
      dists.full_sample = as.numeric(dist(X, method = meth, p = mink))
    }
    means = vapply(dists, mean, double(1))
    medians = vapply(dists, median, double(1))
    res = c(means / mean(dists.full_sample), medians / median(dists.full_sample),
      means - mean(dists.full_sample), medians - median(dists.full_sample))
    res = as.vector(res, mode = "list")
    names(res) = c(sprintf("disp.ratio_mean_%02i", quantiles * 100), 
      sprintf("disp.ratio_median_%02i", quantiles * 100),
      sprintf("disp.diff_mean_%02i", quantiles * 100), 
      sprintf("disp.diff_median_%02i", quantiles * 100))
    return(res)
  }), "disp")
}
