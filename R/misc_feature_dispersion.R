#' @title Calculate dispersion features
#'
#' @description
#' Computes features based on the comparison of the dispersion of distances
#' among the 'best' elements and the entire initial design.
#' @param feat.object [\code{FeatureObject}]\cr
#'   A feature object as created by createFeatureObject.
#' @param control [\code{list}]\cr
#'   A list object that stores additional configuration parameters.
#' @return [\code{list} of \code{numeric(1)}].\cr
#'   List of features.\cr
#'   The ratios and differences between the mean (and median) distances among 
#'   the distances of the 'best' and all elements.
#' @examples
#'   # (1) create the initial design:
#'   X = t(replicate(1000, runif(2, -10, 10)))
#'   y = apply(X, 1, function(x) sum(x^2))
#'   feat.object = createFeatureObject(X = X, y = y)
#'   # (2) compute the dispersion features:
#'   calculateDispersion(feat.object)
#' @export 
calculateDispersion = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
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
  means = sapply(dists, mean)
  medians = sapply(dists, median)
  res = c(means / mean(dists.full_sample), medians / median(dists.full_sample),
    means - mean(dists.full_sample), medians - median(dists.full_sample))
  res = as.vector(res, mode = "list")
  names(res) = c(sprintf("disp.ratio_mean_%02i", quantiles * 100), 
    sprintf("disp.ratio_median_%02i", quantiles * 100),
    sprintf("disp.diff_mean_%02i", quantiles * 100), 
    sprintf("disp.diff_median_%02i", quantiles * 100))
  return(res)
}
