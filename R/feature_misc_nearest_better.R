# @title Calculate nearest better features
# 
# @description
# Computes 5 features based on the comparison of nearest neighbor and nearest
# better neighbor, i.e., the nearest neighbor with a better performance value.
# @param feat.object [\code{FeatureObject}]\cr
#   A feature object as created by createFeatureObject.
# @param control [\code{list}]\cr
#   A list object that stores additional configuration parameters.
# @return [\code{list(7)} of \code{numeric(1)}].\cr
#   List of features.\cr
#   The first feature is the ratio of the standard deviations between the
#   distances of the nearest neighbors and the nearest better neighbors. The
#   second feature works analoguous to the first one, but uses the mean
#   instead of the standard deviation. The correlation between the two
#   distance types is the third feature. The fourth feature gives the 
#   coefficient of variation of the ratio of the distances and the fifth
#   feature computes the correlation between the fitness values and the
#   count of observations, to which an observation is the nearest better.
#   The final two features show the amount of (additional) function
#   evaluations and running time (in seconds) that were needed for the
#   computation of these features.
# @examples
#   # (1) create the initial design:
#   X = t(replicate(1000, runif(2, -10, 10)))
#   y = apply(X, 1, function(x) sum(x^2))
#   feat.object = createFeatureObject(X = X, y = y)
#   # (2) compute the gradient homogeneity features:
#   calculateNearestBetterFeatures(feat.object = feat.object)
# @export 
calculateNearestBetterFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    meth = control_parameter(control, "nbf.dist_method", "euclidean")
    mink = control_parameter(control, "nbf.minkowski_p", 2)
    cor_na = control_parameter(control, "nbf.cor_na", "pairwise.complete.obs")
    init = feat.object$env$init
    X = init[, feat.object$feature.names]
    y = init[, feat.object$objective.name]
    if (meth != "minkowski") {
      distmat = as.matrix(dist(X, method = meth,
        diag = TRUE, upper = TRUE))  
    } else {
      distmat = as.matrix(dist(X, method = meth,
        p = mink, diag = TRUE, upper = TRUE))  
    }
    nb.stats = computeNearestBetterStats(distmat = distmat,
      objectives = ifelse(feat.object$minimize, 1, -1) * y)
    nn.dists = nb.stats$nearDist
    nb.dists = nb.stats$nbDist
    # cure global optima
    nb.dists[is.na(nb.stats$nbID)] = nn.dists[is.na(nb.stats$nbID)]
    dist_ratio = nn.dists / nb.dists
    return(list(
      nb.nn_nb.sd_ratio = sd(nn.dists, na.rm = TRUE) / sd(nb.dists, na.rm = TRUE),
      nb.nn_nb.mean_ratio = mean(nn.dists, na.rm = TRUE) / mean(nb.dists, na.rm = TRUE),
      nb.nn_nb.cor = cor(nn.dists, nb.dists, use = cor_na),
      nb.dist_ratio.coeff_var = 
        sd(dist_ratio, na.rm = TRUE) / mean(dist_ratio, na.rm = TRUE),
      nb.nb_fitness.cor = cor(nb.stats$toMe_count, y, use = cor_na)
    ))
  }), "nb")
}

# compute various distance measures and ratios wrt the nearest and nearest
# better elements:
computeNearestBetterStats = function(distmat, objectives) {
  result = data.frame(ownID = 1:nrow(distmat))
  result = cbind(result, t(vapply(result$ownID, function(row) {
    rowDists = as.numeric(distmat[row, ])
    # first look for elements with better fitness
    better = which(objectives < objectives[row])
    # if no better elements are available check for elements with equal fitness
    if (length(better) == 0L) {
      better = which(objectives == objectives[row])
      better = setdiff(better, row)
    }
    # select the nearest among the (equal-or-)better elements
    if (length(better) > 0L) {
      nb = better[selectMin(rowDists[better])]
      return(c(nbID = nb, nbDist = rowDists[nb], nearDist = min(rowDists[-row])))
    } else {
      return(c(nbID = NA_real_, nbDist = NA_real_, nearDist = min(rowDists[-row])))
    }
  }, double(3))))
  # compute ratio of nearestBetter and nearest
  result$nb_near_ratio = result$nbDist / result$nearDist
  result$fitness = objectives
  result = cbind(result, t(vapply(result$ownID, function(row) {
    x = which(result$nbID == result$ownID[row])
    # number of elements, which have ownID[row] as nearest better
    count = length(x)
    # median distance to the elements, to which ownID[row] is nearest better
    toMe_dist = median(result$nbDist[x])
    if (count > 0L) {
      return(c(toMe_count = count, toMe_dist_median = toMe_dist,
        nb_median_toMe_ratio = result$nbDist[row] / toMe_dist))
    } else {
      return(c(toMe_count = 0, toMe_dist_median = NA_real_, 
        nb_median_toMe_ratio = NA_real_))
    }
  }, double(3))))
  return(result)
}
