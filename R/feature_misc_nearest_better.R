calculateNearestBetterFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  meth = control_parameter(control, "nbc.dist_method", "euclidean")
  mink = control_parameter(control, "nbc.minkowski_p", 2)
  if ((meth == "euclidean") || ((meth == "minkowski") & (mink == 2))) {
    calculateNearestBetterQuickFeatures(feat.object, control)
  } else {
    calculateNearestBetterFlexFeatures(feat.object, control)
  }
}

## Quick version of Nearest Better Features
## so far only available for euclidean distances
calculateNearestBetterQuickFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    cor_na = control_parameter(control, "nbc.cor_na", "pairwise.complete.obs")
    fast_k = control_parameter(control, "nbc.fast_k", 0.05)
    tie.breaker = control_parameter(control, "nbc.dist_tie_breaker", "sample")
    assertNumber(fast_k)
    if (fast_k < 1)
      fast_k = ceiling(fast_k * feat.object$n.obs) 
    assertInt(fast_k, lower = 0L, upper = feat.object$n.obs)
    X = extractFeatures(feat.object)
    y = ifelse(feat.object$minimize, 1, -1) * extractObjective(feat.object)
    nn = RANN::nn2(X, k = fast_k)
    nb.stats = t(vapply(1:nrow(X), function(i) {
      y_rec = y[i]
      ind_nn = nn$nn.idx[i, -1]
      y_near = y[ind_nn]
      better = (y_near < y_rec)
      if (any(better)) {
        j = min(which(better))
        return(c(i, ind_nn[j], nn$nn.dists[i, j + 1L]))
      } else {
        ind_alt = setdiff(1:nrow(X), nn$nn.idx[i,])
        if (any(y[ind_alt] < y_rec)) {
          ind_alt = ind_alt[which(y[ind_alt] < y_rec)]
        } else if (any(y[ind_alt] == y_rec)) {
          ind_alt = ind_alt[which(y[ind_alt] == y_rec)]
        } else {
          return(c(i, NA_real_, NA_real_))
        }
        if (length(ind_alt) == 1) {
          return(c(i, ind_alt, sqrt(sum((X[ind_alt, ] - X[i, ])^2))))
        } else {
          d = apply(X[ind_alt, ], 1, function(x) sqrt(sum((x - X[i, ])^2)))
          j = selectMin(d, tie.breaker = tie.breaker)
          return(c(i, ind_alt[j], d[j]))
        }
      }
    }, double(3L)))
    colnames(nb.stats) = c("ownID", "nbID", "nbDist")
    nb.stats = as.data.frame(nb.stats)
    nb.stats$ownID = as.integer(nb.stats$ownID)
    nb.stats$nbID = as.integer(nb.stats$nbID)
    nb.stats$nearDist = nn$nn.dists[, 2L]
    nb.stats$nb_near_ratio = nb.stats$nbDist / nb.stats$nearDist
    nb.stats$fitness = y

    to.me.stats = t(vapply(nb.stats$ownID, function(row) {
      x = which(nb.stats$nbID == nb.stats$ownID[row])
      # number of elements, which have ownID[row] as nearest better
      count = length(x)
      # median distance to the elements, to which ownID[row] is nearest better
      toMe_dist = median(nb.stats$nbDist[x])
      if (count > 0L) {
        return(c(count, toMe_dist, nb.stats$nbDist[row] / toMe_dist))
      } else {
        return(c(0, NA_real_, NA_real_))
      }
    }, double(3)))

    colnames(to.me.stats) = c("toMe_count", "toMe_dist_median", "nb_median_toMe_ratio")
    nb.stats = cbind(nb.stats, to.me.stats)
    nb.stats$toMe_count = as.integer(nb.stats$toMe_count)

    nn.dists = nb.stats$nearDist
    nb.dists = nb.stats$nbDist
    # cure global optima
    nb.dists[is.na(nb.stats$nbID)] = nn.dists[is.na(nb.stats$nbID)]
    dist_ratio = nn.dists / nb.dists
    return(list(
      nbc.nn_nb.sd_ratio = sd(nn.dists, na.rm = TRUE) / sd(nb.dists, na.rm = TRUE),
      nbc.nn_nb.mean_ratio = mean(nn.dists, na.rm = TRUE) / mean(nb.dists, na.rm = TRUE),
      nbc.nn_nb.cor = cor(nn.dists, nb.dists, use = cor_na),
      nbc.dist_ratio.coeff_var = 
        sd(dist_ratio, na.rm = TRUE) / mean(dist_ratio, na.rm = TRUE),
      nbc.nb_fitness.cor = cor(nb.stats$toMe_count, y, use = cor_na)
    ))
  }), "nbc")
}

## Flexible version of Nearest Better Features
## slower than the quick version, but able to handle other
## distance metrics as well.
calculateNearestBetterFlexFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    meth = control_parameter(control, "nbc.dist_method", "euclidean")
    mink = control_parameter(control, "nbc.minkowski_p", 2)
    cor_na = control_parameter(control, "nbc.cor_na", "pairwise.complete.obs")
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
      nbc.nn_nb.sd_ratio = sd(nn.dists, na.rm = TRUE) / sd(nb.dists, na.rm = TRUE),
      nbc.nn_nb.mean_ratio = mean(nn.dists, na.rm = TRUE) / mean(nb.dists, na.rm = TRUE),
      nbc.nn_nb.cor = cor(nn.dists, nb.dists, use = cor_na),
      nbc.dist_ratio.coeff_var = 
        sd(dist_ratio, na.rm = TRUE) / mean(dist_ratio, na.rm = TRUE),
      nbc.nb_fitness.cor = cor(nb.stats$toMe_count, y, use = cor_na)
    ))
  }), "nbc")
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
