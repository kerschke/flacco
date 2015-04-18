# @title Calculate Linear Model Coefficient Features
# 
# @description
# Linear models are computed per cell, provided the decision space is divided
# into a grid of cells. Each one of the models has the form
# \code{objective ~ features}.
# @param feat.object [\code{\link{FeatureObject}}]\cr
# A feature object as created by \link{createFeatureObject}.
# @return [\code{\link{list}(14)} of \code{\link{numeric}(1)}].\cr
# List of features.\cr
# For further information, see details.
# @details
# The length of the average coefficient vector, i.e. the vector consisting of
# the average of the coefficients (per feature and over all cells), is
# computed based on the\cr
# (1) regular coefficient vectors.\cr
# (2) normalized coefficient vectors.\cr
# 
# For each cell, the length of the coefficient vector is computed and
# afterwards aggregated using the\cr
# (3) arithmetic mean.\cr
# (4) standard deviation.\cr
# 
# The correlation of all coefficient vectors is computed (w.r.t. the
# features) and aggregated using the arithmetic mean based on the\cr
# (5) regular coefficient vectors.\cr
# (6) normalized coefficient vectors.\cr
# 
# The ratios of (absolute) maximum and minimum coefficients are computed (per
# cell) and afterwards aggregated by the\cr
# (7) arithmetic mean.\cr
# (8) standard deviation.\cr
# 
# The standard deviation is computed (per feature) over all cells. Then, the
# ratio of biggest and smallest standard deviation is computed based on the\cr
# (9) regular coefficient vectors.\cr
# (10) normalized coefficient vectors.\cr
# 
# The same standard deviations, which have been used in the previous two
# features, are aggregated by the\cr
# (11) arithmetic mean.\cr
# (12) standard deviation.
# 
# The final two features show the amount of (additional) function
# evaluations and running time (in seconds) that were needed for the
# computation of these features.
# @examples
# # (1) create a feature object:
# X = t(replicate(1000, runif(n = 3)))
# feat.object = createFeatureObject(X = X, 
#   fun = function(x) sum(x^2), blocks = c(5, 10, 4))
#   
# # (2) compute the linear modell coefficient features:
# calculateLinearModelFeatures(feat.object = feat.object)
# @export 
calculateLinearModelFeatures = function(feat.object) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  measureTime(expression({
    init.grid = feat.object$init.grid
    dims = feat.object$dim
    cells = split(init.grid, init.grid$cell.ID)
    dep = paste(feat.object$feature.names, collapse = " + ")
    form = as.formula(paste(feat.object$objective.name, dep, sep = " ~ "))
    if (max(vapply(cells, nrow, integer(1))) <= dims) {
      result = as.list(rep(NA_real_, 12L))
      names(result) = c("limo.avg.length", "limo.avg.length.scaled", "limo.length.mean",
        "limo.length.sd", "limo.cor", "limo.cor.scaled", "limo.ratio.mean",
        "limo.ratio.sd", "limo.sd.max_min_ratio", "limo.sd.max_min_ratio.scaled",
        "limo.sd.mean", "limo.sd.mean.scaled")
      return(result)
    }    
    ## vectors of coefficients (except for intercept)
    coeff.vect = vapply(cells, function(cell) {
      if (nrow(cell) > dims) {
        as.numeric(lm(form, data = cell)$coeff[-1])
      } else {
        rep(NA_real_, dims)
      }
    }, double(dims))
    ## ratio of biggest to smallest absolute coefficient (per cell)
    coeff.ratio = apply(coeff.vect, 2, function(x) {
      if (all(is.na(x))) {
        return(NA_real_)
      } else {
        max(abs(x), na.rm = TRUE) / min(abs(x), na.rm = TRUE)
      }
    })
    ## normalized vectors of coefficients
    norm.coeff.vect = apply(coeff.vect, 2, normalizeVector)
    ## length of each single coefficient vector
    length.coeff.vects = apply(coeff.vect, 2, function(x) sqrt(sum(x^2)))
    ## sd of coefficients (per feature)
    sds.unscaled = apply(coeff.vect, 1, sd, na.rm = TRUE)
    ## sd of normalized coefficients (per feature)
    sds.scaled = apply(norm.coeff.vect, 1, sd, na.rm = TRUE)
    ## correlation between coefficients
    cor.unscaled = cor(t(coeff.vect), use = "pairwise.complete.obs")
    diag(cor.unscaled) = NA_real_
    ## correlation between coefficients
    cor.scaled = cor(t(norm.coeff.vect), use = "pairwise.complete.obs")
    diag(cor.scaled) = NA_real_
    return(list(limo.avg.length = 
        sqrt(sum((rowMeans(coeff.vect, na.rm = TRUE))^2)),
      limo.avg.length.scaled = 
        sqrt(sum((rowMeans(norm.coeff.vect, na.rm = TRUE))^2)),
      limo.length.mean = mean(length.coeff.vects, na.rm = TRUE),
      limo.length.sd = sd(length.coeff.vects, na.rm = TRUE),
      limo.cor = mean(cor.unscaled, na.rm = TRUE),
      limo.cor.scaled = mean(cor.scaled, na.rm = TRUE),
      limo.ratio.mean = mean(coeff.ratio, na.rm = TRUE),
      limo.ratio.sd = sd(coeff.ratio, na.rm = TRUE),
      limo.sd.max_min_ratio = max(sds.unscaled) / min(sds.unscaled),
      limo.sd.max_min_ratio.scaled = max(sds.scaled) / min(sds.scaled),
      limo.sd.mean = mean(sds.unscaled),
      limo.sd.mean.scaled = mean(sds.scaled)))
  }), "limo")
}