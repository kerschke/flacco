calculateLinearModelFeatures = function(feat.object) {
  assertClass(feat.object, "FeatureObject")
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
    if (all(is.na(cor.unscaled))) {
      limo.cor = NA_real_
    } else {
      limo.cor = mean(cor.unscaled, na.rm = TRUE)
    }
    ## correlation between coefficients
    cor.scaled = cor(t(norm.coeff.vect), use = "pairwise.complete.obs")
    diag(cor.scaled) = NA_real_
    if (all(is.na(cor.scaled))) {
      limo.cor.scaled = NA_real_
    } else {
      limo.cor.scaled = mean(cor.scaled, na.rm = TRUE)
    }
    return(list(limo.avg_length.reg = 
        sqrt(sum((rowMeans(coeff.vect, na.rm = TRUE))^2)),
      limo.avg_length.norm = 
        sqrt(sum((rowMeans(norm.coeff.vect, na.rm = TRUE))^2)),
      limo.length.mean = mean(length.coeff.vects, na.rm = TRUE),
      limo.length.sd = sd(length.coeff.vects, na.rm = TRUE),
      limo.cor.reg = limo.cor,
      limo.cor.norm = limo.cor.scaled,
      limo.ratio.mean = mean(coeff.ratio, na.rm = TRUE),
      limo.ratio.sd = sd(coeff.ratio, na.rm = TRUE),
      limo.sd_ratio.reg = max(sds.unscaled) / min(sds.unscaled),
      limo.sd_ratio.norm = max(sds.scaled) / min(sds.scaled),
      limo.sd_mean.reg = mean(sds.unscaled),
      limo.sd_mean.norm = mean(sds.scaled)))
  }), "limo")
}
