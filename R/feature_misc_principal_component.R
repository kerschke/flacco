calculatePrincipalComponentFeatures = function(feat.object, control = list()) {
  assertClass(feat.object, "FeatureObject")
  assertList(control)
  measureTime(expression({
    prop.cov_x = control_parameter(control, "pca.cov_x", 0.9)
    prop.cor_x = control_parameter(control, "pca.cor_x", 0.9)
    prop.cov_init = control_parameter(control, "pca.cov_init", 0.9)
    prop.cor_init = control_parameter(control, "pca.cor_init", 0.9)
    init = extractInit(feat.object)
    X = extractFeatures(feat.object)
    d = feat.object$dim
  
    explainVariance = function(data, cov = TRUE) {
      if (cov) {
        ev = eigen(cov(data))$values
      } else {
        ev = eigen(cor(data))$values
      }
      cumsum(ev) / sum(ev)
    }
  
    cov_x = explainVariance(X, cov = TRUE)
    cor_x = explainVariance(X, cov = FALSE)
    cov_init = explainVariance(init, cov = TRUE)
    cor_init = explainVariance(init, cov = FALSE)
    
    return(list(
      pca.expl_var.cov_x = min(which(cov_x >= prop.cov_x)) / d,
      pca.expl_var.cor_x = min(which(cor_x >= prop.cor_x)) / d,
      pca.expl_var.cov_init = min(which(cov_init >= prop.cov_init)) / (d + 1),
      pca.expl_var.cor_init = min(which(cor_init >= prop.cor_init)) / (d + 1),
      pca.expl_var_PC1.cov_x = cov_x[1],
      pca.expl_var_PC1.cor_x = cor_x[1],
      pca.expl_var_PC1.cov_init = cov_init[1],
      pca.expl_var_PC1.cor_init = cor_init[1]))
  }), "pca")
}
