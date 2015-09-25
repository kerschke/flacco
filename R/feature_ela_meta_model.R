calculateMetaModelFeatures = function(feat.object) {
  assertClass(feat.object, "FeatureObject")
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    df = as.data.frame(X)
    
    ## Simple linear model:
    lin.model = lm(y ~ ., data = df)
    model.coeff = coef(lin.model)
    res = list(ela_meta.lin_simple.adj_r2 = calculateAdjustedR2(lin.model),
      ela_meta.lin_simple.intercept = as.numeric(model.coeff[1L]),
      ela_meta.lin_simple.coef.min = min(abs(model.coeff[-1L])),
      ela_meta.lin_simple.coef.max = max(abs(model.coeff[-1L])),
      ela_meta.lin_simple.coef.max_by_min = max(abs(model.coeff[-1L])) / min(abs(model.coeff[-1L]))
    )
    
    ## Linear interactions:
    lin.model = lm(y ~ .^2, data = df) ## Include interactions
    res$ela_meta.lin_w_interact.adj_r2 = calculateAdjustedR2(lin.model)
        
    ## Simple quadratic model:
    cns = names(df)
    df = cbind(df, df^2)
    cns.squared = sprintf("%s_squared", cns)
    names(df) = c(cns, cns.squared)
    quad.model = lm(y ~ ., data = df)
    quad.model_cond = range(abs(coef(quad.model)[cns.squared]))
        
    res$ela_meta.quad_simple.adj_r2 = calculateAdjustedR2(quad.model)
    res$ela_meta.quad_simple.cond = quad.model_cond[2L] / quad.model_cond[1L]
    
    ## Quadratic interactions:
    quad.model_matrix = model.matrix(~ .^2, data = df)
    quad.model = lm.fit(quad.model_matrix, y)
    res$ela_meta.quad_w_interact.adj_r2 = calculateAdjustedR2(quad.model)
    res
  }), "ela_meta")
}

## Calculate adjusted R^2
calculateAdjustedR2 = function(mod) {
  pred = fitted(mod)
  resi = residuals(mod)
  SS_reg = crossprod(pred - mean(pred))
  SS_res = crossprod(resi)
  SS_total = SS_reg + SS_res
  n = mod$df.residual
  p = mod$rank
  drop(1 - (SS_res / SS_total) / ((n - p - 1) / (n - 1)))
}
