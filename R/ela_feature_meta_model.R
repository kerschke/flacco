#' @title Calculate Meta Model Features
#' @description
#' Computes features, which describe the dependency between objective and
#' decision space by means of simple linear and quadratic models.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.
#' @return [\code{\link{list}(9)} of \code{\link{numeric}(1)}].\cr
#' List of features.\cr
#' For further information, see details.
#' @details
#' Given an initial design, linear and quadratic models of the form
#' \code{objective ~ features} are created. Both versions are created either
#' with simple interactions (e.g., \code{x1:x2}) or without interactions.\cr
#' 
#' In case of the \emph{simple linear model}, the following five features are
#' computed:\cr
#' (1) the model fit, i.e. the adjusted R^2\cr
#' (2) the estimate of the intercept\cr
#' (3) the smallest (absolute) value of the models coefficients\cr
#' (4) the biggest (absolute) value of the models coefficients\cr
#' (5) the ratio of the biggest and smallest coefficients, i.e. (4) / (5)\cr
#' 
#' The 6th feature shows the model fit (i.e., adj. R^2) for the 
#' \emph{linear model with interactions} and the 7th feature does the same
#' for a \emph{simple quadratic model}.\cr
#' Apart from that, the condition of the latter - i.e. the ratio of
#' its biggest and smallest (absolute) coefficients - is computed.\cr
#' At last, the model fit of a \emph{quadratic model with interactions} is
#' being computed.\cr
#' 
#' The final two features show the amount of (additional) function
#' evaluations and running time (in seconds) that were needed for the
#' computation of these features.
#' @references
#' See Mersmann et al. (2011), \dQuote{Exploratory Landscape Analysis} 
#' (\url{http://dx.doi.org/10.1145/2001576.2001690}).
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#' # (2) compute the meta model features
#' calculateMetaModel(feat.object)
#' @export 

calculateMetaModel = function(feat.object) {
  assertClass(feat.object, "FeatureObject")
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    df = as.data.frame(X)
    
    ## Simple linear model:
    lin.model = lm(y ~ ., data = df)
    model.coeff = coef(lin.model)
    res = list(meta.lin.simple.adj_r2 = calculateAdjustedR2(lin.model),
      meta.lin.simple.intercept = as.numeric(model.coeff[1L]),
      meta.lin.simple.coef.min = min(abs(model.coeff[-1L])),
      meta.lin.simple.coef.max = max(abs(model.coeff[-1L])),
      meta.lin.simple.coef.max_by_min = max(abs(model.coeff[-1L])) / min(abs(model.coeff[-1L]))
    )
    
    ## Linear interactions:
    lin.model = lm(y ~ .^2, data = df) ## Include interactions
    res$meta.lin.w_interact.adj_r2 = calculateAdjustedR2(lin.model)
        
    ## Simple quadratic model:
    cns = names(df)
    df = cbind(df, df^2)
    cns.squared = sprintf("%s_squared", cns)
    names(df) = c(cns, cns.squared)
    quad.model = lm(y ~ ., data = df)
    quad.model_cond = range(abs(coef(quad.model)[cns.squared]))
        
    res$meta.quad.simple.adj_r2 = calculateAdjustedR2(quad.model)
    res$meta.quad.simple.cond = quad.model_cond[2L] / quad.model_cond[1L]
    
    ## Quadratic interactions:
    quad.model_matrix = model.matrix(~ .^2, data = df)
    quad.model = lm.fit(quad.model_matrix, y)
    res$meta.quad.w_interact.adj_r2 = calculateAdjustedR2(quad.model)
    res
  }), "meta")
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
