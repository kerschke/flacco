# @title Calculate gradient homogeneity features for a given cell grid
# 
# @description
# Computes 2 features based on the gradient homogeneity, i.e. within a cell
# of the initial grid, one computes the gradients from each observation to its
# nearest observations. The gradients will always be directed towards the
# smaller objective value. Each of the directed gradients will be normalized.
# Afterwards one computes the length of the sum of all normalized gradients
# and scales it by the number of gradients. In the end, the mean and standard
# deviation over all those sums will be returned.
# @param feat.object [\code{FeatureObject}]\cr
#   A feature object as created by createFeatureObject.
# @param control [\code{list}]\cr
#   A list object that stores additional configuration parameters.
#   Here, the parameters pca.cov_x and pca.cor_x indicate, which proportion
#   of the variance should be explained by the PCs based on the feature space X.
#   Analoguous, pca.cov_init and pca.cor_init do the same - based on the
#   initial design, i.e., the combination of the feature and objective space.
# @return [\code{list(10)} of \code{numeric(1)}].\cr
#   List of features.\cr
#   The first four features return the proportion of PCs that are needed to
#   explain the required proportion of the variance. The first one corresponds
#   to the PCA of the feature space based on the covariance matrix. The second
#   does the same, but is based on the correlation matrix. The third and
#   fourth feature are computed analoguous - but are based on the entire 
#   initial design, i.e., the combination of feature and objective space.
#   The next four features are based on the same categories and indicate
#   which proportion of the variance is explained by the first PC.
#   The final two features show the amount of (additional) function
#   evaluations and running time (in seconds) that were needed for the
#   computation of these features.
# @examples
#   # (1) create the initial design:
#   X = t(replicate(1000, runif(2, -10, 10)))
#   # (2) create a feature object
#   feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#   # (3) compute the gradient homogeneity features:
#   calculatePrincipalComponentFeatures(feat.object = feat.object)
# @export 
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
      pc.expl_var.cov_x = min(which(cov_x >= prop.cov_x)) / d,
      pc.expl_var.cor_x = min(which(cor_x >= prop.cor_x)) / d,
      pc.expl_var.cov_init = min(which(cov_init >= prop.cov_init)) / (d + 1),
      pc.expl_var.cor_init = min(which(cor_init >= prop.cor_init)) / (d + 1),
      pc.expl_var_PC1.cov_x = cov_x[1],
      pc.expl_var_PC1.cor_x = cor_x[1],
      pc.expl_var_PC1.cov_init = cov_init[1],
      pc.expl_var_PC1.cor_init = cor_init[1]))
  }), "pc")
}
