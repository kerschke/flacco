# @title Calculate Levelset Features
# @description
# Computes features quantifying the levelset of a function (its objective
# space to be precise).
# @param feat.object [\code{\link{FeatureObject}}]\cr
# A feature object as created by \link{createFeatureObject}.\cr
# @param control [\code{\link{list}}]\cr
# A list object that stores additional configuration parameters:\cr
# The element \code{levelset.quantiles} defines the quantiles, which are used
# for splitting the objective space. The default is \code{c(10, 20, 50)}.\cr
# Also, the classification methods can be defined using
# \code{levelset.classif_methods}. The corresponding default is
# \code{c("lda", "qda", "mda")}.\cr
# In addition, the resampling technique and the corresponding number of
# iterations can be defined using \code{levelset.resample_method} (default is 
# \code{"CV"}) and \code{levelset.resample_iterations} (default is 
# \code{10L}).\cr
# Finally, one can define whether information regarding the resampling process
# should be printed to the console (\code{levelset.show_info}). The default is
# \code{FALSE}.
# @return [\code{\link{list}(20)} of \code{\link{numeric}(1)}].\cr
# List of features.\cr
# For further information, see details.
# @details
# For each pair of classification method and quantile, the mean
# misclassfication error (mmce) is returned. In addition, for each quantile
# and pair of classification methods the ratio of the mmces of the latter is
# returned.\cr
# 
# The final two features show the amount of (additional) function
# evaluations and running time (in seconds) that were needed for the
# computation of these features.
# @examples
# # (1) create a feature object:
# X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
# feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
# 
# # (2) compute the levelset features
# library(mlr)
# calculateLevelsetFeatures(feat.object)
# @export 
calculateLevelsetFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    probs = control_parameter(control, "levelset.quantiles", 
      c(0.10, 0.25, 0.5))
    ## FIXME: what about svm, rpart, etc?!
    methods = control_parameter(control, "levelset.classif_methods", 
      c("lda", "qda", "mda"))
    show.info = control_parameter(control, "levelset.show_info", FALSE)
    res.iters = control_parameter(control, "levelset.resample_iterations", 10L)
    res.meth = control_parameter(control, "levelset.resample_method", "CV")
    colnames(X) = paste0("x", 1:ncol(X))
    desc = mlr::makeResampleDesc(res.meth, iters = res.iters)
    inst = mlr::makeResampleInstance(desc, size = nrow(X))
    result = vapply(probs, function(prob) {
      y_quant = quantile(y, prob)
      data = data.frame(class = as.factor(y < y_quant), X)
      task = mlr::makeClassifTask(id = "prob", data = data, target = "class")
      mmces = vapply(methods, function(method) {
        lrn = mlr::makeLearner(paste("classif.", method, sep = ""))
        mlr::resample(learner = lrn, task = task, 
          resampling = inst, show.info = show.info)$aggr["mmce.test.mean"]
      }, double(1))
      names(mmces) = paste("mmce", methods, sep = "_")
      if (length(methods) > 1) {
        combis = combn(paste("mmce", methods, sep = "_"), 2)
        ratios = apply(combis, 2, function(x) mmces[x[1]] / mmces[x[2]])
        combis = combn(methods, 2)
        names(ratios) = apply(combis, 2, function(x) paste0(x[1], "_", x[2]))
        mmces = c(mmces, ratios)
      }    
      return(mmces)
    }, double(choose(length(methods), 2) + length(methods)))
    meth.names = rownames(result)
    result = as.vector(result, mode = "list")
    names(result) = sprintf("lvlset.%s_%02i", rep(meth.names, length(probs)), 
      rep(100 * probs, each = length(meth.names)))
    return(result)
  }), "lvlset")
}