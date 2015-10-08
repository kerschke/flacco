calculateLevelsetFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  assertList(control)
  parallelize = control_parameter(control, "ela_level.parallelize", FALSE)
  assertLogical(parallelize, len = 1L)
  if (parallelize) {
    on.exit(parallelMap::parallelStop())
    parallel.mode =
      control_parameter(control, "ela_level.parallel.mode", "local")
    assertChoice(parallel.mode,
      choices = c("local", "multicore", "socket", "mpi", "BatchJobs"))
    parallel.cpus = control_parameter(control, "ela_level.parallel.cpus",
      parallel::detectCores())
    parallel.cpus = ifelse(parallel.mode == "local", NA, parallel.cpus)
    assertInt(parallel.cpus, lower = 1L, na.ok = TRUE)
    parallel.logging =
      control_parameter(control, "ela_level.parallel.logging", FALSE)
    assertLogical(parallel.logging, len = 1L)
    parallel.level =
      control_parameter(control, "ela_level.parallel.level", "mlr.resample")
    lvls = as.character(unlist(parallelMap::parallelGetRegisteredLevels()))
    assertChoice(parallel.level, choices = lvls)
    parallel.info =
      control_parameter(control, "ela_level.parallel.show_info", FALSE)
    assertLogical(parallel.info, len = 1L)
    parallelMap::parallelStart(
      mode = parallel.mode, cpus = parallel.cpus,
      logging = parallel.logging,
      level = parallel.level,
      show.info = parallel.info)
  }
  measureTime(expression({
    probs = control_parameter(control, "ela_level.quantiles", 
      c(0.10, 0.25, 0.5))
    methods = control_parameter(control, "ela_level.classif_methods", 
      c("lda", "qda", "mda"))
    show.info = control_parameter(control, "ela_level.resample_info", FALSE)
    res.iters = control_parameter(control, "ela_level.resample_iterations", 10L)
    res.meth = control_parameter(control, "ela_level.resample_method", "CV")
    colnames(X) = paste0("x", 1:ncol(X))
    desc = mlr::makeResampleDesc(res.meth, iters = res.iters)
    inst = mlr::makeResampleInstance(desc, size = nrow(X))
    result = vapply(probs, function(prob) {
      y_quant = quantile(y, prob)
      data = data.frame(class = as.factor(y < y_quant), X)
      if (min(table(data$class)) < res.iters) {
        warningf("There are too few observations in case of 'quantile = %.3f'. In order to have at least one element of each class per block, quantile should be at least %.3f.",
          prob, (length(y[y <= y_quant]) + 1L) / feat.object$n.obs)
      }
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
    names(result) = sprintf("ela_level.%s_%02i", rep(meth.names, length(probs)), 
      rep(100 * probs, each = length(meth.names)))
    return(result)
  }), "ela_level")
}
