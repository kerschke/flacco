calculateLocalSearchFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  f = initializeCounter(feat.object$fun)
  if (is.null(f))
    stop("The local search features require the exact function!")
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    d = feat.object$dim
    if (missing(control))
      control = list()
    assertList(control)
    N = control_parameter(control, "ela_local.local_searches", 50L * d)
    opt.algo = control_parameter(control, "ela_local.optim_method", "L-BFGS-B")
    opt.algo.control = control_parameter(control, "ela_local.optim_method_control", 
      list())
    low = control_parameter(control, "ela_local.optim.lower", ifelse(opt.algo == "L-BFGS-B", feat.object$lower, -Inf))
    upp = control_parameter(control, "ela_local.optim.upper", ifelse(opt.algo == "L-BFGS-B", feat.object$upper, Inf))
    if (!feat.object$minimize) {
      y = -1 * y
      opt.algo.control$fnscale = -1
    } else {
      opt.algo.control$fnscale = 1
    }
    id.seed = control_parameter(control, "ela_local.sample_seed", sample(1:1e6, 1))
    clust.method = control_parameter(control, "ela_local.clust_method", "single")
    clust.cutfun = control_parameter(control, "ela_local.clust_cut_function", 
      function(cl) as.numeric(quantile(cl$height, 0.1)))
    
    calcOptim = function(par, ...) {
      res = optim(as.numeric(par), fn, method = opt.algo, control = opt.algo.control, lower = low, upper = upp, ...)
      return(list(par = res$par, counts = resetCounter(fn)))
    }
    
    if (nrow(X) >= N) {
      set.seed(id.seed)
      ids = sample(nrow(X), N) 
    } else
      stop("Requesting more starting points than observations in the initial design.")
    
    fn = initializeCounter(f)
    result = lapply(ids, function(i) calcOptim(drop(X[i,]), ...))
    pars = t(vapply(result, function(i) i$par, double(d)))
    fun.evals = vapply(result, function(i) i$counts, integer(1))
    
    cl = hclust(dist(pars), clust.method)
    clust = cutree(cl, h = clust.cutfun(cl))
    
    clust.size = tapply(clust, clust, length)
    clust.size = clust.size / sum(clust.size) ## Normalize!
    
    centers = t(vapply(seq_along(clust.size),
      function(i) colMeans(pars[clust == i, , drop = FALSE]), double(d)))
    centers.funvals = apply(centers, 1, f)
    centers.best = which(centers.funvals == min(centers.funvals))
    centers.worst = which(centers.funvals == max(centers.funvals))
    
    list(ela_local.n_loc_opt.abs = max(clust),
      ela_local.n_loc_opt.rel = max(clust) / N,
      ela_local.best2mean_contr.orig = min(centers.funvals) / mean(centers.funvals),
      ela_local.best2mean_contr.ratio = (mean(centers.funvals) - min(centers.funvals)) / 
        (max(centers.funvals) - min(centers.funvals)),
      ela_local.basin_sizes.avg_best = mean(clust.size[centers.best]),
      ela_local.basin_sizes.avg_non_best = ifelse(length(clust.size[-centers.best]) == 0L,
        0, mean(clust.size[-centers.best])),
      ela_local.basin_sizes.avg_worst = mean(clust.size[centers.worst]),
      ela_local.fun_evals.min = min(fun.evals),
      ela_local.fun_evals.lq = as.numeric(quantile(fun.evals, 0.25)),
      ela_local.fun_evals.mean = mean(fun.evals),
      ela_local.fun_evals.median = median(fun.evals),
      ela_local.fun_evals.uq = as.numeric(quantile(fun.evals, 0.75)),
      ela_local.fun_evals.max = max(fun.evals),
      ela_local.fun_evals.sd = sd(fun.evals),
      ela_local.costs_fun_evals = showEvals(f)
    )
  }), "ela_local")
}
