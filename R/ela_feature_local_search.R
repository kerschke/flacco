#' @title Calculate Local Search Features
#' @description
#' Computes features based on a local search approach. 
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.\cr
#' Note, that the feature object has to contain the function itself in
#' order to compute the local search features.
#' @param control [\code{\link{list}}]\cr
#' A list object that stores additional configuration parameters:\cr
#' The parameter \code{local.local_searches} defines the number of local
#' searches. The default is set to \code{50 * d} with \code{d} being the
#' dimension (i.e., the number of features).\cr
#' The argument \code{local.optim_method} defines the local search algorithm
#' (the default is \code{"BFGS"}). The corresponding settings of the local
#' search algorithm can be defined using \code{local.optim_method_control}.
#' This has to be a list object.\cr
#' In order to allow the reproducability of the results, one can also define a
#' seed (\code{local.sample_seed}), which will be set before the selection of
#' the initial start points for the local search.\cr
#' Once the local searches converge, basins have to be assigned. This is done
#' using hierarchical clustering from \code{\link{hclust}} (per default
#' \code{local.clust_method} is set to \code{"single"}, i.e.,
#' \emph{single linkage clustering}).\cr
#' The distances between merging two clusters are used to define whether
#' clusters belong together. Per default, \code{local.clust_cut_function}
#' calculates the \code{10\%}-quantile of all the distances in order to define
#' the clusters.
#' @param ... [any]\cr
#' Further arguments handled by \code{optim}.
#' @return [\code{\link{list}(15)} of \code{\link{numeric}(1)}].\cr
#' List of features.\cr
#' For further information, see details.
#' @details 
#' Given an initial design, a pre-defined number of local searches 
#' (\code{local.local_searches}) are executed, starting at randomly chosen 
#' points from the initial design.\cr
#' Those \sQuote{optima} are then clustered (using a hierarchical clustering
#' approach), considering close local optima belonging to the same basin.
#' Then, features that are based on the basin sizes as well as on the
#' (aggregated) objective values of the basins will be returned.\cr
#' 
#' The first two features (\code{ls.n_loc_opt}) return the absolute and
#' relative number of local optima.\cr
#' The next two features (\code{ls.best2mean_contr}) compare the ratio of the 
#' best local optimum to the mean of all found local optima.\cr
#' The following three features (\code{ls.basin_sizes.avg}) aggregate the
#' basin sizes of the the best, non-best (all except for the best) and worst
#' basins (w.r.t. the objective value).\cr
#' The next six features (\code{ls.f_evals}) aggregate the number of
#' function evaluations, which were needed for the local searches (aggregated
#' using minimum, 1st quartile, mean, median, 3rd quartile and maximum).\cr
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
#' 
#' # (2a) compute the convexity features (simple)
#' calculateLocalSearchFeatures(feat.object)
#' # (2b) compute the convexity features (with some control settings)
#' cluster_function = function(cl) as.numeric(quantile(cl$height, 0.25))
#' calculateLocalSearchFeatures(feat.object, control = list(
#'   local.optim_method = "L-BFGS-B", 
#'   local.clust_cut_function = cluster_function, 
#'   lower = -10, upper = 10
#' ))
#' @export 
calculateLocalSearchFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  f = initializeCounter(feat.object$fun)
  if (is.null(f))
    stop("The local search features require the exact function!")
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    if (!feat.object$minimize)
      y = -1 * y
    d = feat.object$dim
    if (missing(control))
      control = list()
    assertList(control)
    N = control_parameter(control, "local.local_searches", 50L * d)
    opt.algo = control_parameter(control, "local.optim_method", "BFGS")
    opt.algo.control = control_parameter(control, "local.optim_method_control", 
      list())
    opt.algo.control$fnscale = -1
    id.seed = control_parameter(control, "local.sample_seed", sample(1:1e6, 1))
    clust.method = control_parameter(control, "local.clust_method", "single")
    clust.cutfun = control_parameter(control, "local.clust_cut_function", 
      function(cl) as.numeric(quantile(cl$height, 0.1)))
    
    calcOptim = function(par) {
      res = optim(par, fn, method = opt.algo, control = opt.algo.control, ...)
      return(list(par = res$par, counts = resetCounter(fn)))
    }
    
    if (nrow(X) >= N) {
      set.seed(id.seed)
      ids = sample(nrow(X), N) 
    } else
      stop("Error in local_search_feature: More startpoints than Design Points.")
    
    fn = initializeCounter(f)
    result = lapply(ids, function(i) calcOptim(drop(X[i,])))
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
    
    list(ls.n_loc_opt.abs = max(clust),
      ls.n_loc_opt.rel = max(clust) / N,
      ls.best2mean_contr.orig = min(centers.funvals) / mean(centers.funvals),
      ls.best2mean_contr.ratio = (mean(centers.funvals) - min(centers.funvals)) / 
        (max(centers.funvals) - min(centers.funvals)),
      ls.basin_sizes.avg_best = mean(clust.size[centers.best]),
      ls.basin_sizes.avg_non_best = ifelse(length(clust.size[-centers.best]) == 0L,
        0, mean(clust.size[-centers.best])),
      ls.basin_sizes.avg_worst = mean(clust.size[centers.worst]),
      ls.fun_evals.min = min(fun.evals),
      ls.fun_evals.lq = as.numeric(quantile(fun.evals, 0.25)),
      ls.fun_evals.mean = mean(fun.evals),
      ls.fun_evals.med = median(fun.evals),
      ls.fun_evals.uq = as.numeric(quantile(fun.evals, 0.75)),
      ls.fun_evals.max = max(fun.evals),
      ls.costs_fun_evals = showEvals(f)
    )
  }), "ls")
}
