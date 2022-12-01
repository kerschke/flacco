calculateHillClimbingFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  if (is.null(feat.object$fun))
    stop("The hill climbing features require the exact function!")
  if (missing(control))
    control = list()
  allow.costs = control_parameter(control, "allow_costs", TRUE)
  if (!allow.costs)
    stop("You can not prohibit additional costs and still try to compute these features!")
  assertList(control)
  measureTime(expression({
    f = initializeCounter(feat.object$fun)
    d = feat.object$dim
    n.obs = feat.object$n.obs
    N = control_parameter(control, "hill_climbing.local_searches", 100L)
    budget_per_run = control_parameter(control, "hill_climbing.max_iterations_per_run", 1000L * d)
    opt.algo = control_parameter(control, "hill_climbing.optim_method", "L-BFGS-B")
    d.meth = control_parameter(control, "hill_climbing.dist_method", "minkowski")
    d.p = control_parameter(control, "hill_climbing.dist_p", 2L)
    ## TODO: find a way to force L-BFGS-B to stop after a fixed number of fun evals (not iterations)
    opt.algo.control = control_parameter(control, "hill_climbing.optim_method_control", list(maxit = budget_per_run, ...))
    low = control_parameter(control, "hill_climbing.optim.lower", if (opt.algo == "L-BFGS-B") feat.object$lower else -Inf)
    upp = control_parameter(control, "hill_climbing.optim.upper", if (opt.algo == "L-BFGS-B") feat.object$upper else Inf)
    opt.algo.control$fnscale = ifelse(feat.object$minimize, 1L, -1L)
    id.seed = control_parameter(control, "hill_climbing.sample_seed", sample(seq_len(1e6), 1L))

    ## helper function for running the local searches and returning the relevant information
    calcOptim = function(par, ...) {
      res = optim(as.numeric(par), fn, method = opt.algo, control = opt.algo.control, lower = low, upper = upp, ...)
      return(list(par = res$par, fun.val = res$value, counts = resetCounter(fn)))
    }

    ## sample local search starting positions
    set.seed(id.seed)
    ls.starts = vapply(seq_len(d), function(i) runif(N, min = low[i], max = upp[i]), double(N))

    fn = initializeCounter(f)
    result = lapply(seq_len(N), function(i) calcOptim(ls.starts[i,], ...))
    pars = t(vapply(result, function(res) res$par, double(d)))
    fun.values = vapply(result, function(res) res$fun.val, double(1L))
    fun.evals = vapply(result, function(res) res$counts, integer(1L))

    dists = dist(pars, method = d.meth, p = d.p)
    dists = as.vector(dists)

    ## which (local) optima are globally optimal?
    if (feat.object$minimize) {
      global.opt = which(fun.values == min(fun.values))
    } else {
      global.opt = which(fun.values == max(fun.values))
    }

    ## consider distances to all global optima
    # d.rel = vapply(global.opt, function(g.opt) {
    #   j = seq_len(g.opt - 1L)
    #   d.rel = c(dists[cumsum(N - j) - (N - g.opt)], 0)
    #   if (g.opt < N) {
    #     d.rel = c(d.rel, dists[(sum(N - j) + 1L) : sum(N - seq_len(g.opt))])
    #   }
    #   return(d.rel)
    # }, double(N))

    ## only consider distances to closest global optimum
    local.opt = setdiff(seq_len(n.obs), global.opt)
    d.rel = vapply(local.opt, function(l.opt) {
      j = seq_len(l.opt - 1L)
      d.rel = c(dists[cumsum(n.obs - j) - (n.obs - l.opt)], 0)
      if (l.opt < n.obs) {
        d.rel = c(d.rel, dists[(sum(n.obs - j) + 1L) : sum(n.obs - seq_len(l.opt))])
      }
      return(min(d.rel[global.opt]))
    }, double(1L))

    list(
      hill_climbing.dist_between_opt.mean = mean(dists),
      hill_climbing.dist_between_opt.sd = sd(dists),
      hill_climbing.dist_local_to_global.mean = mean(d.rel),
      hill_climbing.dist_local_to_global.sd = sd(d.rel),
      hill_climbing.costs_fun_evals = sum(fun.evals)
    )
  }), "hill_climbing")
}
