calculateFitnessDistanceFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    if (!feat.object$minimize)
      y = -1 * y
    opt.x = control_parameter(control, "fdr.opt_x", NULL)
    opt.y = control_parameter(control, "fdr.opt_y", NULL)
    assertNumeric(opt.x, null.ok = TRUE)
    assertNumber(opt.y, null.ok = TRUE)
    if (is.null(opt.y)) {
      opt.y = min(y)
    }
    tie.breaker = control_parameter(control, "fdr.tiebreaker", "sample")
    if (is.null(opt.x)) {
      id.opt = selectMin(y, tie.breaker = tie.breaker)
      opt.x = X[id.opt,]
    }
    prop.best = control_parameter(control, "fdr.prop_best", 1)
    assertNumber(prop.best, lower = .Machine$double.eps, upper = 1, null.ok = FALSE)

    if (prop.best < 1) {
      n.best = ceiling(feat.object$n.obs * prop.best)
      idx = which(y <= y[order(y)[n.best]])
      if (sum(idx) < 2L) {
        stop(sprintf("Selecting only %.1f%% of the sample results in less than 2 remaining observations.", 100 * prop.best))
      }
      y = y[idx]
      X = X[idx,]
    }
    id.opt = selectMin(y, tie.breaker = tie.breaker)

    d.meth = control_parameter(control, "fdr.dist_method", "minkowski")
    d.p = control_parameter(control, "fdr.dist_p", 2L)
    dists = dist(X, method = d.meth, p = d.p)
    dists = as.vector(dists)

    N = length(y)
    j = seq_len(id.opt - 1L)
    d.rel = c(dists[cumsum(N - j) - (N - id.opt)], 0)
    if (id.opt < N) {
      d.rel = c(d.rel, dists[(sum(N - j) + 1L) : sum(N - seq_len(id.opt))])
    }

    fd.cov = cov(y, d.rel)
    fd.cor = cfd / (sd(y) * sd(d.rel))

    list(fitness_distance.fd.cor = fd.cor,
      fitness_distance.fd.cov = fd.cov,
      fitness_distance.distance.mean = mean(d.rel),
      fitness_distance.distance.sd = sd(d.rel),
      fitness_distance.fitness.mean = mean(y),
      fitness_distance.fitness.sd = sd(y)
    )
  }), "fitness_distance")
}
