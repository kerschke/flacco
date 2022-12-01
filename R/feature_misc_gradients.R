calculateGradientFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  if (is.null(feat.object$fun))
    stop("The gradient features require the exact function!")
  if (missing(control))
    control = list()
  assertList(control)
  allow.costs = control_parameter(control, "allow_costs", TRUE)
  if (!allow.costs)
    stop("You can not prohibit additional costs and still try to compute these features!")
  measureTime(expression({
    f = initializeCounter(feat.object$fun)
    d = feat.object$dim
    low = feat.object$lower
    upp = feat.object$upper
  
    n.steps = control_parameter(control, "gradient.n_steps", 100L * d)
    step.size = control_parameter(control, "gradient.step_size", (upp - low) / 20)

    # start by sampling a random point in one of the edges
    dd = sample(x = c(FALSE, TRUE), size = d, replace = TRUE)
    x = ifelse(dd, low, upp)
    y = f(x)
    signs = ifelse(dd, -1, 1)

    g.t = NULL
    for (i in seq_len(n.steps)) {
      x.t = x
      y.t = y
      # sample random direction
      rd = sample(x = seq_len(d), size = 1L)
      # avoid moving out of bounds
      tmp = x[rd] + signs[rd] * step.size[rd]
      if (!((tmp <= upp[rd]) & (tmp >= low[rd]))) {
        signs[rd] = -1L * signs[rd]
      }
      x[rd] = x[rd] + signs[rd] * step.size[rd]
      y = f(x)
      g.t = c(g.t, (y - y.t) / step.size[rd])
    }
    
    G.avg = mean(abs(g.t))
    G.dev = sqrt(sum((G.avg - abs(g.t))^2) / (n.steps - 1))
    
    list(gradient.G_avg = G.avg,
      gradient.G_dev = G.dev,
      gradient.costs_fun_evals = showEvals(f)
    )
  }), "ela_local")
}
