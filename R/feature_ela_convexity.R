calculateConvexityFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  if (is.null(feat.object$fun))
    stop("The convexity features require the exact function!")
  measureTime(expression({
    f = initializeCounter(feat.object$fun)
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    if (missing(control))
      control = list()
    calcDistance = function(n) {
      i = sample(n, 2L)
      wt = runif(1)
      wt = c(wt, 1 - wt)
      ## Linear compbination of X[i[1],] and X[i[2],]
      xn = drop(wt %*% X[i, ])
      ## Distance between xn and linear combination of y[i[1]] and y[i[2]]
      drop(f(xn) - y[i] %*% wt)
    }
    n = feat.object$n.obs
    N = control_parameter(control, "ela_conv.nsample", 1000L)
    eps = control_parameter(control, "ela_conv.threshold", 1e-10)
    delta = replicate(N, calcDistance(n))
    list(ela_conv.conv_prob = mean(delta < -eps),
      ela_conv.lin_prob = mean(abs(delta) <= eps),
      ela_conv.lin_dev.orig = mean(delta),
      ela_conv.lin_dev.abs = mean(abs(delta)),
      ela_conv.costs_fun_evals = showEvals(f))
  }), "ela_conv")
}
