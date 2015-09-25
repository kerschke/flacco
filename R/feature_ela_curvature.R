calculateCurvatureFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  f = initializeCounter(feat.object$fun)
  if (is.null(f))
    stop("The curvature features require the exact function!")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    X = extractFeatures(feat.object)
    d = feat.object$dim
    N = control_parameter(control, "ela_curv.sample_size", 100L * d)
    calcNumDeriv = function(par) {
      gr = numDeriv::grad(f, par)
      hess = numDeriv::hessian(f, par)
      eig = abs(eigen(hess)$values)
      c(curv.grad_norm = sqrt(sum(gr^2)),
        curv.grad_scale = max(abs(gr)) / min(abs(gr)),
        curv.hessian_cond = max(eig) / min(eig))
    }
    ids = sample(feat.object$n.obs, N, replace = FALSE)
    res = apply(X[ids, ], 1, calcNumDeriv)
    fn = apply(res, 1, function(x) {
      z = fivenum(x)
      return(c(z[1:2], mean(x), z[3:5], sd(x)))
    })
    fn = as.vector(fn, mode = "list")
    nn = c("min", "lq", "mean", "med", "uq", "max", "sd")
    names(fn) = paste(rep(rownames(res), each = length(nn)), nn, sep = ".")
    names(fn) = paste0("ela_", names(fn))
    fn
    return(c(fn, ela_curv.costs_fun_evals = showEvals(f)))
  }), "ela_curv")
}
