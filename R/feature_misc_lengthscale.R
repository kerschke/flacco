calculateLengthScaleFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  if (is.null(feat.object$fun))
    stop("The length scale features require the exact function!")
  allow.costs = control_parameter(control, "allow_costs", TRUE)
  if (!allow.costs)
    stop("You can not prohibit additional costs and still try to compute these features!")
  measureTime(expression({
    f = feat.object$fun
    d = feat.object$dim
    low = feat.object$lower
    upp = feat.object$upper
    steps = control_parameter(control, "lengthscale.steps", 1000L * d^2L)
    if (steps > 1e6) {
      warningf("Attention! Your sample will be of size %i and consequently you are computing features based on %i pairs of points.", steps, steps * (steps - 1) / 2)
    }
    assertNumber(steps, lower = 1L, finite = TRUE)
    x = vapply(seq_len(d), function(i) runif(1L, min = low[i], max = upp[i]), double(1L))
    x.path = x
    y = f(x)
    for (i in seq_len(steps - 1L)) {
      x = levyStep(x)
      # ensure that the step remains within bounds
      x = pmin(pmax(x, low), upp)
      x.path = rbind(x.path, x)
      y = c(y, f(x))
    }

    dx.meth = control_parameter(control, "lengthscale.dist_x.method", "minkowski")
    dx.p = control_parameter(control, "lengthscale.dist_x.p", 2L)
    dy.meth = control_parameter(control, "lengthscale.dist_y.method", "manhattan")
    dy.p = control_parameter(control, "lengthscale.dist_y.p", 2L)

    ## TODO: add Rcpp version for computing pairwise distances
    x.dist = dist(x.path, method = dx.meth, p = dx.p)
    y.dist = dist(y, method = dy.meth, p = dy.p)
    r = y.dist / x.dist
    r = r[!is.na(r)]

    use.kernel = control_parameter(control, "lengthscale.use_kernel", FALSE)
    if (use.kernel) {
      kernel.type = control_parameter(control, "lengthscale.kernel_type", "gaussian")
      smoothing.bw = control_parameter(control, "lengthscale.smoothing_bandwidth", "SJ")
      kernel = density(r, kernel = kernel.type, bw = smoothing.bw)
      kx = kernel$x
      ky = kernel$y
      KDE.sample_size = control_parameter(control, "lengthscale.KDE_sample_size", 500L)
      entropy.sample = runif(KDE.sample_size, min = min(r), max = max(r))
      # interpolate KDE probability
      probs = vapply(entropy.sample, function(s) {
        idx = max(which(kx <= s))
        if (idx == length(kx)) {
          return(ky[idx])
        }
        x1 = kx[idx]
        x2 = kx[idx + 1L]
        y1 = ky[idx]
        y2 = ky[idx + 1L]
        split = (s - x1) / (x2 - x1)
        (1 - split) * y1 + split * y2
      }, double(1L))
      h.r = calculateEntropy(probs)
    } else {
      h.r = calculateEntropy(r)
    }

    k.moment = control_parameter(control, "lengthscale.moments", 2:4)
    r.centered = mean(r)
    moments = vapply(k.moment, function(k) mean(r.centered^k), double(1L))
    moments = as.list(moments)
    names(moments) = sprintf("length_scale.distribution_moment_%i", k.moment)

    c(
      length_scale.shanon_entropy = h.r,
      length_scale.mean = mean(r),
      length_scale.sd = sd(r),
      moments,
      length_scale.costs_fun_evals = steps
    )
  }), "length_scale")
}

# single step of levy random walk
levyStep = function(x, loc = 0, scale = 1e-3) {
  vec = rnorm(n = length(x), mean = 0, sd = 1)
  norm.vec = normalizeVector(vec)
  step.size = randomLevy(1L, m = loc, s = scale)
  return(x + step.size * norm.vec )
}

# copied from rmutil::rlevy (to avoid unnecessary dependencies)
randomLevy = function(n = 1L, m = 0, s = 1) {
  return(s / qnorm(1 - runif(n) / 2)^2 + m)
}

calculateEntropy = function(x, base = 2L) {
  x = x / sum(x)
  return(-sum(x * log(x)) / log(base))
}
