calculateSobolIndicesFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    # f = feat.object$fun
    d = feat.object$dim
    low = feat.object$lower
    upp = feat.object$upper
    n = feat.object$n.obs

    ## A. Metrics based on Sobol Indices
    sens = sobolAnalyze(d, y)
    v.inter = 1 - sum(sens[["S1"]])
    v.cv = sd(sens[["ST"]]) * sqrt((d - 1) / d) / mean(sens[["ST"]])

    ## B. Fitness- and State-Variance
    # 1. Fitness Variance
    fitness.variance = var(y / mean(y))

    # 2. State Variance
    n.bins = control_parameter(control, "sobol.state_var.n_bins", 20L)
    y.bin = as.integer(cut(y, breaks = seq(min(y) - .Machine$double.eps, max(y), length.out = n.bins + 1L)))

    min.obs.per.bin.factor = control_parameter(control, "sobol.state_var.obs_per_bin_factor", 1.5)
    obs.per.bin = vapply(seq_len(n.bins), function(i) sum(y.bin == i), integer(1L))
    index = (obs.per.bin >= (min.obs.per.bin.factor * d))
    if (any(index)) {
      disp.per.bin = vapply(which(index), function(bin) {
        X.group = X[y.bin == bin, , drop = FALSE]
        x.mean = colMeans(X.group)
        db.j = colMeans(sqrt((t(X.group) - x.mean)^2))
        # # TODO: shouldn't it rather be the following one?
        # db.j = sqrt(colMeans((t(X.group) - x.mean)^2))
        mean(db.j)
      }, double(1L))
      state.variance = var(rep(disp.per.bin, obs.per.bin[index])) * (n - 1L) / n
    } else {
      state.variance = NA_real_
    }

    ## C. Fitness- and State Skewness
    # 1. Fitness Skewness
    # TODO: check why "abs" in pflacco
    norm.factor = (max(y) - min(y)) / 2
    y.hat = norm.factor + min(y)
    fitness.skewness = mean((y.hat - y) / norm.factor)

    # 2. State Skewness
    # TODO: check in pflacco
    state.skewness = 1 - (2 * n.bins / (n * (n.bins - 1))) * sum(disp.per.bin * obs.per.bin[index])

    list(
      fla_metric.sobol_ind.var_interaction_degree = v.inter,
      fla_metric.sobol_ind.var_coeff_sensitivity = v.cv,
      fla_metric.fitness.var = fitness.variance,
      fla_metric.state.var = state.variance,
      fla_metric.fitness.skewness = fitness.skewness,
      fla_metric.state.skewness = state.skewness
    )
  }), "fla_metric")
}


sobolAnalyze = function(d, y) {
  # conf.level = 0.95
  # num_resamples = 100L
  # normalize y
  y = (y - mean(y)) / sd(y)
  # TODO: check how to reasonably adjust if length(y) is not multiple of d + 2
  N = as.integer(length(y) / (d + 2))
  y.mat = matrix(y, ncol = d + 2L, byrow = TRUE)
  A = y.mat[,1L]
  B = y.mat[, ncol(y.mat)]
  AB = y.mat[, 2L:(ncol(y.mat) - 1L)]
  # r = matrix(sample(N, size = N * num_resamples, replace = TRUE), ncol = num_resamples)
  # Z = qnorm(0.5 + conf.level / 2)

  S.vec = c("S1", "S1_conf", "ST", "ST_conf")
  S = setNames(lapply(S.vec, function(sv) rep(0, d)), nm = S.vec)

  # A.tmp = t(vapply(seq_len(N), function(i) {A[r[i,]]}, double(num_resamples)))
  # B.tmp = t(vapply(seq_len(N), function(i) {B[r[i,]]}, double(num_resamples)))
  # AB.tmp = lapply(seq_len(d), function(j) t(vapply(seq_len(N), function(i) {AB[r[i,], j]}, double(num_resamples))))

  S1 = vapply(seq_len(d), function(j) calcFirstOrder(A, AB[, j], B), double(1L))
  # S1_conf = vapply(seq_len(d), function(j) {
  #   firstOrder = vapply(seq_len(num_resamples), function(k) calcFirstOrder(A.tmp[,k], AB.tmp[[j]][,k], B.tmp[,k]), double(1L))
  #   Z * sd(firstOrder)
  # }, double(1L))
  ST = vapply(seq_len(d), function(j) calcTotalOrder(A, AB[, j], B), double(1L))
  # ST_conf = vapply(seq_len(d), function(j) {
  #   totalOrder = vapply(seq_len(num_resamples), function(k) calcTotalOrder(A.tmp[,k], AB.tmp[[j]][,k], B.tmp[,k]), double(1L))
  #   Z * sd(totalOrder)
  # }, double(1L))
  
  # S = list("S1" = S1, "S1_conf" = S1_conf, "ST" = ST, "ST_conf" = ST_conf)
  S = list("S1" = S1, "ST" = ST)

  return(S)
}

calcFirstOrder = function(A, AB, B) {
  ABvec = c(A, B)
  n = length(ABvec)
  mean(B * (AB - A)) / (var(ABvec) * (n - 1) / n)
}

calcTotalOrder = function(A, AB, B) {
  ABvec = c(A, B)
  n = length(ABvec)
  0.5 * mean((A - AB)^2) / (var(ABvec) * (n - 1) / n)
}
