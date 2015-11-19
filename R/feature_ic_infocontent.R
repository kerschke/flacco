calculateInformationContentFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    res = computeInfoContentStatistics(feat.object, control)
    # h.max = "maximum information content" - cf. equation (5)
    # eps.s = "settling sensitivity" - cf. equation (6)
    # eps.max (created due to a comment from Mario Munoz)
    # eps.ratio = "half partial information sensitivity" - cf. equation (8)
    # M0 = "initial partial information" - cf. equation (7)
    return(list(ic.h.max = res$Hmax,
      ic.eps.s = res$eps.S,
      ic.eps.max = log10(max(res$eps)),
      ic.eps.ratio = res$eps05,
      ic.m0 = res$M0
    ))
  }), "ic")
}

computeInfoContentStatistics = function(feat.object, control) {
  # epsilon values, as described in section V.A
  epsilon = control_parameter(control, "ic.epsilon", 
    c(0, 10^(seq(-5, 15, length.out = 1000)))
  )
  assertNumeric(epsilon, lower = 0)
  if (all(epsilon != 0)) {
    stop("One component of ic.epsilon has to be 0. Please add this component.")
  }
  epsilon = unique(epsilon)

  ## sorting strategy, either "nn" (= default) or "random"
  ## (motivation of default value: see discussion chapter)
  sorting = control_parameter(control, "ic.sorting", "nn")
  assertChoice(sorting, c("nn", "random"))

  generate.sample = control_parameter(control, "ic.sample.generate", FALSE)
  assertLogical(generate.sample, len = 1L)

  if (generate.sample) {
    # control parameters, which are only required, if the sample needs to be generated
    assertFunction(feat.object$fun)

    # size of sample (according to upper bound, as discussed on p. 78)
    sample.size = control_parameter(control, "ic.sample.size", 100L * feat.object$dim)
    assertInt(sample.size)

    # input dimensions (default: no. of cols in feat.object)
    sample.dimensions = control_parameter(control, "ic.sample.dimensions", feat.object$dim)
    assertInt(sample.dimensions)

    # minimum and maximum possible value of generated samples
    # (Needs to be identical for every dimension! Passing vectors leads to unexpected results.
    # If individual ranges per dimension are required, please generate the sample yourself.)
    sample.lower = control_parameter(control, "ic.sample.lower", feat.object$lower)
    sample.upper = control_parameter(control, "ic.sample.upper", feat.object$upper)
    assertNumeric(sample.lower)
    assertNumeric(sample.upper)

    X = initializeLHD(points = sample.size, dims = sample.dimensions, 
      lower = sample.lower, upper = sample.upper)
    y = apply(X, 1, feat.object$fun)
    n = nrow(X)
  } else {
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
    n = feat.object$n.obs
  }
  
  ## aggregate observations which have duplicates in the decision space
  dup.index = duplicated(X)
  if (any(dup.index)) {
    if (all(dup.index[-1L]))
      stop("Can not compute information content features, because ALL values are identical.")
    completely.duplicated = duplicated(cbind(X, y))
    if (any(completely.duplicated)) {
      X = X[!completely.duplicated,]
      y = y[!completely.duplicated]
      dup.index = duplicated(X)
    }
    aggr = control_parameter(control, "ic.aggregate_duplicated", mean)
    dup.index = dup.index | rev(duplicated(X[nrow(X):1L,]))
    Z = X[dup.index,]
    X = X[!dup.index,]
    v = y[dup.index]
    y = y[!dup.index]
    if (control_parameter(control, "ic.show_warnings", FALSE)) {
      warningf("%i duplicated observations were aggregated when computing the information content features.",
        sum(dup.index))
    }
    while (length(v) > 1) {
      index = vapply(1:nrow(Z), function(i) all(Z[i,] == Z[1,]), logical(1L))
      X = rbind(X, Z[1,])
      Z = Z[!index,]
      y = c(y, aggr(v[index]))
      v = v[!index]
    }
    rownames(X) = 1:nrow(X)
    n = nrow(X)
  }

  # sort values (nearest neighbours vs. random) and compute distances
  seed = control_parameter(control, "ic.seed", sample.int(.Machine$integer.max, 1))
  set.seed(seed)
  if (sorting == "random") {
    permutation = sample.int(n)
    d = computeDistances(X = X, permutation = permutation)
  } else if (sorting == "nn") {
    start = control_parameter(control, "ic.nn.start", sample.int(n, 1))
    hood = control_parameter(control, "ic.nn.neighborhood", 20L)
    assertInt(start, lower = 1L, upper = n)
    assertInt(hood, lower = 1L, upper = n)
    res = constructSequence(X = X, start = start, hood = hood)
    permutation = res$permutation
    d = res$distance
  }

  psi.eps = vapply(epsilon, function(eps) {
    computePsi(permutation = permutation, xdists = d, y = y, eps = eps)
  }, integer(length(permutation) - 1L))

  H = apply(psi.eps, 2, computeH)
  M = apply(psi.eps, 2, computeM)

  # calculate eps.S, cf. equation (6) ("settling sensitivity")
  settl.sens = control_parameter(control, "ic.settling_sensitivity", 0.05)
  assertNumeric(settl.sens, lower = 0, upper = .Machine$double.xmax)
  eps.S = epsilon[which(H < settl.sens)]
  eps.S = ifelse(length(eps.S) > 0, log10(min(eps.S)), NA_real_)

  # calculate M0, cf. equation (7) ("initial partial information")
  M0 = M[epsilon == 0]

  # calculate epsilon05 [Eq. (8)] ("half partial information sensitivity")
  inf.sens = control_parameter(control, "ic.info_sensitivity", 0.5)
  assertNumeric(inf.sens, lower = -1, upper = 1)

  eps05 = which(M > inf.sens * M0)
  eps05 = ifelse(length(eps05) > 0, log10(max(epsilon[eps05])), NA_real_)

  return(list(H = H, M = M, eps = epsilon, eps05 = eps05, eps.S = eps.S,
    M0 = M0, Hmax = max(H)))
}

## initialization of Latin-Hypercube-Sample:
## generates a LHS on [0,1]^dims and transforms it according
## to the constraints
initializeLHD = function(points, dims, lower, upper) {
  X = lhs::improvedLHS(n = points, k = dims)
  n = nrow(X)
  vapply(1:dims, function(i) {
    X[,i] * (upper - lower)[i] + lower[i]
  }, double(n))
}

## construct a path through the landscape - starting with an initial
## observation and walking (greedily) from an observation to its nearest
## (not-yet-visited) neighbour;
## the output is a matrix with two columns: the first returns the index
## of the elements (in which they've been visited) and the second one
## returns the distance from neighbour to neighbour
constructSequence = function(X, start, hood) {
  nn.list = RANN::nn2(X, k = min(hood, nrow(X)))
  n = nrow(X)
  # add first candidate (random) and initialise permutation vector (avoids
  # continuous allocation of space)
  if (missing(start))
    current = sample.int(n, 1L)
  else
    current = as.integer(start)
  candidates = seq_len(n)[-current]
  permutation = c(current, rep(NA_integer_, n - 1L))
  dists = rep(NA_real_, n)

  # successively add next candidates
  for (i in seq_len(n)[-1L]) {
    currents = nn.list$nn.idx[permutation[i - 1L], ]
    current = intersect(currents, candidates)
    if (length(current) > 0L) {
      current = current[1L]
      permutation[i] = current
      candidates = candidates[-which(candidates == current)]
      dists[i] = nn.list$nn.dists[permutation[i - 1], currents == current]
    } else {
      # list of nearest (yet unvisited) neighbor
      nn.list2 = RANN::nn2(X[candidates, , drop = FALSE],
        query = X[permutation[i - 1L], , drop = FALSE],
        k = min(nrow(X), 1L))
      current = as.integer(candidates[nn.list2$nn.idx])
      permutation[i] = current
      candidates = candidates[-which(candidates == current)]
      dists[i] = as.numeric(nn.list2$nn.dists)
    }
  }
  return(list(permutation = permutation, distance = dists[-1L]))
}

## compute distance (needed, if sequence is random):
computeDistances = function(X, permutation) {
  X = X[permutation, ]
  n = nrow(X) - 1L
  vapply(seq_len(n), function(i) sqrt(sum((X[i,] - X[i + 1,])^2)), double(1L))
  # dist(X)[cumsum(n : 2) - (n - 1)]
}

## cf. equation (9)
computePsi = function(permutation, xdists, y, eps) {
  y = y[permutation]
  ratio = diff(y) / xdists
  ifelse(abs(ratio) < eps, 0L, as.integer(sign(ratio)))
}

## cf. equation(2)
computeH = function(psi) {
  a = psi[-length(psi)]
  b = psi[-1]
  probs = c(
    #neg_neg = mean((a == -1) & (b == -1)), 
    neg_neu = mean((a == -1) & (b == 0)), 
    neg_pos = mean((a == -1) & (b == 1)),
    neu_neg = mean((a == 0) & (b == -1)),
    #neu_neu = mean((a == 0) & (b == 0)),
    neu_pos = mean((a == 0) & (b == 1)),
    pos_neg = mean((a == 1) & (b == -1)),
    pos_neu = mean((a == 1) & (b == 0))#,
    #pos_pos = mean((a == 1) & (b == 1))
  )
  -sum(ifelse(probs == 0, 0, probs * log(probs, base = 6)))
}

## cf. equation(3)
computeM = function(psi) {
  n = length(psi)
  psi = psi[psi != 0]
  psi = psi[c(FALSE, diff(psi) != 0)]
  length(psi) / (n - 1)
}
