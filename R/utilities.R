control_parameter = function(control, name, default) {
  v = control[[name]]
  if (is.null(v))
    return(default)
  else
    return(v)
}

## wraps a evaluation counter around the function
initializeCounter = function(fn) {
  force(fn)
  count = 0L
  structure(function(x, ...) {
    count <<- count + if (is.matrix(x)) 
      ncol(x)
    else 1L
    fn(x, ...)
  })
}

## shows the number of function evaluations / calls
showEvals = function(fn) {
  environment(fn)$count
}

## resets the evaluation counter to 0 and returns the number of calls
resetCounter = function (fn) {
  counts = environment(fn)$count
  environment(fn)$count = 0L
  counts
}

## helper, which returns the minimum of a vector
selectMin = function(x, tie.breaker = "sample") {
  i = which(x == min(x))
  if (length(i) > 1L) {
    if (tie.breaker == "first") {
      return(i[1L])
    } else if (tie.breaker == "last") {
      return(i[length(i)])
    } else if (tie.breaker == "sample") {
      return(sample(i, 1L))
    }
  }
  return(i)
}

## helper, which returns the maximum of a vector
selectMax = function(x, tie.breaker = "sample") {
  i = which(x == max(x))
  if (length(i) > 1L) {
    if (tie.breaker == "first") {
      return(i[1L])
    } else if (tie.breaker == "last") {
      return(i[length(i)])
    } else if (tie.breaker == "sample") {
      return(sample(i, 1L))
    }
  }
  return(i)
}

normalizeVector = function(x) {
  x / sqrt(crossprod(x, x))
}

extractFeatures = function(feat.object) {
  as.matrix(subset(feat.object$env$init, select = feat.object$feature.names))
}

extractObjective = function(feat.object) {
  feat.object$env$init[, feat.object$objective.name]
}

extractInit = function(feat.object) {
  feat.object$env$init
}

# Imitates matlab full(sparse(i,j,s)) function for the special case of square matrices.
# Creates an m by m (square) matrix A, where m = max(i, j) and a[i[k], j[k]] = s[k].
fullsparse = function (i, j, s) {
  if (length(i) != length(j) || length(j) != length(s) || length(i) != length(s))
    warning("Arguments must be of same length!")
  m = max(i, j)
  A = matrix(0, nrow = m, ncol = m)
  for (k in seq_along(i))
    A[i[k], j[k]] = s[k]
  return(A)
}


## modified version of numDeriv::hessian (which now allows to compute the
## Hessian without exceeding the boundaries)
flaccoHessian = function (func, x, method.args = list(), lower, upper) {
  args = list(eps = 1e-04, d = 0.1, zero.tol = sqrt(.Machine$double.eps / 7e-07), r = 4, v = 2, show.details = FALSE)
  args[names(method.args)] = method.args
  D = flaccoGenD(func, x, method.args = args, lower = lower, upper = upper)
  H = diag(NA, length(x))
  u = length(x)
  for (i in seq_along(x)) {
    ind = seq_len(i)
    H[i, ind] = D[, u + ind]
    u = u + length(ind)
  }
  H = H + t(H)
  diag(H) = diag(H)/2
  H
}

## helper function for flaccoHessian (also a modified version of numDeriv::genD)
flaccoGenD = function (func, x, method.args = list(), lower, upper) {
  args = list(eps = 1e-04, d = 1e-04, zero.tol = sqrt(.Machine$double.eps/7e-07), r = 4, v = 2)
  args[names(method.args)] = method.args
  d = args$d
  r = args$r
  v = args$v
  if (v != 2) 
    stop("The current code assumes v is 2 (the default).")
  f0 = func(x)
  n = length(x)
  h0 = abs(d * x) + args$eps * (abs(x) < args$zero.tol)
  D = matrix(0, length(f0), (n * (n + 3))/2)
  Daprox = matrix(0, length(f0), r)
  Hdiag = matrix(0, length(f0), n)
  Haprox = matrix(0, length(f0), r)
  for (i in seq_len(n)) {
    h = h0
    for (k in seq_len(r)) {
      x1 = x + (i == (1:n)) * h
      ind1 = (x1 >= upper)
      x2 = x - (i == (1:n)) * h
      ind2 = (x2 <= lower)
      if (any(ind1)) {
        ## exceeded upper boundaries by this value
        excess = x1[ind1] - upper[ind1]
        x1[ind1] = upper[ind1]
        x2[ind1] = x2[ind1] - excess
      }
      if (any(ind2)) {
        ## exceeded lower boundaries by this value
        excess = lower[ind2] - x2[ind2]
        x1[ind2] = x1[ind2] + excess
        x2[ind2] = lower[ind2]
      }
      f1 = func(x1)
      f2 = func(x2)
      Daprox[, k] = (f1 - f2)/(2 * h[i])
      Haprox[, k] = (f1 - 2 * f0 + f2)/h[i]^2
      h = h/v
    }
    for (m in 1:(r - 1)) for (k in 1:(r - m)) {
      Daprox[, k] = (Daprox[, k + 1] * (4^m) - Daprox[, k])/(4^m - 1)
      Haprox[, k] = (Haprox[, k + 1] * (4^m) - Haprox[, k])/(4^m - 1)
    }
    D[, i] = Daprox[, 1]
    Hdiag[, i] = Haprox[, 1]
  }
  u = n
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      u = u + 1
      if (i == j) {
        D[, u] = Hdiag[, i]
      } else {
        h = h0
        for (k in 1:r) {
          x1 = x + (i == (1:n)) * h + (j == (1:n)) * h
          ind1 = (x1 >= upper)
          x2 = x - (i == (1:n)) * h - (j == (1:n)) * h
          ind2 = (x2 <= lower)
          if (any(ind1)) {
            ## exceeded upper boundaries by this value
            excess = x1[ind1] - upper[ind1]
            x1[ind1] = upper[ind1]
            x2[ind1] = x2[ind1] - excess
          }
          if (any(ind2)) {
            ## exceeded lower boundaries by this value
            excess = lower[ind2] - x2[ind2]
            x1[ind2] = x1[ind2] + excess
            x2[ind2] = lower[ind2]
          }
          f1 = func(x1)
          f2 = func(x2)
          Daprox[, k] = (f1 - 2 * f0 + f2 - Hdiag[, i] * h[i]^2 - Hdiag[, j] * h[j]^2)/(2 * h[i] * h[j])
          h = h/v
        }
        for (m in 1:(r - 1)) {
          for (k in 1:(r - m)) {
            Daprox[, k] = (Daprox[, k + 1] * (4^m) - Daprox[, k])/(4^m - 1)
          }
        }
        D[, u] = Daprox[, 1]
      }
    }
  }
  return(D)
}
