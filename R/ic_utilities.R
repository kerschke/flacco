## initialization of Latin-Hypercube-Design:
## generates a LHD on [0,1]^dims and transforms it according
## to the constraints
initializeLHD = function(points, dims, lower, upper) {
  X = lhs::randomLHS(points, dims)
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
constructSequence = function(X, start) {
  distances = as.matrix(dist(X))
  n = nrow(X)
  # add first candidate (random) and initialise permutation vector (avoids continuous allocation of space)
  if (missing(start))
    current = sample.int(n, 1L)
  else
    current = as.integer(start)
  candidates = seq_len(n)[-current]
  permutation = c(current, rep(NA_integer_, n - 1L))
  dists = rep(NA_real_, n)
  
  # successively add next candidates
  for (i in 2:(n - 1L)) {
    # invalidate those that are already in permutation
    d = distances[permutation[i - 1], ]
    
    # then select the neighbour with minimal distance (sample if necessary)
    permutation[i] = candidates[selectMin(d[candidates])]
    dists[i] = min(d[candidates])
    # remove from future candidates
    candidates = candidates[-which(candidates == permutation[i])]
  }
  # add last candidate
  permutation[n] = as.integer(candidates)
  dists[n] = distances[permutation[n - 1], candidates]
  return(cbind(permutation = permutation, distance = dists))
}

## compute distance (needed, if sequence is random):
computeDistances = function(X, permutation) {
  X = X[permutation, ]
  n = nrow(X)
  dist(X)[cumsum(n : 2) - (n - 1)]
}

## cf. equation (9)
computePsi = function(permutation, xdists, y, eps) {
  y = y[permutation]
  ratio = diff(y) / xdists
  ifelse(ratio < -eps, -1L, ifelse(ratio > eps, 1L, 0L))
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
