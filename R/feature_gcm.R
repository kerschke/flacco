calculateGCMFeatures = function (feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  assertList(control)

  approaches = control_parameter(control, "gcm.approaches", c("min", "mean", "near"))
  cf.power = control_parameter(control, "gcm.cf_power", 256L)
  assertInt(cf.power, lower = 1L, upper = Inf)
  gcm.control = list(cf.power = cf.power)

  result = lapply(approaches, function(approach) {
    measureTime(expression({
      yvals = getObjectivesByApproach(feat.object, approach)
      sparse.matrix = calculateSparseMatrix(feat.object, yvals)
      canonical.list = computeCanonical(sparse.matrix)
      fundamental.list = computeFundamental(
        canonical.list = canonical.list, gcm.control = gcm.control)
      feats = computeGCMFeats(fundamental.list, yvals)
      names(feats) = sprintf("gcm.%s.%s", approach, names(feats))
      return(feats)
    }), sprintf("gcm.%s", approach))
  })
  return(unlist(result, recursive = FALSE))
}


getObjectivesByApproach = function(feat.object, approach) {
  assertChoice(approach, c("min", "mean", "near"))
  obj = feat.object$objective.name
  if (!feat.object$minimize)
    init.grid[, obj] = -1 * init.grid[, obj]

  if (approach == "min") { 
    yvals = vapply(seq_len(feat.object$total.cells), function (i) {
      if (any(feat.object$init.grid$cell.ID == i)) {
        return(min(feat.object$init.grid[feat.object$init.grid$cell.ID == i, obj]))
      } else {
        return(Inf)
      }
    }, numeric(1))
  } else if (approach == "mean") {
    yvals = vapply(seq_len(feat.object$total.cells), function (i) {
      if (any(feat.object$init.grid$cell.ID == i)) {
        return(mean(feat.object$init.grid[feat.object$init.grid$cell.ID == i, obj]))
      } else {
        return(Inf)
      }
    }, numeric(1))
  } else {
    nearest = findNearestPrototype(feat.object)
    yvals = rep(Inf, length = feat.object$total.cells)
    yvals[nearest$represented.cell] = nearest[[obj]]
  }
  return(yvals)
}


calculateSparseMatrix = function(feat.object, yvals) {
  valid.cells = setdiff(seq_len(feat.object$total.cells), which(!is.finite(yvals)))
  transitions = lapply(seq_len(feat.object$total.cells), function(i) {
    if (is.infinite(yvals[i])) {
      return(NULL)
    }
    cell.coord = celltoz(i, feat.object$blocks)

    # Find all neighbours of the current cell
    nb.coord = expand.grid(lapply(cell.coord, function(x) x + c(-1, 0, 1)))

    # Discard those neighbours that are outside the bounds or the cell itself
    discard = apply(nb.coord, 1, isInvalidNeighbour, cell = cell.coord, blocks = feat.object$blocks)
    nb = as.integer(apply(nb.coord[!discard, ], 1, ztocell, blocks = feat.object$blocks))
    nb = nb[nb %in% valid.cells]
    cell = as.integer(rep(i, length(nb)))

    y1 = yvals[cell]
    y2 = yvals[nb]
    better = (y1 > y2)
    if (any(better)) {
      diffs = (y1 - y2)[better]
      return(list(cell = cell[better], nb = nb[better], prob = diffs / sum(diffs)))
    } else {
      cell = c(cell, i)
      nb = c(nb, i)
      equal = c(y1 == y2, TRUE)
      return(list(cell = cell[equal], nb = nb[equal],
        prob = rep(1 / sum(equal), sum(equal))))
    }
  })
  fullsparse(unlist(lapply(transitions, function(x) x$cell)),
    unlist(lapply(transitions, function(x) x$nb)),
    unlist(lapply(transitions, function(x) x$prob)))
}

# Computes the canonical form of a stochastic matrix "mat"
# - permutation contains the permutation of indices
computeCanonical = function(mat) {
  directed.graph = findDirectedGraph(mat)
  # two cells i and j belong to the same communicating class iff there is a
  # directed path from i to j and a directed path from j to i
  communicating.class = directed.graph & t(directed.graph)
  closed.class = vapply(seq_len(nrow(mat)), function(i)
    all(directed.graph[,i] == communicating.class[,i]), logical(1L))
  index.closed = which(closed.class)
  index.open = which(!closed.class)

  # Each closed class has a unique representative in representative.
  ind = lapply(index.closed, function(i) which(communicating.class[i,]))
  permutation = c(unique(unlist(ind)), index.open)
  return(list(canonical.form = mat[permutation, permutation],
    permutation.index = permutation, no.attractors = sum(closed.class)))
}


# check for each cell (row) i, whether there is a directed graph from that
# cell to another cell (column) j
findDirectedGraph = function(mat) {
  n = nrow(mat)
  classes = diag(n)
  # calculate which cells (i.e. rows) are connected to cell i
  classes = vapply(seq_len(n), function(i) {
    connected.cells = i
    current.row = classes[i,]
    old.val = 1
    new.val = 0
    while (old.val != new.val) {
      old.val = sum(current.row > 0)
      if (length(connected.cells) == 1) {
        sums = mat[connected.cells, ]
      } else {
        sums = colSums(mat[connected.cells, ])
      }
      d = which(sums > 0)
      current.row[d] = 1      
      new.val = sum(current.row > 0)
      connected.cells = d
    }
    as.logical(current.row)
  }, logical(n))
  return(classes)
}


computeFundamental = function(canonical.list, gcm.control) {

  canonical.form = canonical.list$canonical.form
  permutation.index = canonical.list$permutation.index
  no.attractors = canonical.list$no.attractors
  seq.closed.classes = seq_len(no.attractors)

  # approximate canonical.form to the power of infinity
  canonical.form = expm::"%^%"(canonical.form, gcm.control$cf.power) # [ orig:  Q = Cf^50; ]

  # write matrix of closed.classes columns of Q    
  fundamental.mat = canonical.form[, seq.closed.classes, drop = FALSE]

  # remove rows that contain only zeros (i.e. diffcells / empty cells)
  non.zero = vapply(seq_len(nrow(fundamental.mat)),
    function(i) any(fundamental.mat[i,] != 0), logical(1L))
  fundamental.mat = fundamental.mat[non.zero, , drop = FALSE]

  # remove columns that only contain zeros (and recalculate no. of closed classes)
  zero = vapply(seq_len(ncol(fundamental.mat)), function(i) all(fundamental.mat[, i] == 0), logical(1L))
  if (sum(zero) > 0) {
    rtmp = permutation.index[seq.closed.classes]
    rtmp = rtmp[!zero]
    no.attractors = length(rtmp)
    seq.closed.classes = seq_len(no.attractors)
    
    # actually remove columns now:
    permutation.index = permutation.index[-which(zero)] # [ orig: %Index ]
    fundamental.mat = fundamental.mat[ , !zero, drop = FALSE] #[ orig: Fm( :, !any(Fm,1) ) = [];  %columns ]
  }
  return(list(fundamental.mat = fundamental.mat,
    permutation.index = permutation.index, seq.closed.classes = seq.closed.classes))
}


# actual computation of the GCM features
computeGCMFeats = function(fundamental.list, yvals) {
  n = length(yvals)

  # store components of fundamental.list
  fundamental.mat = fundamental.list$fundamental.mat
  permutation.index = fundamental.list$permutation.index
  result = list(attractors = ncol(fundamental.mat))
  seq.closed.classes = fundamental.list$seq.closed.classes

  # compute and store periodic and transient cells
  pcells = permutation.index[seq.closed.classes]
  result$pcells = length(pcells) / n
  tcells = permutation.index[-seq.closed.classes]
  result$tcells = length(tcells) / n

  # compute uncertain boxes
  no.uncertain.cells = sum(rowSums(fundamental.mat != 0) > 1)
  result$uncertain = no.uncertain.cells / n
  
  # compute probability of each basin of attraction (origin: sBoA)
  basin.prob = colSums(fundamental.mat) / n
  result$basin_prob.min = min(basin.prob)
  result$basin_prob.mean = mean(basin.prob)
  result$basin_prob.median = median(basin.prob)
  result$basin_prob.max = max(basin.prob)
  result$basin_prob.sd = sd(basin.prob)

  ## compute basin(s) of attraction
  basin.size.uncertain = calcBasinsSize(fundamental.mat)
  fundamental.mat.certain = fundamental.mat[rowSums(fundamental.mat != 0) == 1, , drop = FALSE]
  basin.size.certain = calcBasinsSize(fundamental.mat.certain)

  result$basin_certain.min = min(basin.size.certain) / n
  result$basin_certain.mean = mean(basin.size.certain) / n
  result$basin_certain.median = median(basin.size.certain) / n
  result$basin_certain.max = max(basin.size.certain) / n
  result$basin_certain.sd = sd(basin.size.certain) / n
  result$basin_certain.sum = sum(basin.size.certain) / n
  result$basin_uncertain.min = min(basin.size.uncertain) / n
  result$basin_uncertain.mean = mean(basin.size.uncertain) / n
  result$basin_uncertain.median = median(basin.size.uncertain) / n
  result$basin_uncertain.max = max(basin.size.uncertain) / n
  result$basin_uncertain.sd = sd(basin.size.uncertain) / n
  result$basin_uncertain.sum = sum(basin.size.uncertain) / n
  
  # compute the probability to find the best cell
  yvals.attr = yvals[permutation.index[seq.closed.classes]]
  result$best_attr.prob = sum(basin.prob[yvals.attr == min(yvals.attr)])
  result$best_attr.no = sum(yvals.attr == min(yvals.attr)) / n

  return(result)
}

# calculates the size of each basin of attraction
calcBasinsSize = function(fundamental.mat) {
  basin = apply(fundamental.mat, 1, selectMax)
  vapply(seq_len(ncol(fundamental.mat)), function(i) sum(basin == i), integer(1L))
}

## check whether the coordinates of the neighbour cells (nb) of the current cell
## (cell) are invalid (TRUE) or valid (FALSE); i.e. return TRUE for those
## neighbours that are outside the bounds of blocks or that are the cell itself
## cell: cell coordinate of the current cell
## nb: cell coordinate of the neighbour
## blocks: given GCM divisions / blocks
isInvalidNeighbour = function(cell, nb, blocks) {
  any(nb <= 0) || any(nb > blocks) || all(nb == cell)
}
