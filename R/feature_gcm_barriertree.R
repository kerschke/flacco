calculateBarrierTreeFeatures = function (feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  assertList(control)
  allows.cellmapping = control_parameter(control, "allow_cellmapping", TRUE)
  if (!allows.cellmapping)
    stop("You can not prohibit cell-mapping features and still try to compute them!")
  if (feat.object$total.cells == 1L)
    stop("The barrier tree features can't be computed as long as the feature object consists of a single cell!")

  approaches = control_parameter(control, "gcm.approaches", c("min", "mean", "near"))
  cf.power = control_parameter(control, "gcm.cf_power", 256L)
  assertInt(cf.power, lower = 1L, upper = Inf)
  gcm.control = list(cf.power = cf.power)

  result = lapply(approaches, function(approach) {
    measureTime(expression({
      yvals = getObjectivesByApproach(feat.object, approach)
      yvals[is.infinite(yvals)] = max(yvals[is.finite(yvals)]) * 100
      ## creates a sparse matrix (m by m, where m is the number of cells),
      ## which contains the probabilities to move from cell A (row of the
      ## matrix) to cell B (column of the matrix)
      sparse.matrix = calculateSparseMatrix(feat.object, yvals)
      canonical.list = computeCanonical(sparse.matrix)
      fundamental.list = computeFundamental(
        canonical.list = canonical.list,
        gcm.control = gcm.control)
      barrier.tree = createBarrierTree(feat.object, fundamental.list,
        canonical.list, yvals = getObjectivesByApproach(feat.object, approach),
        control)
      feats = computeBarrierTreeFeats(yvals, fundamental.list, barrier.tree, feat.object)
      names(feats) = sprintf("bt.%s.%s", approach, names(feats))
      return(feats)
    }), sprintf("bt.%s", approach))
  })
  return(unlist(result, recursive = FALSE))
}


createBarrierTree = function(feat.object, fundamental.list, canonical.list, yvals, control) {
  ## store components of fundamental.list
  permutation.index = fundamental.list$permutation.index
  seq.closed.classes = fundamental.list$seq.closed.classes
  canonical.form = canonical.list$canonical.form
  lsc = length(seq.closed.classes)

  ## store the perm-index in a 2-column matrix in order to keep track of the
  ## transformations of the permutation index (compared to the original values)
  perm.index = cbind(p.index = permutation.index, p.index2 = permutation.index,
    index = seq_along(permutation.index), yvals = yvals[permutation.index])

  ## remove entries that correspond to cells with infinite y-values
  finite.values = is.finite(perm.index[, 4L])
  # index.of.infinite = perm.index[!finite.values, 1L]
  canonical.form = canonical.form[finite.values, finite.values]
  perm.index = perm.index[finite.values,]
  perm.index[, 1L] = rank(perm.index[, 1L])

  # Probability of transient to transient state [Q = P(idx(sv+1:end),idx(sv+1:end));]
  Q = canonical.form[-seq.closed.classes, -seq.closed.classes]

  # Probability of transient to absorbing state [R = P(idx(sv+1:end),idx(1:sv));]
  R = canonical.form[-seq.closed.classes, seq.closed.classes, drop = FALSE]

  # Note that the decomposition is not necessarily unique
  # [[L,U] = lu(speye(length(Q)) - Q)]
  decomp = Matrix::expand(Matrix::lu(diag(nrow(Q)) - Q))

  # L\R [Matlab] <--> solve(L, R) [R]
  LdR = Matrix::solve(decomp$P %*% decomp$L, R)

  # U\(L\R) [Matlab] <--> solve(U, LdR) [R]
  prob.absorb = as.matrix(Matrix::solve(decomp$U, LdR))
  prob.absorb.orig = prob.absorb

  ## restrict canonical form to the attractor cells
  canonical.form = canonical.form[seq.closed.classes, seq.closed.classes, drop = FALSE]

  ## look for possible plateaus among attractors
  key.plateau = rowSums(canonical.form != 0) >= 2 # [ orig: key2 = sum(I~=0,2) >= 2 ]
  non.zero = lapply(which(key.plateau), function(i) which(canonical.form[i,] != 0))
  plateaus = NULL
  remove.columns = NULL

  ## if there exist plateaus, "merge" them
  if (length(non.zero) > 0) {
    for (i in rev(seq_along(non.zero)[-1])) {
      x = non.zero[[i]]
      common.basin = vapply(non.zero, function(y) length(intersect(y, x)) > 0, logical(1L))
      if (any(common.basin[-i])) {
        non.zero = lapply(seq_along(non.zero), function(j) {
          if (common.basin[j])
            return(sort(unique(c(non.zero[[j]], x))))
          else
            return(non.zero[[j]])
        })
        non.zero[[i]] = NULL
      }
    }

    plateaus = lapply(non.zero, function(ids) {
      perm.index[perm.index[, 3L] %in% ids, 2L]
    })

    ## Update matrices, i.e., add the probabilites of the columns that belong
    ## to the same "attractor" within the first cell of that basin and remove
    ## the remaining columns
    for (i in seq_along(non.zero)) {
      canonical.form[, non.zero[[i]][1L]] = rowSums(canonical.form[, non.zero[[i]]])
      prob.absorb[, non.zero[[i]][1L]] = rowSums(prob.absorb[, non.zero[[i]]])
    }
    remove.columns = sort(unique(unlist(lapply(non.zero, function(x) x[-1L]))))
    canonical.form = canonical.form[-remove.columns, -remove.columns, drop = FALSE]
    prob.absorb = prob.absorb[, -remove.columns, drop = FALSE]
  }
  perm.index2 = perm.index[-seq.closed.classes,,drop = FALSE]

  ## initialize cells, diffs and predecessors
  perm.index = cbind(perm.index, cells = seq_row(perm.index),
    diffs = 0, predecessors = 0)

  ## Put all attractors in the buffer and remove those which actually have
  ## zero-columns
  if (length(remove.columns) > 0) {
    perm.ind.attr = perm.index[seq.closed.classes[-remove.columns], 2L]
  } else {
    perm.ind.attr = perm.index[seq.closed.classes, 2L]
  }
  buffer = perm.ind.attr[colSums(canonical.form) != 0]

  # 8  7 12  3 13 11  2  4  9  6 14  5  1 10
  for (i in order(perm.index2[, 4L])) {
    ## we are done, once all rows (and thereby transient cells) contain (at
    ## most) one non-zero probability
    if (all(rowSums(prob.absorb != 0) <= 1))
      break

    ## make predecessor graph;
    ## start with the (transient) cell that has the lowest y-value
    key3 = which(prob.absorb[i, ] != 0)
    if (length(key3) <= 1) {
      ## predecessor has to connect at least two basins (!)
      next
    }

    ## which cells lead to the same basins as the current cell
    g1 = rowSums(canonical.form[, key3] != 0) > 0 # [ orig: g1 = sum(I(:, key3) ~= 0, 2) > 0; ]
    g2 = rowSums(prob.absorb[, key3] != 0) > 0 # [ orig: g2 = sum(Apr(:, key3) ~= 0, 2) > 0; ]
    g  = c(perm.ind.attr[g1], perm.index2[g2, 2L])

    ## which cells are already in the buffer (i.e. have no predecessor),
    ## also belong to the same basins and therefore should be merged
    ## by the current cell
    isec = intersect(g, buffer) # [ orig: [ib, ~, ic] = intersect(g, buffer); ]
    ## check whether any of the current elements belongs to a plateau
    if (!is.null(plateaus)) {
      plateau.index = vapply(plateaus, function(ind) any(ind %in% isec), logical(1L))
      if (any(plateau.index))
        isec = union(isec, plateaus[[which(plateau.index)]])
    }
    ## i is predecessor of the elements that are in the buffer AND in g
    indi = which(perm.index[, 2L] %in% isec)
    perm.index[indi, 7L] = perm.index2[i, 2L]
    perm.index[indi, 6L] = perm.index2[i, 4L] - perm.index[indi, 4L]

    ## remove the cells (from the buffer) which were just merged
    ## and add the current branching cells
    buffer = union(setdiff(buffer, isec), perm.index2[i, 2L])

    ## join basins
    canonical.form[, key3[1]] = rowSums(canonical.form[, key3])
    canonical.form[, key3[-1]] = canonical.form[key3[-1], ] = 0
    prob.absorb[, key3[1]] = rowSums(prob.absorb[, key3])
    prob.absorb[, key3[-1]] = 0
  }

  ## the last remaining element in buffer becomes the root of the barrier tree
  root = buffer[1]

  ## only keep cells which are part of the tree
  ## (= if NOT ALL elements have a predecessor)
  if (sum(perm.index[,7] == 0) > 0) {
    rel.ind = sort(union(perm.index[seq.closed.classes, 2], perm.index[perm.index[,7] > 0, 7]))
    perm.index3 = perm.index[perm.index[,2] %in% rel.ind,, drop = FALSE]
  } else {
    perm.index3 = perm.index
  }
  base.value = max(table(perm.index3[,7L]))

  ## remove root from cells (otherwise we might have loops)
  root.ind = which(perm.index3[, 2L] == root)
  if (length(root.ind) > 0) {
    perm.index3 = perm.index3[-root.ind, , drop = FALSE]
  }

  ## initialise BFS
  next.level = perm.index3[which(perm.index3[,7] == root), 2L] #children(root)

  ## ugly work-around
  bt.base = control_parameter(control, "bt.base", base.value)
  max.depth = control_parameter(control, "bt.max_depth", 16L)
  max.depth.possible = 1L + floor(length(unique(perm.index3[,7])) / 2)
  max.depth = min(max.depth, max.depth.possible)
  tree = rep(NA_integer_, bt.base^max.depth)
  tree[1L] = root
  max.levels = 0L
  if (length(next.level) > 0) {
    tree[1L + seq_along(next.level)] = next.level
  }
  while (length(next.level) > 0) {
    max.levels = max.levels + 1L
    ## change to next level (BFS)
    current.level = next.level
    next.level = NULL
    for (i in current.level) {
      ## append children of this node to the next level (BFS)
      potential.next.level = perm.index3[perm.index3[,7] == i, 2L]
      if (length(potential.next.level) == 0)
        next
      i1 = which(perm.index3[,2L] == i)
      i2 = which(perm.index3[,7L] == i)
      index = perm.index3[i1, 4L] > perm.index3[i2, 4L]
      potential.next.level = potential.next.level[index]
      next.level = c(next.level, potential.next.level)
      if (any(index)) {
        old.index = which(tree == i)
        new.index = (old.index - 1L) * bt.base + 1L + seq_along(potential.next.level)
        tree[new.index] = potential.next.level
      }
    }
  }

  # 'cells': index of nodes (without the root node!)
  # 'predecessors': index of parent nodes
  # 'diffs': differences in y-values
  # 'root': root of barrier tree
  # 'tree.nodes': nodes of the barrier tree
  # 'tree.index': indices of the tree nodes
  return(list(cells = as.integer(perm.index3[, 2L]),
    predecessors = as.integer(perm.index3[, 7L]),
    diffs = perm.index3[, 6L],
    root = as.integer(root),
    prob.absorb = prob.absorb.orig,
    tree.nodes = as.integer(tree[!is.na(tree)]),
    tree.index = as.integer(which(!is.na(tree))),
    base = as.integer(bt.base),
    max.levels = as.integer(max.levels))
  )
}

## Careful:
## The matlab code made the assumption that the minimum is a leaf in the lowest
## level of the tree, thus traversing it from minimum to the root. Example runs
## showed that this is not necessarily true. While lower values cannot be child
## nodes of the minimum, they can be child nodes of a sibling. Therefore, rather
## traverse from the root until there are no further children, using BFS. This
## is a litte bit more expensive, but probably worth the effort.
computeBarrierTreeFeats = function(yvals, fundamental.list, barrier.tree, feat.object) {
  seq.closed.classes = fundamental.list$seq.closed.classes
  permutation.index = fundamental.list$permutation.index
  prob.absorb = barrier.tree$prob.absorb
  predecessors = barrier.tree$predecessors
  root = barrier.tree$root
  cells = barrier.tree$cells
  diffs = barrier.tree$diffs
  bt.base = barrier.tree$base
  max.node.per.level = cumsum(bt.base^(0:barrier.tree$max.levels))
  levels = vapply(barrier.tree$tree.index, function(x)
    min(which(x <= max.node.per.level)), integer(1L)) - 1L
  depth = diff(range(yvals[barrier.tree$tree.nodes]))

  # storing depth-features
  feats = vector(mode = "list", length = 0L)
  feats$levels = max(levels)
  feats$leaves = length(seq.closed.classes)
  feats$depth = depth
  if (max(levels) != 0L)
    feats$depth_levels_ratio = depth / max(levels)
  else
    feats$depth_levels_ratio = NA_real_

  # ratio levels/nodes
  feats$levels_nodes_ratio = max(levels) / length(seq.closed.classes)

  if (length(levels) > 1L) {
    # location and dispersion of 'weights' (diffs)
    feats$diffs.min = min(diffs)
    feats$diffs.mean = mean(diffs)
    feats$diffs.median = median(diffs)
    feats$diffs.max = max(diffs)
    feats$diffs.sd = sd(diffs)
  
    # average diff per level
    avg.diffs = vapply(unique(levels)[-1L], function(lvl) {
      x = barrier.tree$tree.nodes[levels == lvl]
      mean(barrier.tree$diffs[barrier.tree$cells %in% x])
    }, double(1L))
    feats$level_diffs.min = min(avg.diffs)
    feats$level_diffs.mean = mean(avg.diffs)
    feats$level_diffs.median = median(avg.diffs)
    feats$level_diffs.max = max(avg.diffs)
    feats$level_diffs.sd = sd(avg.diffs)  
  } else {
    x = as.list(rep(NA_real_, 10L))
    x = setNames(x, sprintf("%s.%s", rep(c("diffs", "level_diffs"), each = 5),
      rep(c("min", "mean", "median", "max", "sd"), 2)))
    feats = c(feats, x)
  }

  # distances from local best to global best attractors
  index.ybest = selectMin(yvals)
  global.opt = ztox(z = celltoz(index.ybest, feat.object$blocks),
    cell.size = feat.object$cell.size, 
    lower = feat.object$lower
  )
  distances = vapply(seq.closed.classes, function(i) {
    local.opt = ztox(z = celltoz(permutation.index[i], feat.object$blocks),
      cell.size = feat.object$cell.size, 
      lower = feat.object$lower
    )
    sqrt(sum((global.opt - local.opt)^2))
  }, double(1L))
  feats$attractor_dists.min = min(distances)
  feats$attractor_dists.mean = mean(distances)
  feats$attractor_dists.median = median(distances)
  feats$attractor_dists.max = max(distances)
  feats$attractor_dists.sd = sd(distances)  

  # Basins size ratio
  basin.uncertain = vapply(seq.closed.classes, function(i) {
    sum(prob.absorb[, i] > 1e-15) + 1L
  }, integer(1L))
  basin.certain = vapply(seq.closed.classes, function(i) {
    sum(abs(prob.absorb[, i] - 1) < 1e-15) + 1L
  }, integer(1L))

  # b1 stores, which attractor is most likely
  b1 = apply(prob.absorb, 1, selectMax)
  basin.max = vapply(seq.closed.classes,
    function(i) sum(b1 == i) + 1L, integer(1L))
  feats$basin_ratio.uncertain = max(basin.uncertain) / min(basin.uncertain)
  feats$basin_ratio.certain = max(basin.certain) / min(basin.certain)
  feats$basin_ratio.most_likely = max(basin.max) / min(basin.max)

  # Intersection of (global with local) basins
  global.basin = which(prob.absorb[ , which(permutation.index == index.ybest)] > 1e-15)
  basin.intersection.count = vapply(seq.closed.classes, function(i) {
    local.basin = which(prob.absorb[ , i] > 1e-15)
    length(intersect(global.basin, local.basin)) + 1L
  }, integer(1L))

  total.cells = feat.object$total.cells
  feats$basin_intersection.min = min(basin.intersection.count) / total.cells
  feats$basin_intersection.mean = mean(basin.intersection.count) / total.cells
  feats$basin_intersection.median = median(basin.intersection.count) / total.cells
  feats$basin_intersection.max = max(basin.intersection.count) / total.cells
  feats$basin_intersection.sd = sd(basin.intersection.count) / total.cells

  # range of basin
  coord = vapply(seq_along(global.basin), function(i) {
    celltoz(global.basin[i], feat.object$blocks)
  }, integer(feat.object$dim))
  feats$basin_range = sqrt(sum((apply(coord, 1, function(x) diff(range(x))))^2))
  return(feats)
}

## computes the center point of a given cell coordinate z
ztox = function(z, cell.size, lower) {
  return(lower + cell.size * (z - 0.5))
}
