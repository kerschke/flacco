calculateBarrierTreeFeatures = function (feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  if (feat.object$dim != 2)
    stop("The barrier trees can currently only be computed for 2-dimensional problems!")
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
      yvals[is.infinite(yvals)] = max(yvals[is.finite(yvals)]) * 100
      sparse.matrix = calculateSparseMatrix(feat.object, yvals)
      canonical.list = computeCanonical(sparse.matrix)
      fundamental.list = computeFundamental(
        canonical.list = canonical.list,
        gcm.control = gcm.control)
      barrier.tree = createBarrierTree(feat.object, fundamental.list,
        canonical.list, yvals, control)
      feats = computeBarrierTreeFeats(yvals, fundamental.list, barrier.tree, feat.object)
      names(feats) = sprintf("bt.%s.%s", approach, names(feats))
      return(feats)
    }), sprintf("bt.%s", approach))
  })
  return(unlist(result, recursive = FALSE))
}


createBarrierTree = function(feat.object, fundamental.list, canonical.list, yvals, control) {
  # store components of fundamental.list
  fundamental.mat = fundamental.list$fundamental.mat
  permutation.index = fundamental.list$permutation.index
  seq.closed.classes = fundamental.list$seq.closed.classes
  canonical.form = canonical.list$canonical.form
  finite.values = is.finite(yvals)

  # remove invalid (infinite) values
  canonical.form = canonical.form[finite.values, finite.values]
  yvals = yvals[finite.values]
  permutation.index = permutation.index[finite.values]

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

  cells = seq_along(permutation.index)
  diffs = predecessors = rep(0L, length(permutation.index))
  canonical.form = canonical.form[seq.closed.classes, seq.closed.classes, drop = FALSE]

  # copy of original yvals for later comparisons
  yvals.orig = yvals

  # combine columns that belong to the same basins
  key2 = rowSums(canonical.form != 0) >= 2 # [ orig: key2 = sum(I~=0,2) >= 2 ]
  non.zero = lapply(which(key2), function(i) which(canonical.form[i,] != 0))
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

  # Update matrices
  for (i in seq_along(non.zero)) {
    canonical.form[, non.zero[[i]][1]] = rowSums(canonical.form[, non.zero[[i]]])
    prob.absorb[, non.zero[[i]][1]] = rowSums(prob.absorb[, non.zero[[i]]])
  }

  remove.columns = unique(unlist(lapply(non.zero, function(x) x[-1])))
  if (length(remove.columns) > 0) {
    remove.columns = sort(unique(remove.columns)  )
    canonical.form = canonical.form[-remove.columns , -remove.columns, drop = FALSE]
    prob.absorb = prob.absorb[, -remove.columns, drop = FALSE]
  }

  is.attr = rowSums(prob.absorb != 0) < 2  # [ orig: key = sum(Apr~=0, 2) < 2 ]
  prob.absorb = prob.absorb[!is.attr, ]

  # 'keep' stores the indices of all non-attractor cells
  keep = sort(permutation.index[length(seq.closed.classes) + which(!is.attr)])
  #  [ orig: fe([indexPermutation(1:closedClassIndex); indexPermutation(closedClassIndex+find(key ~= 0))]) = []; ]
  a1 = permutation.index[seq.closed.classes]
  a2 = na.omit(permutation.index[length(seq.closed.classes) + which(is.attr)])
  yvals = yvals[-unique(c(a1, a2))]

  # Update permutation.index 
  if (length(remove.columns) > 0) {
    permutation.index  = permutation.index[-remove.columns]
  }

  # Put all canonical.form in the buffer    
  buffer = permutation.index[seq.closed.classes]
  # remove those which are actually zero-columns
  buffer = buffer[colSums(canonical.form) != 0]

  # find order
  yval.order = order(yvals) # [ orig: #   [~, indexPermutation2] = sort(fe); ]

  for (i in yval.order) {
    # make predecessor graph        
    key3 = which(prob.absorb[i, ] != 0)
    if (length(key3) <= 1) {
      next
    }

    g1 = rowSums(canonical.form[, key3] != 0) > 0 # [ orig: g1 = sum(I(:, key3) ~= 0, 2) > 0; ]
    g2 = rowSums(prob.absorb[, key3] != 0) > 0 # [ orig: g2 = sum(Apr(:, key3) ~= 0, 2) > 0; ]
    g  = c(permutation.index[seq_along(g1)][g1], keep[g2])

    # build intersection to join columns
    ib = intersect(g, buffer) # [ orig: [ib, ~, ic] = intersect(g, buffer); ]
    ic = match(ib, buffer) # TODO rename ib (intersection) and ic (indices in ib)
    predecessors[ib] = keep[i]
    diffs[ib] = yvals[i] - yvals.orig[ib]

    buffer = c(buffer[-ic], keep[i])

    # join basins and update indices
    canonical.form[, key3[1]] = rowSums(canonical.form[, key3]) # join
    canonical.form = canonical.form[-key3[-1], -key3[-1], drop = FALSE] # remove
    prob.absorb[ , key3[1]] = rowSums(prob.absorb [ , key3])
    prob.absorb = prob.absorb[ , -key3[-1], drop = FALSE]
    permutation.index = permutation.index[-key3[-1]]
    if (all(rowSums((prob.absorb != 0)) <= 1))
      break
  }

  root = buffer[1] # the last element in buffer becomes the root of the barrier tree

  # clean up vectors: only remaining elements are those cells which are part of the tree
  if (sum(predecessors == 0) > 0) {
    cells = cells[predecessors != 0]
    diffs = diffs[predecessors != 0]
    predecessors = predecessors[predecessors != 0]
  }

  # remove root from cells (otherwise we might have loops)
  cell.ind = which(cells == root)
  if (length(cell.ind) > 0) {
    cells = cells[-cell.ind]
    predecessors = predecessors[-cell.ind]
    diffs = diffs[-cell.ind]  
  }

  # initialise BFS
  next.level = cells[which(predecessors == root)] #children(root)

  # ugly work-around
  base = control_parameter(control, "bt.base", 4L)
  max.depth = control_parameter(control, "bt.max_depth", 16L)
  max.depth.possible = 1 + floor(length(unique(predecessors)) / 2)
  max.depth = min(max.depth, max.depth.possible)
  tree = rep(NA_integer_, base^max.depth)
  tree[1] = root
  max.levels = 0L
  if (length(next.level) > 0) {
    tree[1 + seq_along(next.level)] = next.level
  }
  while (length(next.level) > 0) {
    max.levels = max.levels + 1L
    # change to next level (BFS)
    current.level = next.level
    next.level = NULL
    for (i in current.level) {
      # append children of this node to the next level (BFS)
      potential.next.level = cells[which(predecessors == i)]
      index = yvals.orig[i] > yvals.orig[potential.next.level]
      potential.next.level = potential.next.level[index]
      next.level = c(next.level, potential.next.level)
      if (any(potential.next.level)) {
        old.index = which(tree == i)
        new.index = (old.index - 1) * base + 1 + seq_along(potential.next.level)
        tree[new.index] = potential.next.level
      }
    }
  }

  # 'cells': index of nodes
  # 'predecessors': index of parent nodes
  # 'diffs': differences in y-values
  # 'root': root of barrier tree
  # 'tree.nodes': nodes of the barrier tree
  # 'tree.index': indices of the tree nodes
  cell.index = which(cells %in% tree)
  return(list(cells = cells[cell.index],
    predecessors = predecessors[cell.index],
    diffs = diffs[cell.index],
    root = root,
    prob.absorb = prob.absorb.orig,
    tree.nodes = tree[!is.na(tree)],
    tree.index = which(!is.na(tree)),
    base = base,
    max.levels = max.levels)
  )
}

# Careful:
# The matlab code made the assumption that the minimum is a leaf in the lowest
# level of the tree, thus traversing it from minimum to the root. Example runs
# showed that this is not necessarily true. While lower values cannot be child
# nodes of the minimum, they can be child nodes of a sibling. Therefore, rather
# traverse from the root until there are no further children, using BFS. This
# is a litte bit more expensive, but probably worth the effort.
computeBarrierTreeFeats = function(yvals, fundamental.list, barrier.tree, feat.object) {
  seq.closed.classes = fundamental.list$seq.closed.classes
  permutation.index = fundamental.list$permutation.index
  prob.absorb = barrier.tree$prob.absorb
  predecessors = barrier.tree$predecessors
  root = barrier.tree$root
  cells = barrier.tree$cells
  diffs = barrier.tree$diffs
  base = barrier.tree$base
  max.node.per.level = cumsum(barrier.tree$base^(0:barrier.tree$max.levels))
  levels = vapply(barrier.tree$tree.index, function(x)
    min(which(x <= max.node.per.level)), integer(1L)) - 1L
  depth = diff(range(yvals[barrier.tree$tree.nodes]))

  # storing depth-features
  feats = vector(mode = "list", length = 0L)
  feats$levels = max(levels)
  feats$leaves = length(seq.closed.classes)
  feats$depth = depth
  if (max(levels) != 0)
    feats$depth_levels_ratio = depth / max(levels)
  else
    feats$depth_levels_ratio = NA_real_

  # ratio levels/nodes
  feats$levels_nodes_ratio = max(levels) / length(seq.closed.classes)

  if (length(diffs) > 0) {
    # location and dispersion of 'weights' (diffs)
    feats$diffs.min = min(diffs)
    feats$diffs.mean = mean(diffs)
    feats$diffs.median = median(diffs)
    feats$diffs.max = max(diffs)
    feats$diffs.sd = sd(diffs)
  
    # average diff per level
    avg.diffs = vapply(unique(levels)[-1], function(lvl) {
      x = barrier.tree$tree.nodes[levels == lvl]
      mean(barrier.tree$diffs[barrier.tree$cells %in% (x)])
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
