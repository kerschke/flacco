#' @title Calculate GCM features for a given function
#'
#' @description
#' Based on Generalized Cell Mapping (GCM) techniques, Barrier Trees of the landscape
#' are created from cell mappings. Thirty features are calculated from these Barrier Trees.
#' 
#' Provided the decision space is divided into a grid of cells, GCM incorporates
#' the dynamics of a system into the cell mapping. For each cell, the probabilities of transitioning
#' to successor cells are calculated, where each cell can have multiple successor cells. 
#'  
#' Computation is performed based on \code{min}, \code{mean}, and \code{near} approaches for
#' finding a representative objective function value for each cell.
#'
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#'   A feature object as created by \code{\link{createFeatureObject}}.
#'   Note that the object needs to contain the number of blocks per dimension
#'   (parameter \code{blocks}) in order to allow cellmapping features to compute.
#' @param control [\code{\link{list}}]\cr
#'   A list object that stores additional configuration parameters.
#'   The following parameter is used here:
#'   [\code{barrierTree.plot}] is a boolean, indicating whether barrier trees are visualised in a plot [experimental].
#' @return [\code{\link{list}(30)} of \code{\link{numeric}(1)}].\cr
#'   List of features.\cr
#'   For further information, see details.
#' @details
#' For each GCM approach, i.e. \code{min}, \code{mean}, and \code{near}, ten features are computed:\cr
#'   \code{barrierTree.{min, mean, near}$levels}: Levels of the barrier tree,\cr
#'   \code{barrierTree.{min, mean, near}$depth}: Distance from root of the barrier tree to the node with the minimum value,\cr
#'   \code{barrierTree.{min, mean, near}$ratioDepthLevels}: Ratio depth/levels,\cr
#'   \code{barrierTree.{min, mean, near}$ratioLevelsLeaves}: Ratio levels/leaves (where leaves is the number of leaf nodes of the tree),\cr
#'   \code{barrierTree.{min, mean, near}${min, mean, max, mean, std}w}: Aggregations of weights, i.e. distances between tree nodes,\cr
#'   \code{barrierTree.{min, mean, near}${min, mean, max, mean, std}Distance}: Aggregations of distances between global and local optima,\cr
#'   \code{barrierTree.{min, mean, near}$bratio{Uncertain, Certain, Max}}: Ratio of maximum and minimum of {uncertain, certain, and maximum}\cr basins,
#'   \code{barrierTree.{min, mean, near}${min, mean, max, mean, std}BasinIntersectionCount}: Aggregations of numbers of intersections, intersecting global and local basins,\cr
#'   \code{barrierTree.{min, mean, near}$range}: Range of basins.
#' @references
#'  For the underlying concept of GCM refer to Kerschke et al. (2014), 
#'  \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis}, 
#'  in EVOLVE-A Bridge between Probability, Set Oriented Numerics, and Evolutionary Computation V,
#'  pp. 115-131 (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
#' @examples
#' # (1) create the initial design:
#' X = t(replicate(5000, runif(2, -1000, 1000)))
#' y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
#' feat.object = createFeatureObject(X = X, y = y, 
#'   lower = -1000, upper = 1000, blocks = 10)
#' # (2) compute the GCM-based Barrier Tree features:
#' calculateBarrierTrees(feat.object = feat.object, control = list(barrierTree.plot=TRUE))
#' @export 
calculateBarrierTrees = function (feat.object, control = list()) {
  assertClass(feat.object, "FeatureObject")
  assertList(control)
  # [ for gcm: x is var, y is fun ]
  if (!feat.object$allows.cellmapping) {
    stop( paste(c("This feature object does not support cell mapping. You need to ",
          "create a feature object with the parameter 'blocks', defining the number ",
          "of divisons per dimension.")) )
  }

  # Visualisation option
  visualise_barrierTrees = control_parameter(control, "barrierTree.plot", FALSE)
  visualise_barrierTrees.colors = control_parameter(control, "barrierTree.plot.colors", NULL)
  
  # ensure that canonical form and representatives per cell are initialised (for min, mean, and near)
  gcm_init(feat.object)
  
  #Evaluate barrier tree features for Min
  barrierTree.min = evaluate_barrierTree(
                      feat.object$env$gcm.representatives$min, 
                      feat.object$env$gcm.canonicalForm$min,
                      feat.object$blocks, feat.object$lower, feat.object$upper, feat.object$cell.size,
                      visualise_barrierTrees, visualise_barrierTrees.colors)
  
  #Evaluate barrier tree features for Mean
  barrierTree.mean = evaluate_barrierTree(
                      feat.object$env$gcm.representatives$mean, 
                      feat.object$env$gcm.canonicalForm$mean,
                      feat.object$blocks, feat.object$lower, feat.object$upper, feat.object$cell.size,
                      visualise_barrierTrees, visualise_barrierTrees.colors)
  
  #Evaluate barrier tree features for Near
  barrierTree.near = evaluate_barrierTree(
                      feat.object$env$gcm.representatives$near, 
                      feat.object$env$gcm.canonicalForm$near,
                      feat.object$blocks, feat.object$lower, feat.object$upper, feat.object$cell.size,
                      visualise_barrierTrees, visualise_barrierTrees.colors)
  
  return (list(
    barrierTree.min = barrierTree.min,
    barrierTree.mean = barrierTree.mean,
    barrierTree.near = barrierTree.near
    ))
}

# ( origin: barrierTreeGCM3.m, features.m )
evaluate_barrierTree = function(fe, canform,
                                 divisions, lowerBounds, upperBounds, cellSize,
                                 visualise_barrierTrees, visualise_barrierTrees.colors) {
  # rename canform list's components
  canonicalForm    = canform$canonicalForm
  indexPermutation = canform$indexPermutation
  closedClassIndex = canform$closedClassIndex
  
  seqClosedClasses = seq_len( closedClassIndex ) # used often => cached for speedup & smaller memory footprint
  
  #initialise return values
  levels = depth = ratioDepthLevels = 
    minw = meanw = maxw = stdw = 
    ratioLevelsLeaves =
    minDistance = meanDistance = maxDistance = stdDistance = 
    bratioUncertain = bratioCertain = bratioMax = 
    minBasinIntersectionCount = meanBasinIntersectionCount = 
    maxBasinIntersectionCount = stdBasinIntersectionCount = range = 0
  
  if (closedClassIndex > 1) {
    # submatrices Q and R from the canonical form:
    #Q: Probability of transitioning from some transient state [Q = P(idx(sv+1:end),idx(sv+1:end));]
    Q = canonicalForm[(closedClassIndex+1) : length(indexPermutation),
                       (closedClassIndex+1) : length(indexPermutation)]
    
    #R: Probability of transitioning from some transient state to some absorbing state [R = P(idx(sv+1:end),idx(1:sv));]
    R = canonicalForm[(closedClassIndex+1) : length(indexPermutation),
                       seqClosedClasses]
    
    # calculate absorbing probabilities (Apr)
    
    ## (translation note: lu calculates LU decompositions of a matrix.
    ## However, in matlab, [L,U] = lu(A) implies A = L %*% U, whereas
    ## in R, list [L,U,P] = expand(lu(A)) implies A= (P %*% L) %*% U.
    ## Therefore, P needs to be considered (see below).
    ## Furthermore, note that the decomposition is not necessarily unique.)
    decomp = Matrix::expand( Matrix::lu( diag(nrow(Q)) - Q ) ) # [ orig: [L,U] = lu( speye(length(Q)) - Q ) ) ]
    
    ## (translation note: A\B should equal solve(A, B), as both solve the
    ##  equation A %*% x = B for x )
    Apr = solve(decomp$U, solve( decomp$P %*% decomp$L, R))
    # Apr(isnan(Apr)) = 0; # TODO check for isnan necessary? what is the equivalent in R? up to now, there are no missing values or thelike
    
    # calculate barrier tree
    barrierTree = create_barrierTree(
      canonicalForm[seqClosedClasses, seqClosedClasses], # [ orig: I ]
      Apr, fe, indexPermutation, closedClassIndex)
    
    # convert into features
    list [levels, depth, ratioDepthLevels,
          minw, meanw, maxw, stdw,
          ratioLevelsLeaves,
          minDistance, meanDistance, maxDistance, stdDistance,
          bratioUncertain, bratioCertain, bratioMax,
          minBasinIntersectionCount, meanBasinIntersectionCount, maxBasinIntersectionCount,
          stdBasinIntersectionCount, range] = features_barrierTree( 
            fe, barrierTree$cells, root = barrierTree$root, barrierTree$diffs, divisions, 
            cellSize, lowerBounds, Apr, 
            indexPermutation, closedClassIndex, barrierTree$preds)
    
    
    if (visualise_barrierTrees) {
      
      plotBarrierTree3d(barrierTree$cells, barrierTree$preds,
                        barrierTree$diffs, barrierTree$root, fe, divisions, levels,
                        visualise_barrierTrees.colors)
      #plotBarrierTreePCA(barrierTree$cells, barrierTree$preds, 
      #                   barrierTree$diffs, barrierTree$root, fe, divisions)
    }
  } else {
    # barrier trees could not be calculated
    if (visualise_barrierTrees) {
      #title(sub="Barrier trees cannot be calculated for this instance.")
      plot.new() # skip plot
    }
  }
  
  return( list( 
    levels = levels, depth = depth, ratioDepthLevels = ratioDepthLevels,  # levels & depth
    ratioLevelsLeaves = ratioLevelsLeaves,                                # ratio levels/leaves
    minw = minw, meanw = meanw, maxw = maxw, stdw = stdw,                 # loc/disp of weights
    minDistance = minDistance, meanDistance = meanDistance,               # distances global -> local optima
    maxDistance = maxDistance, stdDistance = stdDistance,
    bratioUncertain = bratioUncertain, bratioCertain = bratioCertain, bratioMax = bratioMax, # basins
    minBasinIntersectionCount = minBasinIntersectionCount,                # loc/disp of basins
    meanBasinIntersectionCount = meanBasinIntersectionCount,
    maxBasinIntersectionCount = maxBasinIntersectionCount,
    stdBasinIntersectionCount = stdBasinIntersectionCount,
    range = range                                                         # ranges of basins
  ))
}

# I contains the minimums
# Apr contains the saddle points
# [ orig: function [z pred w root] = barrierTreeGCM3(I, Apr, fx, idx, sv) ]
create_barrierTree = function(I, Apr, fe, indexPermutation, closedClassIndex) {
  seqClosedClasses = seq_len(closedClassIndex)
  
  cells = seq_len(  length(indexPermutation) )
  pred = rep(0, length(indexPermutation)) # "0" indicates no predecessor. Elements corresponding to pred==0 will be removed from all vectors in the end.
  diffs = rep(0, length(indexPermutation))
  # [ orig: %Make full [I; Apr] ]
  fe2 = fe # create copy of function evaluations for later comparison
  
  # Add (i.e. combine) columns that belong to the same basins
  key2 = rowSums(I != 0) >= 2 # [ orig: key2 = sum(I~=0,2) >= 2 ]
  
  removeCols = c()
  for (i in seq_len( length(key2) ) ) {
    # find the columns of this row for which the cell value is > 0
    nonZeroColumns = which(I[i, ] != 0)
    if (length(nonZeroColumns) <= 1) {
      next
      # the following is basically a no-op if there is only one such column -- therefore save time!
    } 
    
    # then, combine all column values of that basin into the first column
    I[ , nonZeroColumns[1] ] = rowSums( as.matrix(I[ , nonZeroColumns]))
    Apr[ , nonZeroColumns[1] ] = rowSums( as.matrix(Apr[ , nonZeroColumns]))
    
    # prepare for removing the remaining columns
    removeCols = c(removeCols, nonZeroColumns[ 2:length(nonZeroColumns) ])
  }
  
  # Update matrices
  if (length(removeCols) > 0) {
    removeCols = unique(removeCols)  
    I = as.matrix(I[-removeCols , -removeCols])
    Apr = as.matrix(Apr[ , -removeCols])
  }
  
  key = rowSums(Apr != 0) < 2  # [ orig: key = sum(Apr~=0, 2) < 2 ]
  Apr = as.matrix(Apr)[-which(key), ]
  
  keep = sort(indexPermutation[closedClassIndex+which(key == FALSE)])
  fe = fe[-c(     #  [ orig: fe([indexPermutation(1:closedClassIndex); indexPermutation(closedClassIndex+find(key ~= 0))]) = []; ]
    indexPermutation[seqClosedClasses],
    indexPermutation[closedClassIndex+which(key == TRUE)]
  )]
  
  #Update indexPermutation
  if (length(removeCols) > 0) {
    indexPermutation = indexPermutation[-removeCols]
    closedClassIndex = closedClassIndex - length(removeCols)
  }
  seqClosedClasses = seq_len(closedClassIndex)
  
  #Put all I in the buffer    
  buffer = indexPermutation[seqClosedClasses]
  # remove those which are actually zero-columns
  zeroCols = which(colSums(I) == 0)
  if (length(zeroCols) > 0) {
    buffer = buffer[-zeroCols]
  }
  
  #sort fe, obtain permutation ($ix of sort output)
  indexPermutation2 = sort(fe, index.return=TRUE)$ix # [ orig: #   [~, indexPermutation2] = sort(fe); ]
  
  
  for (i in 1:length(fe)) {
    #make predecesor graph        
    key3 = which(Apr[indexPermutation2[i], ] != 0)
    if (length(key3) == 1) {
      next
    }
    
    g1 = rowSums( I  [ , key3] != 0) > 0 # [ orig: g1 = sum(I(:, key3) ~= 0, 2) > 0; ]
    g2 = rowSums( Apr[ , key3] != 0) > 0 # [ orig: g2 = sum(Apr(:, key3) ~= 0, 2) > 0; ]
    g  = c(  # [ orig: g = [indexPermutation[length(g1)][g1]; keep(g2)]; ]
      indexPermutation[1:length(g1)][g1],
      keep[g2]
    ) # TODO rename g
    
    
    # build intersection to join columns
    ib = intersect(g, buffer) # [ orig: [ib, ~, ic] = intersect(g, buffer); ]
    ic = match(ib, buffer) # TODO rename ib (intersection) and ic (indices in ib)
    pred[ib]  = keep[ indexPermutation2[i] ]
    diffs[ib] = fe[ indexPermutation2[i] ] - fe2[ib]
    
    buffer = c(
      buffer[-ic], # [ orig: buffer(ic) = []; ]
      keep[ indexPermutation2[i] ] # [ orig: buffer = [buffer; keep(indexPermutation2(i))]; ]
    )
    
    #join basins (add columns)
    I[ , key3[1]] = rowSums( I[ , key3]) # join
    I = as.matrix(I [ -key3[2:length(key3)] , -key3[2:length(key3)] ]) # remove
    Apr[ , key3[1]] = rowSums(Apr [ , key3])
    Apr = as.matrix(Apr[ , -key3[2:length(key3)] ])
    
    # update cci, indices
    closedClassIndex = closedClassIndex - length(key3) + 1 # [ orig: closedClassIndex - length(key3(2:end)) ]
    indexPermutation = indexPermutation[ -key3[2:length(key3)] ]
  }
  
  root = buffer # the last element in buffer becomes the root of the barrier tree
  
  # clean up lists: only remaining elements are those cells which part of the tree
  if ( length(pred[ pred == 0 ]) > 0) {
    cells    = cells[-which(pred==0)]
    diffs    = diffs[-which(pred==0)]
    pred     = pred[-which(pred==0)]
  }
  
  #%     DG = sparse(pred, z, true, len, len);
  #%     h = view(biograph(DG))
  return ( list( cells = cells,     # list of nodes (-> index of function evaluation)
                 preds = pred,      # list of predecessors of nodes (-> parent node index)
                 diffs = diffs,     # difference value 
                 root = root ) )    # root of barrier tree
}


# [ orig: function [levels, depth, ratio1, minw, meanw, maxw, stdw, ratio2, mind, meand, maxd, stdd, bratio1, bratio2, bratio3, mini, meani, maxi, stdi, range] = features( fe, z2, root, weights, N, h, lb, Apr, idx, sv, pred2 ) ]
features_barrierTree = function( fe, cells, root, weights, divisions, 
                                  cellSize, lowerBounds, Apr, indexPermutation, 
                                  closedClassIndex, preds ) {
  seqClosedClasses = seq_len(closedClassIndex)
  
  #Levels
  # [Careful: The matlab code made the assumption that the minimum is a leaf in the lowest
  #  level of the tree, thus traversing it from minimum to the root.
  #  Example runs showed that this is not necessarily true. While lower values cannot be
  #  child nodes of the minimum, they can be child nodes of a sibling.
  #  Therefore, rather traverse from the root until there are no further children, 
  #  using BFS. This is a litte more expensive, but probably worth the effort...]

  # initialise BFS
  nextLevel = which(preds == root) #children(root)
  levels = 0
  while (length(nextLevel) > 0) {
    # change to next level (BFS)
    currentLevel = nextLevel
    nextLevel = c()
    levels = levels + 1
    
    # draw nodes of current level (BFS)
    for (i in currentLevel) {
      # append children of this node to the next level (BFS)
      nextLevel = c(nextLevel,  which(preds == cells[i]) ) # append children(i)
    }
  }
  
  
  #Depth of tree
  depth = abs(fe[root] - min(fe)) # diverted from the very suspicious "abs(fe[root]) - abs(min(fe))"
  
  #ratio of depth/longest
  ratioDepthLevels = depth/levels
  
  #location and dispersion of weights
  minw  = min(weights)
  meanw = mean(weights)
  maxw  = max(weights)
  stdw  = sd(weights)
  
  #ratio levels/leaves
  ratioLevelsLeaves = levels/closedClassIndex
  
  #Distances locals to global
  minValueIdx = which.min(fe)
  gopt = ztox(celltoz(minValueIdx, divisions), cellSize, 
               lowerBounds) # center of the cell containing the global optimum
  
  distances = na.omit(
    sapply( seqClosedClasses , function(i) {
      if ( i == which(indexPermutation==minValueIdx) ) {
        return (NA)
      }
      
      lopt = ztox(celltoz(indexPermutation[i], divisions), cellSize, lowerBounds)
      return (
        sqrt( sum( (gopt-lopt)^2 ) )
      )
      
    })
  )
  
  minDistance  = min(distances)
  meanDistance = mean(distances)
  maxDistance  = max(distances)
  stdDistance  = sd(distances)
  
  
  #----------------------------------
  #Basins size ratio
  
  basinUncertain = sapply(seqClosedClasses , function(i) {
    nnz( Apr[ , i] )
  })
  basinCertain = sapply(seqClosedClasses, function(i) {
    nnz( Apr[ , i] == 1 )
  })
  
  b1 = apply(Apr, 1, function(x) { match( max(x), x ) }) # [ orig: #   [a1, b1] = max(Apr,[],2); ], a1 unused
  basinMax = table(b1) # [ orig: histc(b1, 1:closedClassIndex); ]
  
  bratioUncertain = max(basinUncertain) / min(basinUncertain)
  bratioCertain   = max(basinCertain)   / min(basinCertain)
  bratioMax       = max(basinMax)       / min(basinMax)
  
  
  #Intersection of (global with local) basins
  gbasin = which( Apr[ , which(indexPermutation==minValueIdx)] !=0 )
  basinIntersectionCount = na.omit(
    sapply(seqClosedClasses, function(i) {
      if ( i == which(indexPermutation==minValueIdx) ) {
        return (NA)
      }
      lbasin = which(Apr[ , i] != 0)
      return ( length( intersect(gbasin, lbasin) ) )
    })
  )
  
  
  minBasinIntersectionCount  = min(basinIntersectionCount)
  meanBasinIntersectionCount = mean(basinIntersectionCount)
  maxBasinIntersectionCount  = max(basinIntersectionCount)
  stdBasinIntersectionCount  = sd(basinIntersectionCount)
  
  #range of basin
  coord = sapply(1:length(gbasin), FUN = function(i) {
    celltoz(gbasin[i], divisions)
  })
  range =  sqrt( sum( # calculate range. suspicious: why don't we (nor the matlab version) care about dimensions?
    (
      max(coord)-   # [ orig: max(coord,[],1) ]
        min(coord))   # [ orig: min(coord,[],1) ]
    ^2 ) )  # [ orig: norm(max(coord,[],1) - min(coord,[],1));]
  
  return( list(
    levels = levels, depth = depth, ratioDepthLevels = ratioDepthLevels, 
    minw = minw, meanw = meanw, maxw = maxw, stdw = stdw,                 # loc/disp of weights
    ratioLevelsLeaves = ratioLevelsLeaves,                                # ratio levels/leaves
    minDistance = minDistance, meanDistance = meanDistance,                # distances global -> local optima
    maxDistance = maxDistance, stdDistance = stdDistance,
    bratioUncertain = bratioUncertain, bratioCertain = bratioCertain, bratioMax = bratioMax, # basins
    minBasinIntersectionCount = minBasinIntersectionCount,                # loc/disp of 
    meanBasinIntersectionCount = meanBasinIntersectionCount,              # intersection of basins
    maxBasinIntersectionCount = maxBasinIntersectionCount,
    stdBasinIntersectionCount = stdBasinIntersectionCount,    
    range = range                                                         # range of basin
  ))
}