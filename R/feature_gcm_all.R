# @title Calculate GCM features for a given function
# 
# @description
# Based on Generalized Cell Mapping (GCM) techniques, the cell mappings are interpreted as
# an absorbing Markov chain, from which 24 features are derived.
# 
# Provided the decision space is divided into a grid of cells, GCM incorporates
# the dynamics of a system into the cell mapping. For each cell, the probabilities of transitioning
# to successor cells are calculated, where each cell can have multiple successor cells. 
#  
# Computation is performed based on \code{min}, \code{mean}, and \code{near} approaches for finding a representative
# objective function value for each cell.
# 
# @param feat.object [\code{\link{FeatureObject}}]\cr
#   A feature object as created by \code{\link{createFeatureObject}}.
#   Note that the object needs to contain the number of blocks per dimension
#   (parameter \code{blocks}) in order to allow cellmapping features to compute.
# @param control [\code{\link{list}}]\cr
#   A list object that stores additional configuration parameters.
#   The following parameters are used here:\cr
#   \code{gcm.plot} is a boolean, indicating whether GCM results are visualised in a plot.\cr
#   \code{gcm.plot.colors} is a vector defining the first cell colours to be used by the plot.
#      Note that the first colour is used for cells that belong to multiple attractors,
#      the second colour is used for attractors, 
#      and the remaining ones are assigned to cells with distinct single attractors.
# @return [\code{\link{list}(24)} of \code{\link{numeric}(1)}].\cr
#   List of features.\cr
#   For further information, see details.
# @details
#   For the \code{min}, \code{mean}, and \code{near} approaches, \code{calculateGCMFeatures} 
#   calculates seven features each. Furthermore, three features summarise the 
#   findings of the \code{min} and \code{mean} approaches in order to study similarities and 
#   differences between both.\cr
#   \code{gcm.{min, mean, near}.uncert_ratio}: Ratio of uncertain cells (for min and mean approaches),\cr
#   \code{gcm.{min, mean, near}.prob_best}: Probability to find the best cell,\cr
#   \code{gcm.{min, mean, near}.{min, max, mean, std}_bs}: Aggregations of the different basin sizes,\cr
#   \code{gcm.{min, mean, near}.no_attr}: Number of attractors within the grid,\cr
#   \code{gcm.common.pcells}: Number of periodic cells common between min and mean approaches,\cr
#   \code{gcm.common.tcells}: Number of transient cells common between min and mean approaches, and\cr
#   \code{gcm.common.dcells}: Percentage of cells that change their roles from one approach to the other.
# @references
#  See Kerschke et al. (2014), \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis}, 
#  in EVOLVE-A Bridge between Probability, Set Oriented Numerics, and Evolutionary Computation V,
#  pp. 115-131 (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
# @examples
# # (1) create the initial design:
# X = t(replicate(2000, runif(2, -10, 10)))
# y = apply(X, 1, function(x) sum(x^2))
# feat.object = createFeatureObject(X = X, y = y, 
#   lower = -10, upper = 10, blocks = 10)
# # (2) compute the Generalized Cell Mapping features:
# calculateGCMFeatures(feat.object = feat.object, control = list(gcm.plot=TRUE))
# @export 
calculateGCMFeatures = function (feat.object, control = list()) {
  assertClass(feat.object, "FeatureObject")
  assertList(control)
  # [ for gcm: x is var, y is fun ]
  if (!feat.object$allows.cellmapping) {
    stop( paste(c("This feature object does not support cell mapping. You need to ",
          "create a feature object with the parameter 'blocks', defining the number ",
          "of divisons per dimension.")) )
  }
  
  # Visualisation options
  plot.gcm = control_parameter(control, "gcm.plot", FALSE)
  plot.gcm.colors = control_parameter(control, "gcm.plot.colors", NULL)
  
  # ensure that canonical form and representatives per cell are initialised (for min, mean, and near)
  gcm_init(feat.object)
  
  #Evaluate GCM features for Min
  features.min = evaluateGCM(
                      feat.object$env$gcm.representatives$min, 
                      feat.object$env$gcm.canonicalForm$min,
                      feat.object,
                      plot.gcm, plot.gcm.colors)
  
  #Evaluate GCM features for Mean
  features.mean = evaluateGCM(
                      feat.object$env$gcm.representatives$mean, 
                      feat.object$env$gcm.canonicalForm$mean,
                      feat.object,
                      plot.gcm, plot.gcm.colors)
  
  #Evaluate GCM features for Near
  features.near = evaluateGCM(
                      feat.object$env$gcm.representatives$near, 
                      feat.object$env$gcm.canonicalForm$near,
                      feat.object,
                      plot.gcm, plot.gcm.colors)
  
  
  # Evaluate overall features (min + mean)
  common_p = length(intersect(features.min$pcell, features.mean$pcell))
  common_t = length(intersect(features.min$tcell, features.mean$tcell))
  
  # Truncate non-feature results
  features.min$pcell = features.mean$pcell = features.near$pcell = 
    features.min$tcell = features.mean$tcell = features.near$tcell =
    features.min$diffcell = features.mean$diffcell = features.near$diffcell = NULL
  
  return (list(
    gcm.min.no_attr = features.min$no_attr, # number of attractors
    gcm.min.uncert_ratio = features.min$uncert_ratio, # uncertainty ratio
    gcm.min.prob_best = features.min$prob_best, # probability to find the best
    gcm.min.std_bs = features.min$std_bs, # loc / disp of basin sizes
    gcm.min.min_bs = features.min$min_bs,
    gcm.min.mean_bs = features.min$mean_bs,
    gcm.min.max_bs = features.min$max_bs,
    
    gcm.mean.no_attr = features.mean$no_attr, # number of attractors
    gcm.mean.uncert_ratio = features.mean$uncert_ratio, # uncertainty ratio
    gcm.mean.prob_best = features.mean$prob_best, # probability to find the best
    gcm.mean.std_bs = features.mean$std_bs, # loc / disp of basin sizes
    gcm.mean.min_bs = features.mean$min_bs,
    gcm.mean.mean_bs = features.mean$mean_bs,
    gcm.mean.max_bs = features.mean$max_bs,
    
    gcm.near.no_attr = features.near$no_attr, # number of attractors
    gcm.near.uncert_ratio = features.near$uncert_ratio, # uncertainty ratio
    gcm.near.prob_best = features.near$prob_best, # probability to find the best
    gcm.near.std_bs = features.near$std_bs, # loc / disp of basin sizes
    gcm.near.min_bs = features.near$min_bs,
    gcm.near.mean_bs = features.near$mean_bs,
    gcm.near.max_bs = features.near$max_bs,
    
    gcm.common.pcells = common_p,  # common periodic and transient (and differing) cells, comparing min and mean
    gcm.common.tcells = common_t,
    gcm.common.dcells = 1 - ( (1/prod(feat.object$blocks)) * (common_p + common_t) )
    ))
}

# [orig: function [no_attr, unc_rat, prob_best, pcell, tcell, diffcell, sd_bs, min_bs, mean_bs, max_bs] = evaluate_features(filename, option, range_x, range_y) ]
# Perform GCM Calculation for a Specific Mode (i.e. either min, mean, or near)
evaluateGCM = function(fe, cf, # fe and cf are specific to the mode
                        feat.object, # passed for common parameters
                        plot.gcm, plot.gcm.colors) {
  # rename canform list's components
  canonicalForm    = cf$canonicalForm
  indexPermutation = cf$indexPermutation
  closedClassIndex = cf$closedClassIndex
  
  seqClosedClasses = 1:closedClassIndex # used often -> cached for speedup & smaller memory footprint
  
  #Compute fundamenta matrix of Cf
  
  # Assume the idea is canonicalForm^(infinity). 
  Q = expm::"%^%"(canonicalForm, 64) # [ orig:  Q = Cf^50; ]
  
  #Write the matrix of closedClass columns of Q    
  Fm = as.matrix(Q[ , seqClosedClasses])
  
  #Remove zero rows and cols. first: Rows
  zeroRows = na.omit( # remove all NAs; result: Indices of rows w. only zeroes
    sapply(seq_len( nrow(Fm) ), function (i) { # for each row:
      if (all(Fm[i,] == 0)) i         # return index if all entries in row are 0
      else NA
    } )
  )
  if (length(zeroRows) != 0) {
    Fm = as.matrix(Fm[-zeroRows, ]) # removing zero rows [ orig: Fm( !any(Fm,2), : ) = []; ]
  }
  
  # now: Columns (and recalculate no. of closed classes)
  zeroColumns = na.omit( # remove all NAs; result: Indices of columns w. only zeroes
    sapply(seq_len( ncol(Fm) ), function (i) { # for each column:
      if (all(Fm[,i] == 0)) i         # return index if all entries in column are 0
      else NA
    } )
  )
  
  if (length(zeroColumns) != 0) {
    # calculate new number of closed classes
    rtmp = indexPermutation[seqClosedClasses]
    rtmp = rtmp[-zeroColumns]
    closedClassIndex = length(rtmp) # careful, closedClassIndex now: Number of closed classes after removing zero columns
    seqClosedClasses = seq_len(closedClassIndex)
    
    # actually remove columns now:
    indexPermutation = indexPermutation[-zeroColumns] # remove Zerocols from permutation vector [ orig: %Index ]
    Fm = as.matrix(Fm[ , -zeroColumns]) # also remove Zerocols from fundamental matrix [ orig: Fm( :, !any(Fm,1) ) = [];  %columns ]
  }  
  
  # Fundamental matrix Fm fully calculated!  
  
  #Compute number of attractors
  numberOfAttractors = closedClassIndex
  
  sBoA = c() #TODO what is sBoA? Rename!
  
  #compute number of periodic cells and transient cells
  pcell = indexPermutation[ seqClosedClasses ]
  tcell = indexPermutation[ (closedClassIndex+1) : length(indexPermutation) ]
  diffcell = prod(feat.object$blocks)-length(pcell)-length(tcell)
  
  
  #Compute number of uncertain boxes
  counterx = sum(
    sapply(seq_len( nrow(Fm) ), FUN=function(i) {
      nnz(Fm[i, ]) > 1
    }))
  ratioUncertainBoxes = counterx/prod(feat.object$blocks)
  
  #Compute the probability to find each periodic cell
  for (closed in seqClosedClasses) {
    idx = which(Fm[closed, ] != 0)
    tmp = sum(sapply(idx, FUN=function(i) {
      sum(Fm[ ,i]) / prod(feat.object$blocks);
    }))
    sBoA[closed] = tmp;
  }
  
  hi = calcBasins(Fm)$hi
  if (length(hi) > 2) {
    basinsize = hi[1:(length(hi) - 2)]
    std_bs    = sd(basinsize)
    min_bs    = min(basinsize)
    mean_bs   = mean(basinsize)
    max_bs    = max(basinsize)
  }
  if (length(hi) <= 2) {
    std_bs    = 0
    min_bs    = 0
    mean_bs   = 0
    max_bs    = 0
  }
  
  if (plot.gcm) {
    plotgcm(Fm, indexPermutation, feat.object, plot.gcm.colors)
  }
  
  #Compute the probability to find the best cell found
  bestCellProbability = sBoA[
    min( # adds determinism: In case multiple cells evaluate to the minimum value, choose the first one.
      which(
        fe[ indexPermutation[seqClosedClasses] ] == min( fe[ indexPermutation[seqClosedClasses] ])
      )
    )
    ]
  
  
  return( list(no_attr = numberOfAttractors,        # number of attractors
               uncert_ratio = ratioUncertainBoxes,  # uncertainty ratio
               prob_best = bestCellProbability,     # probability to find the best
               pcell = pcell, tcell = tcell,        # lists of cells (periodic / transient)
               diffcell = diffcell,                 # number of cells in neither list
               std_bs = std_bs,                     # loc / disp of basin sizes
               min_bs = min_bs, 
               mean_bs = mean_bs, 
               max_bs = max_bs
  ) )
}

# [orig: function [hi, uni] = calcBasins(Fm) ]
calcBasins = function(Fm) {
  gm = c() #TODO rename
  gr = 0 #TODO rename
  for (i in seq_len(ncol(Fm) )) {
    ci = c() # TODO rename
    keys = c() # TODO rename
    flag = FALSE #TODO rename
    if ( i == 1 || all(Fm[i,1:(i-1)] == 0) ) {
      flag = TRUE
      gr = gr + 1
    } else {
      keys = which(Fm[i, ] != 0)
    }
    for (j in (ncol(Fm)+1):nrow(Fm)) {
      if (Fm[j, i] != 0) {
        keys2 = which(Fm[j, ] != 0)
        
        columns = seq_len( ncol(Fm) )
        columns = columns[columns != i]
        if ( flag && (any(Fm[j, columns] != 0) ) || 
               (!flag && length(keys2) > length(keys)) || 
               (!flag && length(keys2) == length(keys)  && any(keys2 != keys) ) ) {
          ci = c(ci, ncol(Fm)+1)
        } else {
          ci = c(ci, gr)
        }
      }
    }
    gm = c(gm, ci)
  }
  
  ci = rep(ncol(Fm)+2,  ncol(Fm)) # [ orig: ci = [];  for j=1:size(Fm, 2) ci = [ci; size(Fm,2)+2];  end ]
  
  gm = c(gm, ci)
  
  frequencies = table(gm)
  
  uni = as.numeric(names(frequencies)) # [ orig: unique(gm) ]
  hi = as.vector(frequencies) # [ orig: hi = histc(gm, uni) ]
  return( list( hi=hi, uni=uni ) )
}