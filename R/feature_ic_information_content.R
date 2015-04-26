# @title Calculate information content-based features for a given function
# 
# @description
# Computes features based on the Information Content of Fitness 
# Sequences (ICoFiS) approach.
# 
# In this approach, the information content of a
# continuous landscape, i.e. smoothness, ruggedness, or neutrality, are
# quantified. While common analysis methods were able to calculate the information
# content of discrete landscapes, the ICoFiS approach provides an adaptation to 
# continuous landscapes that accounts e.g. for variable step sizes in random walk
# sampling.
# 
# @param feat.object [\code{\link{FeatureObject}}]\cr
#   A feature object as created by \code{\link{createFeatureObject}}.
#   Note that the object needs to contain the number of blocks per dimension
#   (parameter \code{blocks}) in order to allow cellmapping features to compute.
# @param control [\code{\link{list}}]\cr
#   A list object that stores additional configuration parameters. Here, the 
#   following parameters are used:\cr
#   \code{ic.epsilon}: List of epsilon values to be used. Needs to contain \code{epsilon[1] = 0}.\cr
#   \code{ic.sorting}: Either "R" for random sorting or "NN" for nearest neighbour sorting. Defaults to "NN".\cr
#   \code{ic.calculate_partial}: Boolean value whether to calculate partial information measures.\cr
#   \code{ic.generate_sample}: Boolean value whether this function should generate its own sample data using a latin hypercube design strategy. If set to \code{TRUE}, the following three parameters apply.\cr
#   \code{ic.generate_sample.dimensions}: Number of feature dimensions.\cr
#   \code{ic.generate_sample.min}: Lower bound for sampled values.\cr
#   \code{ic.generate_sample.max}: Upper bound for sampled values.\cr
#   \code{ic.plot}: Boolean flag whether the measures and values of H and M should be plotted.
#   
#   Note that for the sampling parameters \code{ic.generate_sample.min} and
#   \code{ic.generate_sample.max} passing vectors  is not supported, since this
#   will yield unexpected results. If different min/max values per dimension are required, 
#   please calculate the data sample yourself before invoking this function.
# @return [\code{\link{list}(4)} of \code{\link{numeric}(1)}].
#   List of features.\cr
#   For further information, see details.
# @details
#   The first feature, \code{ic.Hmax}, describes the maximum information content
#   (entropy) of the fitness sequence. The second, \code{ic.epsilonS} (\dQuote{settling
#   sensitivity}), indicates that epsilon for which the sequence nearly consists
#   of zeros only. 
#   The two remaining features are only computed if the control parameter
#   \code{ic.calculate_partial} is set to TRUE:
#   \code{ic.Mzero} describes the maximum number of inflection points,
#   which are reduced to half using an epsilon of \code{ic.epsilon05}.
# @references
# See Munoz et al. (2015), \dQuote{Exploratory Landscape Analysis of Continuous Space
# Optimization Problems Using Information Content}, in IEEE Transactions
# on Evolutionary Computation (19:1), pp. 74-87, (\url{http://dx.doi.org/10.1109/TEVC.2014.2302006}).
# @examples
# # (1) create the initial design:
# X = t(replicate(1000, runif(2, -10, 10)))
# y = apply(X, 1, function(x) sum(x^2))
# feat.object = createFeatureObject(X = X, y = y)
# # (2) compute the ICoFiS features:
# calculateInformationContent(feat.object = feat.object)
# @export 
calculateInformationContent = function(feat.object, control = list()) {
  assertClass(feat.object, "FeatureObject")
  assertList(control)
  # epsilon values
  epsilon = control_parameter(control, "ic.epsilon", c(
    0, 10^(seq(-5,15,length.out=1000)) # as described in section V-A
    ))
  assertNumeric(epsilon, .var.name="control$ic.epsilon")
  if (epsilon[1] != 0) {
    stop("The first component of ic.epsilon needs to be 0. Please add this component.")
  }
  
  # sorting strategy, either "NN" or "R" (motivation of default value: see discussion chapter)
  sorting = control_parameter(control, "ic.sorting", "NN")
  assertChoice(sorting, c("NN", "R"), .var.name="control$ic.sorting")
  
  # calculation of partial information content M (default value: see discussion chapter)
  calculate.partial.ic = control_parameter(control, "ic.calculate_partial", FALSE)
  
  generate.sample = control_parameter(control, "ic.generate_sample", FALSE)
  
  if (generate.sample) { # control parameters that are only valid if samples are to be generated
    if (!testFunction(feat.object$fun, args=NULL, ordered=FALSE)) {
      stop("For generating the sample using LHD you need to specify a function in feat.object.")
    }
    # size of sample (according to upper bound, as discussed on p. 78)
    generate.sample.size = control_parameter(control, "ic.generate_sample.size", 1000)
    assertInt(generate.sample.size, .var.name="control$ic.generate_sample.size")
    
    # input dimensions (default: no. of cols in feat.object)
    generate.sample.dimensions = control_parameter(control, "ic.generate_sample.dimensions", feat.object$dim)
    assertInt(generate.sample.dimensions, .var.name="control$ic.generate_sample.dimensions")
    
    # minimum and maximum possible value of generated samples
    # (Needs to be identical for every dimension! Passing vectors leads to unexpected results.
    # If individual ranges per dimension are required, please generate the sample yourself.)
    generate.sample.min = control_parameter(control, "ic.generate_sample.min", .Machine$double.xmin)
    generate.sample.max = control_parameter(control, "ic.generate_sample.max", .Machine$double.xmax)
    assertNumeric(generate.sample.min, .var.name="control$ic.generate_sample.min")
    assertNumeric(generate.sample.max, .var.name="control$ic.generate_sample.max")
  }
  debug.plot = control_parameter(control, "ic.plot", FALSE)
  # all params set; ready to go.
  
  # default values for those features that rely on partial IC (M)
  Mzero = NA
  epsilon05 = NA
  
  # custom samples
  if (generate.sample) {
    # LHD samples in [0;1]
    X = lhs::randomLHS(generate.sample.size, generate.sample.dimensions)
    # convert random numbers in [0; 1] to numbers in [min; max],
    # assuming uniform distribution and equal range for all dimensions
    X = qunif(X, min = generate.sample.min, max = generate.sample.max)
    y = apply(X, 1, feat.object$fun)
  }
  else {
    X = extractFeatures(feat.object)
    y = extractObjective(feat.object)
  }

  # sort values ( NN / R )
  if (sorting == "R") {
    permutation = sample.int( nrow(X) )
  } else if (sorting == "NN") {
    permutation = ic_sort_nearest_neighbour(X)
  }
  
  # calculate psi [Eq. (9)], i.e. symbol sequence with elements in {-1, 0, 1}
  # for an input data set with n entries, n-1 psis will be generated
  psi.indices = seq_len( length(permutation) )
  
  HM = sapply(epsilon, FUN= function(e) {
    psi.epsilon = mapply( ic_symbol_sequence, 
      permutation[ psi.indices[-length(psi.indices)] ], # i,
      permutation[ psi.indices[-1] ],  #j,
      MoreArgs = list(epsilon = e, X=X, y=y)
    )
    
    # calculate probabilities of finding blocks ab, a != b, in sequence psi
    # results in n-2 different blocks, thus excluding the last two / the last one element[s] from psi
    blocks = mapply( ic_identify_block,
      psi.epsilon[ psi.indices[-c(length(psi.indices), length(psi.indices)-1)] ], #x
      psi.epsilon[ psi.indices[-c(1,                   length(psi.indices)  )] ]  #y
    )
    # convert absolute block frequencies...
    probability = table(blocks)
    # ...into relative ones
    probability = probability / sum(probability)
    
    terms = c(
      ifelse(is.na(probability["neg.neu"]), 0, 
        probability["neg.neu"] * log( probability["neg.neu"], 6) ),
      ifelse(is.na(probability["neg.pos"]), 0, 
        probability["neg.pos"] * log( probability["neg.pos"], 6) ),
      ifelse(is.na(probability["neu.neg"]), 0, 
        probability["neu.neg"] * log( probability["neu.neg"], 6) ),
      ifelse(is.na(probability["neu.pos"]), 0, 
        probability["neu.pos"] * log( probability["neu.pos"], 6) ),
      ifelse(is.na(probability["pos.neg"]), 0, 
        probability["pos.neg"] * log( probability["pos.neg"], 6) ),
      ifelse(is.na(probability["pos.neu"]), 0, 
        probability["pos.neu"] * log( probability["pos.neu"], 6) )
    )
    
    # calculate H for epsilon [Eq. (2)] (=> overall result: all H)
    H.epsilon = - sum(terms)
    
    M.epsilon = NA
    if (calculate.partial.ic) {
      # calculate phi' derived from psi
      # (actually, only its length is used lateron. Therefore, calculate the lengths of phi')

      #skip zeroes and repeated values
      last.value = 0
      changes.length = 0
      for (value in psi.epsilon) {
        if (value == 0 || value == last.value) 
          next
        
        changes.length = changes.length + 1
        last.value     = value
      }
      
      # calculate M [Eq. (3)]
      M.epsilon = changes.length / (length(permutation) - 1)
    }
    
    return( c(H.epsilon, M.epsilon) )
  })
  H = HM[1, ] # columns correspond to epsilons
  M = HM[2, ]

  # calculate Hmax [Eq. (5)] ("maximum information content")
  Hmax = max(H)
  
  # calculate epsilonS [Eq. (6)] ("settling sensitivity")
  epsilonS = log10( epsilon[ min( which(H < 0.05) ) ] )
  
  if (calculate.partial.ic) {
    # calculate Mzero [Eq. (7)] ("initial partial information")
    Mzero = M[1]
    
    # calculate epsilon05 [Eq. (8)] ("half partial information sensitivity")
    elementsGreaterHalfMzero = which(M > 0.5*Mzero)
    if (length(elementsGreaterHalfMzero) > 0) {
      epsilon05 = log10( epsilon[ max( elementsGreaterHalfMzero ) ] )
    } else {
      epsilon05 = -Inf
    }
    
  }
  
  if (debug.plot) {
    plot_ic (epsilon, calculate.partial.ic, H, M, Hmax, epsilonS, Mzero, epsilon05)
  }

  list(
    ic.Hmax = Hmax,           #maximum information content
    ic.epsilonS = epsilonS,   #settling sensitivity
    ic.Mzero = Mzero,         #initial partial information
    ic.epsilon05 = epsilon05  #half partial information sensitivity
  )
}

# Calculate psi for a certain i and epsilon [Eq. (9)]
# j , i.e. the next element in the sequence, is also passed since a
# permutation of values has taken place before.
# Therefore, j is probably not equal to i+1.
ic_symbol_sequence = function(i, j, epsilon, X, y) {
  difference = (y[j] - y[i]) / 
    sqrt(  sum( (X[j, ] - X[i, ])^2 )  )
  
  if (difference < -epsilon) {
    return (-1)
  } else if (difference > epsilon) {
    return (1)
  } else {
    return (0)
  }
}

ic_identify_block = function(x, y) {
  # switch only allows values > 1, therefore add +2 to gain symbols in {1, 2, 3} (original order)
  return(
    switch(x+2,
      # x neg
      switch(y+2,
        # y neg
        "neg.neg",
        # y neutral,
        "neg.neu",
        # y pos
        "neg.pos"),
      # x neutral,
      switch(y+2,
        # y neg
        "neu.neg",
        # y neutral,
        "neu.neu",
        # y pos
        "neu.pos"),
      # x pos
      switch(y+2,
        # y neg
        "pos.neg",
        # y neutral,
        "pos.neu",
        # y pos
        "pos.pos"),
      )
  )
}

ic_sort_nearest_neighbour = function(X) {
  distances = as.matrix(dist(X))
  
  # add first candidate (random) and initialise permutation vector (avoids continuous allocation of space)
  first = sample.int( nrow(X), 1 )
  candidates = seq_len( nrow(X) )[-first]
  permutation = c(first, rep(-1, nrow(X)-1))
  
  # successively add next candidates
  for (i in 2 : (length(permutation)-1) ) {
    # invalidate those that are already in permutation
    distances[ permutation[i-1], -candidates ] = NA 
    # then select the neighbour with min. distance (if tie: break deterministically by using fist)
    permutation[i] = which.min( distances[ permutation[i-1],  ] )
    # remove from future candidates
    candidates = candidates[ -which(candidates == permutation[i]) ]
  }
  # add last candidate
  permutation[length(permutation)] = as.numeric(candidates)
  return (permutation)
}
