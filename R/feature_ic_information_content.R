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
# calculateInformationContentFeatures(feat.object = feat.object)
# @export
calculateInformationContentFeatures = function(feat.object, control = list()) {
  assertClass(feat.object, "FeatureObject")
  
  assertList(control)
  measureTime(expression({
    # epsilon values, as described in section V.A
    epsilon = control_parameter(control, "ic.epsilon", 
      c(0, 10^(seq(-5, 15, length.out = 1000)))
    )
    assertNumeric(epsilon, .var.name="control$ic.epsilon", lower = 0)
    if (all(epsilon != 0)) {
      stop("One component of ic.epsilon has to be 0. Please add this component.")
    }
    epsilon = unique(epsilon)
    
    ## sorting strategy, either "nn" (= default) or "random"
    ## (motivation of default value: see discussion chapter)
    sorting = control_parameter(control, "ic.sorting", "nn")
    assertChoice(sorting, c("nn", "random"), .var.name="control$ic.sorting")
    
    generate.sample = control_parameter(control, "ic.generate_sample", FALSE)
    assertLogical(generate.sample, len = 1L)
    
    if (generate.sample) { 
      # control parameters, which are only required, if the sample needs to be generated
      if (!testFunction(feat.object$fun, args = NULL, ordered = FALSE)) {
        stop("For generating a sample using a LHD, you need to specify a function in feat.object.")
      }
      
      # size of sample (according to upper bound, as discussed on p. 78)
      sample.size = control_parameter(control, "ic.generate_sample.size", 100L * feat.object$dim)
      assertInt(sample.size, .var.name="control$ic.generate_sample.size")
      
      # input dimensions (default: no. of cols in feat.object)
      sample.dimensions = control_parameter(control, "ic.generate_sample.dimensions", feat.object$dim)
      assertInt(sample.dimensions, .var.name="control$ic.generate_sample.dimensions")
      
      # minimum and maximum possible value of generated samples
      # (Needs to be identical for every dimension! Passing vectors leads to unexpected results.
      # If individual ranges per dimension are required, please generate the sample yourself.)
      sample.lower = control_parameter(control, "ic.generate_sample.lower", feat.object$lower)
      sample.upper = control_parameter(control, "ic.generate_sample.upper", feat.object$upper)
      assertNumeric(sample.lower, .var.name = "control$ic.generate_sample.lower")
      assertNumeric(sample.upper, .var.name = "control$ic.generate_sample.upper")
      
      X = initializeLHD(points = sample.size, dims = sample.dimensions, 
        lower = sample.lower, upper = sample.upper)
      y = apply(X, 1, feat.object$fun)
      n = nrow(X)
    } else {
      X = extractFeatures(feat.object)
      y = extractObjective(feat.object)
      n = feat.object$n.obs
    }
    
    # sort values (nearest neighbours vs. random)
    if (sorting == "random") {
      permutation = sample.int(n)
      d = computeDistances(X = X, permutation = permutation)
    } else if (sorting == "nn") {
      start = control_parameter(control, "ic.nn.start", sample.int(n, 1))
      assertInt(start, .var.name="control$ic.nn.start", lower = 1L, upper = n)
      res = constructSequence(X = X, start = start)
      permutation = res[,1]
      d = res[-1, 2]
    }
    
    psi.eps = vapply(epsilon, function(eps) {
      computePsi(permutation = permutation, xdists = d, y = y, eps = eps)  
    }, integer(length(permutation) - 1L))
    
    H.eps = apply(psi.eps, 2, computeH)
    M.eps = apply(psi.eps, 2, computeM)
    
    # calculate H.max, cf. equation(5) ("maximum information content")
    H.max = max(H.eps)
    
    # calculate eps.S, cf. equation (6) ("settling sensitivity")
    settl.sens = control_parameter(control, "ic.settling_sensitivity", 0.05)
    assertNumeric(settl.sens, .var.name = "control$ic.settling_sensitivity",
      lower = 0, upper = .Machine$double.xmax)
    eps.S = epsilon[which(H.eps < settl.sens)]
    if (length(eps.S) > 0) {
      eps.S = log10(min(eps.S))
    } else {
      eps.S = NA_real_
    }
    
    # calculate M0, cf. equation (7) ("initial partial information")
    M0 = M.eps[epsilon == 0]
    
    # calculate epsilon05 [Eq. (8)] ("half partial information sensitivity")
    inf.sens = control_parameter(control, "ic.information_sensitivity", 0.5)
    assertNumeric(inf.sens, .var.name = "control$ic.information_sensitivity",
      lower = -1, upper = 1)
    
    eps05 = which(M.eps > inf.sens * M0)
    if (length(eps05) > 0) {
      eps05 = log10(max(epsilon[eps05]))
    } else {
      eps05 = NA_real_
    }
    
    debug.plot = control_parameter(control, "ic.plot", FALSE)
    assertLogical(debug.plot, len = 1L)
    
    if (debug.plot) {
      plot_ic(epsilon, TRUE, H.eps, M.eps, H.max, eps.S, M0, eps05)
    }
    
    return(list(ic.max_info_cont = H.max,
      ic.settl_sens = eps.S,
      ic.init_part_info = M0,
      ic.half_part_info_sens = eps05
    ))
  }), "ic")
}
