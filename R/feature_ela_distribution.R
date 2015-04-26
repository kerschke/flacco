# @title Calculates features of the distribution of a function's objective space
# 
# @description
# Computes features based on the objective space of a function.
# 
# @param feat.object [\code{FeatureObject}]\cr
#   A feature object as created by createFeatureObject.
# @param control [\code{list}]\cr
#   A list object that stores additional configuration parameters.
#   Here, the parameter distr.smoothing_bandwidth defines the smoothing 
#   bandwidth, which should be used within the density estimation (cf. 
#   \code{density}). The default is "SJ". In addition to the previous 
#   parameter, distr.modemass_threshold defines the threshold that is used in
#   order to classify whether a minimum can be considered as a peak. Per 
#   default this value is 0.01.
#   Also, the formula, which is used for the calculation of the skewness and
#   kurtosis can be defined using distr.skewness_type and distr.kurtosis_type.
#   The default of both methods is 3, which is also the default in R.
# @param ... [any]\cr
#   Further arguments for the computation within \code{density}.
# @return [\code{list(5)} of \code{numeric(1)}].\cr
#   List of features.\cr
#   The first two features return the skewness and kurtosis of the objective
#   values. The next feature estimates the density of the objective
#   values and analyzes them w.r.t. a certain threshold.\cr
#   
#   The final two features show the amount of (additional) function
#   evaluations and running time (in seconds) that were needed for the
#   computation of these features.
# @examples
#   # (1) draw a random sample; here it consists of 2000 observations with
#   # each one of them being 5-dimensional within -10 and 10:
#   X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#   # (2) create a feature object
#   feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#   # (3) compute the y-distribution-features
#   calculateDistributionFeatures(feat.object)
# @export 
calculateDistributionFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    y = extractObjective(feat.object)
    smoothing.bw = control_parameter(control, "distr.smoothing_bandwidth", "SJ")
    smoothing.modemass_threshold = control_parameter(control, 
      "distr.modemass_threshold", 0.01)
    skewness.type = control_parameter(control, "distr.skewness_type", 3L)
    kurtosis.type = control_parameter(control, "distr.kurtosis_type", 3L)
    list(y_dist.skewness = e1071::skewness(y, type = skewness.type),
      y_dist.kurtosis = e1071::kurtosis(y, type = kurtosis.type),
      y_dist.number_of_peaks = number_of_peaks(y, 
        smoothing.bandwidth = smoothing.bw, 
        modemass.threshold = smoothing.modemass_threshold, ...))
  }), "y_dist")
}

# Estimate the number of peaks
# min.index is the position of valleys within the estimated y-distribution
# modemass is the mass, which is represented by a potential peak
number_of_peaks = function(x, smoothing.bandwidth = "SJ", 
  modemass.threshold = 0.01, ...) {
  intdens = function(a, b) {
    mean(y[a:b]) * diff(d$x[c(a, b)])
  }
  d = density(x, bw = smoothing.bandwidth, ...)
  y = d$y
  n = length(y)
  index = 2L : (n - 1L)
  min.index = c(1L, which((y[index] < y[index - 1L]) & y[index] < y[index + 1L]), n + 1L)
  modemass = vapply(1L : (length(min.index) - 1L), function(i) 
    intdens(min.index[i], min.index[i + 1L] - 1L), double(1))
  sum(modemass > modemass.threshold)
}
