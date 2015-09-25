calculateDistributionFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    y = extractObjective(feat.object)
    smoothing.bw = control_parameter(control, "ela_distr.smoothing_bandwidth", "SJ")
    smoothing.modemass_threshold = control_parameter(control, 
      "ela_distr.modemass_threshold", 0.01)
    skewness.type = control_parameter(control, "ela_distr.skewness_type", 3L)
    kurtosis.type = control_parameter(control, "ela_distr.kurtosis_type", 3L)
    list(ela_distr.skewness = e1071::skewness(y, type = skewness.type),
      ela_distr.kurtosis = e1071::kurtosis(y, type = kurtosis.type),
      ela_distr.number_of_peaks = number_of_peaks(y, 
        smoothing.bandwidth = smoothing.bw, 
        modemass.threshold = smoothing.modemass_threshold, ...))
  }), "ela_distr")
}

# estimate the number of peaks
# - min.index is the position of valleys within the estimated y-distribution
# - modemass is the mass, which is represented by a potential peak
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
