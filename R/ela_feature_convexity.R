#' @title Calculate Convexity Features
#' @description
#' Computes features, which estimate the convexity of a function.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.\cr
#' Note, that the feature object has to contain the function itself in
#' order to compute the convexity features.
#' @param control [\code{\link{list}}]\cr
#' A \code{list} storing additional configuration parameters:\cr
#' The element \code{convex.nsample} defines the number of samples that
#' are drawn for the calculation of the feature. The default is \code{1000}.\cr
#' \code{convex.threshold} defines the threshold of the linearity, i.e.,
#' the tolerance (or deviation) from perfect linearity is allowed in order 
#' to still be considered as linear. Here, the default is \code{1e-10}.
#' @return [\code{\link{list}(3)} of \code{\link{numeric}(1)}].\cr
#' List of features.\cr
#' For further information, see details.
#' @details
#' Two observations are chosen randomly from the initial design. Then, a linear
#' (convex) combination of those observations is calculated -- based on a
#' random weight from [0, 1]. The corresponding function value will be compared
#' to the linear combination of the objectives from the two original
#' observations. This process is replicated \code{convex.nsample} (per
#' default \code{1000}) times and will then be aggregated.\cr
#' 
#' The resulting features are:\cr
#' (1) \code{convex.convex_p} shows the percentage of convexity\cr
#' (2) \code{convex.linear_p} shows the percentage of linearity\cr
#' (3) \code{convex.linear_dev} returns the average deviation between the
#' linear combination of objectives and the objective of the linear combination
#' of the observations
#' 
#' \bold{Note}:\cr
#' These calculations cause \code{convex.nsample} additional function
#' evaluations.
#' @references
#' See Mersmann et al. (2011), \dQuote{Exploratory Landscape Analysis} 
#' (\url{http://dx.doi.org/10.1145/2001576.2001690}).
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#' 
#' # (2) compute the convexity features:
#' calculateConvexity(feat.object)
#' @export 
calculateConvexity = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  f = feat.object$fun
  if (is.null(f))
    stop("The local search features require the exact function!")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  calcDistance = function(n) {
    i = sample(n, 2L)
    wt = runif(1)
    wt = c(wt, 1 - wt)
    ## Linear compbination of X[i[1],] and X[i[2],]
    xn = drop(wt %*% X[i, ])
    ## Distance between xn and linear combination of y[i[1]] and y[i[2]]
    drop(f(xn) - y[i] %*% wt)
  }
  n = feat.object$n.obs
  N = control_parameter(control, "convex.nsamp", 1000L)
  eps = control_parameter(control, "convex.threshold", 1e-10)
  delta = replicate(N, calcDistance(n))
  list(conv.conv_prob = mean(delta < -eps),
    conv.lin_prob = mean(abs(delta) <= eps),
    conv.lin_dev = mean(delta))
}
