#' @title Calculate Curvature Features
#' @description
#' Computes features, which quantify the curvature of a function.
#' @param feat.object [\code{\link{FeatureObject}}]\cr
#' A feature object as created by \link{createFeatureObject}.\cr
#' Note, that the feature object has to contain the function itself in
#' order to compute the convexity features.
#' @param control [\code{\link{list}}]\cr
#' A list object that stores additional configuration parameters:\cr
#' The element \code{curv.sample_size} defines the number of samples that are
#' used for calculating the features. The default is \code{1000}.
#' @details
#' Given a feature object, \code{curv.sample_size} (per default
#' \code{100 * d} with \code{d} being the number of features) samples are
#' randomly chosen. Then, the gradient and hessian of the function are 
#' estimated based on those points and the following features are computed:\cr
#' 
#' The first seven features (\code{curv.grad_norm}) aggregate the length of
#' the gradients, the next seven features (\code{curv.grad_scale}) summarize
#' the ratio of biggest and smallest (absolute) gradient direction and the
#' remaining seven features (\code{curv.hessian_cond}) summarize the ratio of
#' biggest and smallest eigenvalue of the hessian matrices. The aggregation
#' is always done in the same order:\cr
#' (1) minimum\cr
#' (2) lower quartile (also known as 1st quartile or \code{25\%}-quantile)\cr
#' (3) arithmetic mean\cr
#' (4) median\cr
#' (5) upper quartile (also known as 3rd quartile or \code{75\%}-quantile)\cr
#' (6) maximum\cr
#' (7) standard deviation\cr
#'   
#' The final two features show the amount of (additional) function
#' evaluations and running time (in seconds) that were needed for the
#' computation of these features.
#' @return [\code{\link{list}(23)} of \code{\link{numeric}(1)}].\cr
#' List of features.\cr
#' For further information, see details.
#' @references
#' See Mersmann et al. (2011), \dQuote{Exploratory Landscape Analysis} 
#' (\url{http://dx.doi.org/10.1145/2001576.2001690}).
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#' 
#' # (2) compute the curvature features:
#' calculateCurvature(feat.object)
#' @export 
calculateCurvature = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  f = initializeCounter(feat.object$fun)
  if (is.null(f))
    stop("The curvature features require the exact function!")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    X = extractFeatures(feat.object)
    d = feat.object$dim
    N = control_parameter(control, "curv.sample_size", 100L * d)
    calcNumDeriv = function(par) {
      gr = numDeriv::grad(f, par)
      hess = numDeriv::hessian(f, par)
      eig = abs(eigen(hess)$values)
      c(curv.grad_norm = sqrt(sum(gr^2)),
        curv.grad_scale = max(abs(gr)) / min(abs(gr)),
        curv.hessian_cond = max(eig) / min(eig))
    }
    ids = sample(feat.object$n.obs, N, replace = FALSE)
    res = apply(X[ids, ], 1, calcNumDeriv)
    fn = apply(res, 1, function(x) {
      z = fivenum(x)
      return(c(z[1:2], mean(x), z[3:5], sd(x)))
    })
    fn = as.vector(fn, mode = "list")
    nn = c("min", "lq", "mean", "med", "uq", "max", "sd")
    names(fn) = paste(rep(rownames(res), each = length(nn)), nn, sep = ".")
    fn
    return(c(fn, curv.costs_fun_evals = showEvals(f)))
  }), "curv")
}

