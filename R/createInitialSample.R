#' @title Create Initial Sample
#'
#' @description
#'   Convenient helper function, which creates an initial sample - either based
#'   on random (uniform) sampling or using latin hypercube sampling.
#'
#' @param n.obs [\code{integer(1)}]\cr
#'   Number of observations.
#' @param dim [\code{integer(1)}]\cr
#'   Number of dimensions.
#' @param control [\code{list}]\cr
#'   Control argument. For further information refer to the details.
#'
#' @return [\code{\link{matrix}}].\cr
#'   A matrix, consisting of \code{n.obs} rows of \code{dim}-dimensional
#'   observations.
#'
#' @details
#'   Per default, this function will produce \code{n.obs} observations of size
#'   \code{dim} in the range from 0 to 1. If you want to create a more specific
#'   initial sample, the following control arguments might be helpful:
#'   \itemize{
#'     \item{\code{init_sample.type}}: Should the initial sample be created
#'     based on random uniform sampling (\code{"random"}) or on a latin hypercube
#'     sample (\code{"lhs"})? The default is \code{"random"}.
#'     \item{\code{init_sample.lower}}: The lower bounds of the initial sample.
#'     Either a vector of size \code{dim} or a scalar (if all lower bounds are
#'     identical). The default is \code{0}.
#'     \item{\code{init_sample.upper}}: The upper bounds of the initial sample.
#'     Either a vector of size \code{dim} or a scalar (if all upper bounds are
#'     identical). The default is \code{1}.
#'   }
#'
#' @examples
#' # (1) create a simple initial sample:
#' X = createInitialSample(300, 5)
#' summary(X)
#' 
#' # (2) create a more specific initial sample:
#' ctrl = list(init_sample.type = "lhs",
#'   init_sample.lower = c(-5, 2, 0),
#'   init_sample.upper = 10)
#' X = createInitialSample(200, 3, control = ctrl)
#' summary(X)
#'
#' @export
createInitialSample = function(n.obs, dim, control) {
  assertInt(n.obs, lower = 1L)
  assertInt(dim, lower = 1L)
  if (missing(control))
    control = list()
  assertList(control)
  init.type = control_parameter(control, "init_sample.type", "random")
  assertChoice(init.type, choices = c("random", "lhs"))
  lower = control_parameter(control, "init_sample.lower", 0)
  upper = control_parameter(control, "init_sample.upper", 1)
  assertNumeric(lower, min.len = 1L, max.len = dim)
  assertNumeric(upper, min.len = 1L, max.len = dim)
  if (!(length(lower) %in% c(1, dim)))
    stop("The length of the lower bound (%i) does not fit to the desired dimension (%i).", length(lower), dim)
  if ((length(lower) == 1) & (dim != 1)) {
    lower = rep(lower, dim)
  }
  if (!(length(upper) %in% c(1, dim)))
    stop("The length of the upper bound (%i) does not fit to the desired dimension (%i).", length(upper), dim)
  if ((length(upper) == 1) & (dim != 1)) {
    upper = rep(upper, dim)
  }
  if (any(lower > upper))
    stop("The lower bounds need to be below the upper bounds!")
  if (init.type == "random") {
    X = matrix(runif(dim * n.obs), ncol = dim, nrow = n.obs)
  } else if (init.type == "lhs") {
    X = lhs::improvedLHS(n = n.obs, k = dim)
  }
  X = vapply(seq_len(dim), function(i) {
    x = X[,i]
    x * (upper[i] - lower[i]) + lower[i]
  }, double(n.obs))
  X = addClasses(X, "initial.sample")
  attr(X, "lower") = lower
  attr(X, "upper") = upper
  return(X)
}
