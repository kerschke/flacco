#' @title Create a Feature Object
#'
#' @description
#'   Create a \code{\link{FeatureObject}}, which will be used as input for all
#'   the feature computations.
#'
#' @param init [\code{\link{data.frame}}]\cr
#'   A \code{data.frame}, which can be used as initial design. If not provided,
#'   it will be created either based on the initial sample \code{X} and the
#'   objective values \code{y} or \code{X} and the function definition \code{fun}.
#' @param X [\code{\link{data.frame}} or \code{\link{matrix}}]\cr
#'   A \code{data.frame} or \code{matrix} containing the initial sample. If not
#'   provided, it will be extracted from \code{init}.
#' @param y [\code{\link{numeric}} or \code{\link{integer}}]\cr
#'   A vector containing the objective values of the initial design.
#'   If not provided, it will be extracted from \code{init}.
#' @param minimize [\code{\link{logical}(1)}]\cr
#'   Should the objective function be minimized? The default is \code{TRUE}.
#' @param fun [\code{\link{function}}]\cr
#'   A function, which allows the computation of the objective values. If it is
#'   not provided, features that require additional function evaluations, can't
#'   be computed.
#' @template arg_lower_upper
#' @template arg_blocks
#' @param objective [\code{\link{character}(1)}]\cr
#'   The name of the feature, which contains the objective values. The
#'   default is \code{"y"}.
#'
#' @return [\code{\link{FeatureObject}}].
#'
#' @name FeatureObject
#' @rdname FeatureObject
#' @examples
#' # (1a) create a feature object using X and y:
#' X = createInitialSample(n.obs = 500, dim = 3,
#'   control = list(init_sample.lower = -10, init_sample.upper = 10))
#' y = apply(X, 1, function(x) sum(x^2))
#' feat.object1 = createFeatureObject(X = X, y = y, 
#'   lower = -10, upper = 10, blocks = c(5, 10, 4))
#'
#' # (1b) create a feature object using X and fun:
#' feat.object2 = createFeatureObject(X = X, 
#'   fun = function(x) sum(sin(x) * x^2),
#'   lower = -10, upper = 10, blocks = c(5, 10, 4))
#'
#' # (1c) create a feature object using a data.frame:
#' feat.object3 = createFeatureObject(iris[,-5], blocks = 5, 
#'   objective = "Petal.Length")
#'
#' # (2) have a look at the feature objects:
#' feat.object1
#' feat.object2
#' feat.object3
#'
#' # (3) now, one could calculate features
#' calculateFeatureSet(feat.object1, "cm_conv", control = list(cm_conv.diag = TRUE))
#' calculateFeatureSet(feat.object2, "cm_grad")
#' library(plyr)
#' calculateFeatureSet(feat.object3, "cm_angle", control = list(cm_angle.show_warnings = FALSE))
#'
#' @export 
createFeatureObject = function(init, X, y, fun, minimize, 
  lower, upper, blocks, objective) {

  if (missing(init) && (missing(X) || (missing(y) && missing(fun)) ))
    stop("The initial design has to be provided either by init or by X and y or by X and f.")
  if (!missing(X) && !missing(y)) {
    if (length(y) != nrow(X))
      stop("The number of rows in X has to be identical to the length of y!")
  }
  ## extract information on lower and upper bounds from initial sample
  if (!missing(X)) {
    provided.lower = attr(X, "lower")
    provided.upper = attr(X, "upper")
  } else {
    provided.lower = provided.upper = NULL
  }
  ## if the initial data is missing, it will be generated based on X and y (or
  ## fun if the latter is not available)
  if (missing(init)) {
    feat.names = colnames(X)
    if (is.null(feat.names)) {
      feat.names = sprintf("x%i", 1:ncol(X))
    }
    if (missing(objective))
      objective = "y"
    if (!inherits(X, "data.frame")) {
      X = as.data.frame(X)
      colnames(X) = feat.names
    }
    init = X
    if (missing(y))
      y = apply(X, 1, fun)
    init[[objective]] = y
  } else {
    if (missing(objective))
      objective = "y"
    feat.names = colnames(init)
    if (!(objective %in% feat.names))
      stop("The initial design has to include a column with the name of the objective.")
    feat.names = setdiff(feat.names, objective)
    if (missing(y)) {
      y = init[, objective]
    }
    if (missing(X)) {
      X = as.data.frame(init[, feat.names])
    }
    init = as.data.frame(init[, c(feat.names, objective)])
  }
  if (missing(minimize)) {
    minimize = TRUE
  }
  d = ncol(X)
  if (missing(lower)) {
    if (!is.null(provided.lower))
      lower = provided.lower
    else
      lower = vapply(X, min, double(1))
  }
  if (missing(upper)) {
    if (!is.null(provided.upper)) {
      upper = provided.upper
    } else {
      upper = vapply(X, max, double(1))
    }
  }
  if (length(lower) != d) {
    if (length(lower) == 1L) {
      lower = rep(lower, d)
    } else {
      stop("The size of 'lower' does not fit to the dimension of the initial design.")
    }
  }
  if (length(upper) != d) {
    if (length(upper) == 1L) {
      upper = rep(upper, d)
    } else {
      stop("The size of 'upper' does not fit to the dimension of the initial design.")
    }
  }
  if (any(lower > vapply(X, min, double(1)))) {
    stop("The initial data set contains values that are lower than the given lower limits.")
  }
  if (any(upper < vapply(X, max, double(1)))) {
    stop("The initial data set contains values that are bigger than the given upper limits.")
  }
  allows.cellmapping = TRUE
  if (missing(blocks)) {
    blocks = rep(1L, d)
    allows.cellmapping = FALSE
  } else {
    blocks = as.integer(blocks)
  }
  if (length(blocks) != d) {
    if (length(blocks) == 1L) {
      blocks = rep(blocks, d)
    } else {
      stop("The size of 'blocks' does not fit to the dimension of the initial design.")
    }
  }
  if (missing(fun))
    fun = NULL
  env = new.env(parent = emptyenv())
  env$init = init
  init.grid = convertInitDesignToGrid(init = init,
    lower = lower, upper = upper, blocks = blocks)
  centers = computeGridCenters(lower = lower, upper = upper, blocks = blocks)
  colnames(centers)[1:d] = feat.names
  res =  makeS3Obj("FeatureObject", env = env,
    minimize = minimize, fun = fun,
    lower = lower, upper = upper,
    dim = length(lower), n.obs = length(y),
    feature.names = feat.names,
    objective.name = objective,
    blocks = blocks,
    total.cells = prod(blocks),
    allows.cellmapping = allows.cellmapping,
    init.grid = init.grid,
    cell.centers = centers,
    cell.size = (upper - lower) / blocks)
  if (allows.cellmapping) {
    res$env$gcm.representatives = list() # to be filled on first call to gcm_init()
    res$env$gcm.canonicalForm = list()   # to be filled on first call to gcm_init()
  }
  return(res)
}

#' @export
print.FeatureObject = function(x, ...) {
  cat("Feature Object:\n")

  catf("- Number of Observations: %i", x$n.obs)
  catf("- Number of Features: %i", x$dim)
  if (x$dim < 5L) {
    catf("- Lower Boundaries: %s", collapse(sprintf("%.2e", x$lower), sep=", "))
    catf("- Upper Boundaries: %s", collapse(sprintf("%.2e", x$upper), sep=", "))
    catf("- Name of Features: %s", collapse(x$feature.names, sep = ", "))
  } else {
    catf("- Lower Boundaries: %s, ...", collapse(sprintf("%.2e", x$lower[1:4]), sep=", "))
    catf("- Upper Boundaries: %s, ...", collapse(sprintf("%.2e", x$upper[1:4]), sep=", "))
    catf("- Name of Features: %s, ...", collapse(x$feature.names[1:4], sep = ", "))
  }
  catf("- Optimization Problem: %s %s", 
       ifelse(x$minimize, "minimize", "maximize"), x$objective.name)
  if (!is.null(x$fun)) {
    if (inherits(x$fun, "smoof_function")) {
      fun = sprintf("smoof-function (%s)", attr(x$fun, "name"))
    } else {
      fun = as.character(enquote(x$fun))[2]
      fun = paste(unlist(strsplit(fun, "\n")), collapse = "")    
    }
    catf("- Function to be Optimized: %s", fun)
  }
  if (x$allows.cellmapping) {
    if (x$dim < 5L) {
      catf("- Number of Cells per Dimension: %s", collapse(sprintf("%i", x$blocks), sep=", "))
    } else {
      catf("- Number of Cells per Dimension: %s, ...", collapse(sprintf("%i", x$blocks[1:4]), sep=", "))
    }
    if (x$dim < 5L) {
      catf("- Size of Cells per Dimension: %s", collapse(sprintf("%.2f", x$cell.size), sep=", "))
    } else {
      catf("- Size of Cells per Dimension: %s, ...", collapse(sprintf("%.2f", x$cell.size[1:4]), sep=", "))
    }
    filled.cells = length(unique(x$init.grid$cell.ID))
    cat("- Number of Cells:\n")
    catf("  - total: %i", x$total.cells)
    catf("  - non-empty: %i (%.2f%%)", filled.cells, 100 * filled.cells / x$total.cells)
    catf("  - empty: %i (%.2f%%)", x$total.cells - filled.cells,
      100 * (x$total.cells - filled.cells) / x$total.cells)
    cat("- Average Number of Observations per Cell:\n")
    catf("  - total: %.2f", x$n.obs / x$total.cells)
    catf("  - non-empty: %.2f", x$n.obs / filled.cells)  
  }
}
