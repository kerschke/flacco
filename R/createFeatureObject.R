#' @title Create a Feature Object
#' @description
#' Create a feature object, which will be used as input for all
#' the feature computations.
#' @param init [\code{\link{data.frame}}]\cr
#'   A \code{data.frame}, which can be used as initial design. If not provided,
#'   it will be created either based on \code{X} and \code{y} or \code{X} and
#'   \code{fun}.
#' @param X [\code{\link{data.frame}} or \code{\link{matrix}}]\cr
#'   A \code{matrix} or \code{data.frame} containing the features of the
#'   initial design. If not provided, it will be extracted from \code{init}.
#' @param y [\code{\link{numeric}} or \code{\link{integer}}]\cr
#'   A vector containing the objective values of the initial design.
#'   If not provided, it will be extracted from \code{init}.
#' @param minimize [\code{\link{logical}(1)}]\cr
#'   Should the objective function be minimized? The default is \code{TRUE}.
#' @param fun [\code{\link{function}}]\cr
#'   A function, which allows the computation of the objective values. If it is
#'   not provided, features that require additional function evaluations, can't
#'   be computed.
#' @param lower [\code{\link{numeric}} or \code{\link{integer}}]\cr
#'   A vector containing the lower bounds of each feature. If not provided,
#'   the minimum value per feature will be taken as lower bound.
#' @param upper [\code{\link{numeric}} or \code{\link{integer}}]\cr
#'   A vector containing the upper bounds of each feature. If not provided,
#'   the maximum value per feature will be taken as upper bound.
#' @param blocks [\code{\link{integer}}]\cr
#'   A vector containing the number of cells per dimension. If not provided,
#'   cell mapping features can't be computed.
#' @param objective [\code{\link{character}(1)}]\cr
#'   The name of the \dQuote{feature}, which contains the objective values. Per
#'   default, the objective will be defined as \code{"y"}.
#' @return FeatureObject [\code{\link{FeatureObject}}].\cr
#'   An object, containing all the information, which is needed for the
#'   computation of the landscape features.
#' @name FeatureObject
#' @examples
#' # (1a) create a feature object using X and y:
#' X = t(replicate(n = 500, expr = runif(n = 3, min = -10, max = 10)))
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
#' # (3) calculate cell mapping features
#' calculateFeatureSet(feat.object1, "cell_convexity", control = list(cm_conv.diag = TRUE))
#' calculateFeatureSet(feat.object2, "gradient_homogeneity")
#' library(plyr)
#' calculateFeatureSet(feat.object3, "angle", control = list(angle.show_warnings = FALSE))
#' @export 

createFeatureObject = function(init, X, y, fun, minimize, 
  lower, upper, blocks, objective) {
    if (missing(init) && (missing(X) || (missing(y) && missing(fun)) ))
      stop("The initial design has to be provided either by init or by X and y or by X and f.")
    if (!missing(X) && !missing(y)) {
      if (length(y) != nrow(X))
        stop("The number of rows in X has to be identical to the length of y!")
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
      if (class(X) != "data.frame") {
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
      lower = vapply(X, min, double(1))
    }
    if (missing(upper)) {
      upper = vapply(X, max, double(1))
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
      blocks = rep(NA, d)
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
    res =  makeS3Obj("FeatureObject", env = env, 
      minimize = minimize, fun = fun,
      lower = lower, upper = upper, 
      dim = length(lower), n.obs = length(y),
      feature.names = feat.names, 
      objective.name = objective,
      blocks = blocks,
      allows.cellmapping = allows.cellmapping)
    if (allows.cellmapping) {
      res$init.grid = convertInitDesignToGrid(init = init, 
        lower = lower, upper = upper, blocks = blocks)
      centers = computeGridCenters(lower = lower, upper = upper, blocks = blocks)
      colnames(centers)[1:d] = feat.names
      res$cell.centers = centers
      res$cell.size = (upper - lower) / blocks
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
  catf("- Optimization problem: %s %s", 
       ifelse(x$minimize, "minimize", "maximize"), x$objective.name)
  if (!is.null(x$fun)) {
    fun = as.character(enquote(x$fun))[2]
    fun = paste(unlist(strsplit(fun, "\n")), collapse = "")
    catf("- Function to be optimized: %s", fun)
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
    total.cells = prod(x$blocks)
    cat("- Number of Cells:\n")
    catf("  - total: %i", total.cells)
    catf("  - non-empty: %i (%.2f%%)", filled.cells, 100 * filled.cells / total.cells)
    catf("  - empty: %i (%.2f%%)", total.cells - filled.cells, 100 * (total.cells - filled.cells) / total.cells)
    cat("- Average Number of Observations per Cell:\n")
    catf("  - total: %.2f", x$n.obs / total.cells)
    catf("  - non-empty: %.2f", x$n.obs / filled.cells)  
  }
}