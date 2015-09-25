#' @title Converts an Initial Design into a Cell Mapping Grid
#' @description
#'   This function takes an initial design -- with rows being the observations
#'   and columns standing for the dimensions (plus the corresponding objective)
#'   -- and adds an additional column to the \code{data.frame}. This additional
#'   column states the cell ID for each observation.
#'
#' @template arg_init
#' @template arg_lower_upper
#' @template arg_blocks
#'
#' @return [\code{\link{data.frame}}].\cr
#'   A \code{data.frame}, which includes an additional column (\code{cell.ID})
#'   compared to the initial design (\code{init}). The \code{cell.ID} will be a
#'   value between 1 and \code{prod(blocks)}.
#'
#' @examples
#' # (1) create an initial design:
#' X = t(replicate(n = 200, expr = runif(n = 5, min = -10, max = 10)))
#' f = function(x) sum(x^2)
#' y = apply(X = X, MARGIN = 1, FUN = f)
#' init = data.frame(X, y = y)
#'
#' # (2) compute the cell mapping grid
#' convertInitDesignToGrid(init = init, lower = -10, upper = 10, blocks = 20)
#'
#' @export 
convertInitDesignToGrid = function(init, lower, upper, blocks) {
  if (class(init) == "matrix")
    init = as.data.frame(init)
  stopifnot(is.data.frame(init))
  dims = ncol(init) - 1L
  if (missing(lower)) {
    lower = apply(init[,1:dims], 2, min)
  } else if (length(lower) == 1L) {
    lower = rep(lower, dims)
  }
  if (missing(upper)) {
    upper = apply(init[, 1:dims], 2, max)
  } else if (length(upper) == 1L) {
    upper = rep(upper, dims)
  }
  if (missing(blocks)) {
    blocks = rep(10L, dims)
  } else if (length(blocks) == 1L) {
    blocks = as.integer(rep(blocks, dims))
  }
  block.widths = (upper - lower) / blocks
  cp = cumprod(c(1L, blocks))
  init$cell.ID = vapply(1L:nrow(init), function(init.row) {
    z = as.numeric(init[init.row, 1L:dims])
    cell.ID = vapply(1L:dims, function(dim) {
      as.integer(cp[dim] * floor((z[dim] - lower[dim]) / block.widths[dim]))
    }, integer(1))
    ## if observation is on upper limit of a dimension,
    ## it needs to be adjusted
    as.integer(sum(cell.ID - cp[seq_along(blocks)] * (z == upper)))
  }, integer(1))
  # adjust the range to 1 to prod(blocks) instead of 0 to prod(blocks) - 1
  init$cell.ID = init$cell.ID + 1L
  return(init)
}
