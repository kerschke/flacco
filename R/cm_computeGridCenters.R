#' @title Compute the Cell Centers of a Cell Mapping Grid
#'
#' @description
#'   Computes the cell centers and the corresponding cell IDs of a cell mapping
#'   grid.
#'
#' @template arg_lower_upper
#' @template arg_blocks
#'
#' @return [\code{\link{data.frame}}].\cr
#'   A \code{data.frame}, which includes the coordinates of the cell centers,
#'   as well as the corresponding cell ID (\code{cell.ID}).
#'
#' @examples
#' computeGridCenters(lower = -10, upper = 10, blocks = c(10, 5, 8))
#'
#' @export 
computeGridCenters = function(lower, upper, blocks) {
  assertNumeric(lower)
  assertNumeric(upper)
  assertIntegerish(blocks)
  ll = length(lower)
  lu = length(upper)
  lb = length(blocks)
  if (length(unique(c(ll, lu, lb))) != 1L) {
    dims = max(ll, lu, lb)
    if (ll == 1L)
      lower = rep(lower, dims)
    if (lu == 1L)
      upper = rep(upper, dims)
    if (lb == 1L)
      blocks = rep(blocks, dims)
  }
  centers = lapply(seq_along(lower), function(d) {
    x = seq(lower[d], upper[d], length = blocks[d] + 1L)
    return ((x[-1L] + x[-length(x)]) / 2)
  })
  grid = expand.grid(centers)
  names(grid) = sprintf("x%i", 1:ncol(grid))
  grid = convertInitDesignToGrid(lower = lower, upper = upper, blocks = blocks,
    init = cbind(grid, y = NA_integer_))
  grid$y = NULL
  return (grid)
}
