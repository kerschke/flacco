#' @title Find Neighbouring Cells
#'
#' @description
#' Given a vector of cell IDs (\code{cell.ids}) and another vector
#' (\code{blocks}), which defines the number of cells per dimension, a list of
#' all combinations of (linearly) neighbouring cells around each element of
#' \code{cell.ids} is returned.
#' @param cell.ids [\code{\link{integer}}]\cr
#' Vector of cell IDs (one number per cell) for which the neighbouring cells
#' should be computed.
#' @param blocks [\code{\link{integer}}]\cr
#' Vector, defining the number of cells per dimension.
#' @param diag [\code{\link{logical}(1)}]\cr
#' Logical, indicating whether only cells that are located parallel to the
#' axes should be considered (\code{diag = FALSE}) as neighbours.
#' Alternatively, one can also look for neighbours that are located
#' diagonally to a cell. The default setting is \code{diag = FALSE}.
#' @return [\code{\link{list}} of \code{\link{integer}(3)}].\cr
#' List of neighbours.\cr
#' Each list element stands for a combination of predecessing, current
#' and succeeding cell.
#' @examples
#' cell.ids = c(5, 84, 17)
#' blocks = c(5, 4, 7)
#' # (1) Considering diagonal neighbours as well:
#' findLinearNeighbours(cell.ids = cell.ids, blocks = blocks, diag = TRUE)
#' # (2) Only consider neighbours which are parellel to the axes:
#' findLinearNeighbours(cell.ids = cell.ids, blocks = blocks)
#' @export 
findLinearNeighbours = function(cell.ids, blocks, diag = FALSE) {
  dims = seq_along(blocks)
  max.cell = prod(blocks)
  cell.z = t(sapply(cell.ids, celltoz, blocks))
  if (diag) {
    combs = expand.grid(lapply(dims, function(d) 0:1))[-1, ]
  } else {
    combs = diag(length(dims))
  }
  nbs = lapply(seq_along(cell.ids), function(i) {
    x = cell.z[i, ]
    lapply(1:nrow(combs), function(k) {
      succ = ztocell(x + combs[k, ], blocks)
      nb = c(2 * cell.ids[i] - succ, cell.ids[i], succ)
      if (all(nb >= 1) & all(nb <= max.cell)) 
        return(nb)
    })
  })
  nbs = do.call(c, nbs)
  nbs[sapply(nbs, function(x) !is.null(x))]
}