#' @title Find Neighbouring Cells
#'
#' @description
#'   Given a vector of cell IDs (\code{cell.ids}) and a vector (\code{blocks}),
#'   which defines the number of blocks / cells per dimension, a list of all
#'   combinations of (linearly) neighbouring cells around each element of
#'   \code{cell.ids} is returned.
#'
#' @param cell.ids [\code{\link{integer}}]\cr
#'   Vector of cell IDs (one number per cell) for which the neighbouring cells
#'   should be computed.
#' @template arg_blocks
#' @param diag [\code{\link{logical}(1)}]\cr
#'   \code{logical}, indicating whether only cells that are located parallel to
#'   the axes should be considered (\code{diag = FALSE}) as neighbours.
#'   Alternatively, one can also look for neighbours that are located
#'   diagonally to a cell. The default is \code{diag = FALSE}.
#'
#' @return [\code{\link{list}} of \code{\link{integer}(3)}].\cr
#'   List of neighbours. Each list element stands for a combination of
#'   predecessing, current and succeeding cell.
#'
#' @examples
#' cell.ids = c(5, 84, 17, 23)
#' blocks = c(5, 4, 7)
#'
#' # (1) Considering diagonal neighbours as well:
#' findLinearNeighbours(cell.ids = cell.ids, blocks = blocks, diag = TRUE)
#'
#' # (2) Only consider neighbours which are parellel to the axes:
#' findLinearNeighbours(cell.ids = cell.ids, blocks = blocks)
#'
#' @export
findLinearNeighbours = function(cell.ids, blocks, diag = FALSE) {
  assertIntegerish(cell.ids)
  cell.ids = as.integer(cell.ids)
  dims = seq_along(blocks)
  max.cell = prod(blocks)
  cell.z = t(vapply(cell.ids, celltoz, blocks = blocks, integer(max(dims))))
  if (diag) {
    combs = expand.grid(lapply(dims, function(d) -1:1))
    combs = combs[!apply(combs, 1, function(z) all(z %in% c(-1, 0))), ]
  } else {
    combs = diag(length(dims))
  }
  nbs = lapply(seq_along(cell.ids), function(i) {
    x = cell.z[i, ]
    if (all((x == blocks) || (x == 1))) {
      return(list(NULL))
    }
    lapply(1:nrow(combs), function(k) {
      succ = ztocell(x + combs[k, ], blocks)
      if (is.null(succ))
        return(NULL)
      nb = c(2L * cell.ids[i] - succ, cell.ids[i], succ)
      if (all(nb >= 1) & all(nb <= max.cell)) 
        return(nb)
    })
  })
  nbs = do.call(c, nbs)
  nbs = nbs[!vapply(nbs, is.null, logical(1))]
  if (diag & (length(nbs) != 0)) {
    nbs = lapply(nbs, function(nb) nb[sort.list(nb, method = "radix")])
    nbs = nbs[!duplicated(nbs)]
    ind = vapply(nbs, function(h) h[1:2], integer(2))
    return(nbs[order(ind[2,], ind[1,])])
  } else {
    return(nbs)
  }
}

## converts a vector of blocks per dimension into a single cell ID
ztocell = function (z, blocks) {
  if (any(z > blocks) || any(z < 1L))
    return(NULL)
  z = z - 1L
  dim.prod = c(1, cumprod(blocks[-length(blocks)]))
  as.integer(sum(dim.prod * z) + 1L)
}

## converts a single cell ID into the blocks per dimension
celltoz = function (cell, blocks) {
  if ((cell < 1L) || (cell > prod(blocks)))
    return(NULL)
  cell = cell - 1L
  coord = NULL
  # calculate one coordinate per dimension
  for (i in seq_along(blocks)) {
    coord = c(coord,  cell %% blocks[i])
    cell = cell %/% blocks[i]
  }
  return(as.integer(coord + 1L))
}
