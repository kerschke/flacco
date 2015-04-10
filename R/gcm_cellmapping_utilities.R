# Utility functions that are very specific to GCM and this implementation of it.

# (origin: findneighbours.m)
# returns possible coordinates of neighbours of a cell, one coordinate per row.
## A cell is considered a neighbour of itself, but this is eliminated in next step
## (see "isNeighbourInvalid" below), together with neighbours outside the bounds.

## coordinate: vector, defining the cell by its block-dimensions
## nsize: ???
## cellSize: vector of d elements - indicating the cell size per dimension
findAllNeighbours = function (nsize, coord, cellSize) {
  ncsize = ceiling(nsize / (2 * cellSize))
  neighbours = lapply(seq_along(cellSize), 
    function(d) coord[d] + (-ncsize[d] : ncsize[d]))
  as.matrix(expand.grid(neighbours))
}

# check whether the neighbourCoords signify a valid neighbour of currentCoord,
# i.e. return TRUE for those neighbours that are outside the bounds of divisions or
#     that are the cell itself
## currentCoord: Coordinate of the current cell
## neighbourCoord: Coordinate of the neighbour
## divisions: Given GCM divisions
isNeighbourInvalid = function(currentCoord, neighbourCoord, divisions) {
  any(neighbourCoord <= 0) || 
    any( neighbourCoord > divisions ) ||
    all(neighbourCoord == currentCoord)
}

## converts a vector of blocks per dimension into a single cell ID
ztocell = function (z, divisions) {
  if (any(z > divisions))
    return(NULL)
  z = z - 1L
  dim.prod = c(1, cumprod(divisions[-length(divisions)]))
  sum(dim.prod * z) + 1L
}

## converts a single cell ID into the blocks per dimension
celltoz = function (cell, divisions) {
  cell = cell - 1L
  coord = NULL
  # calculate one coordinate per dimension
  for (i in seq_along(divisions)) {
    coord = c(coord,  cell %% divisions[i])
    cell = cell %/% divisions[i]
  }
  return(coord + 1L)
}

## computes the center point of a given cell coordinate z
ztox = function(z, cellSize, lowerBounds) {
  return (lowerBounds + ( cellSize * z ) - (cellSize/2))
}