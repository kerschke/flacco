#' @title Plot Cell Mapping
#'
#' @description
#' Visualizes the transitions among the cells in the General Cell Mapping approach.
#'
#' @template arg_feat_object
#' @template arg_control
#' @details
#'   Possible \code{control} arguments are:
#'   \itemize{
#'     \item{Computation of GCM Features}: \itemize{
#'       \item{\code{gcm.approach}}: Which approach should be used when
#'       computing the representatives of a cell. The default is \code{"min"},
#'       i.e. the observation with the best (minimum) value within per cell.
#'       \item{\code{gcm.cf_power}}: Theoretically, we need to compute the
#'       canonical form to the power of infinity. However, we use this value
#'       as approximation of infinity. The default is \code{256}.
#'     }
#'     \item{Plot Control}: \itemize{
#'       \item{\code{gcm.margin}}: The margins of the plot as used by
#'       \code{par("mar")}. The default is \code{c(5, 5, 4, 4)}.
#'       \item{\code{gcm.color_attractor}}: Color of the attractors. The
#'       default is \code{"#333333"}, i.e. dark grey.
#'       \item{\code{gcm.color_uncertain}}: Color of the uncertain cells. The
#'       default is \code{"#cccccc"}, i.e. grey.
#'       \item{\code{gcm.color_basin}}: Color of the basins of attraction. This
#'       has to be a function, which computes the colors, depending on the
#'       number of attractors. The default is \code{function(n) topo.colors(n)}.
#'       \item{\code{gcm.plot_arrows}}: Should arrows be plotted? The default
#'       is \code{TRUE}.
#'       \item{\code{gcm.arrow.length_{x, y}}}: Scaling factor of the arrow
#'       length in x- and y-direction. The default is \code{0.9}, i.e. 90\%
#'       of the actual length.
#'       \item{\code{gcm.arrowhead.{length, width}}}: Scaling factor for the
#'       width and length of the arrowhead. Per default (\code{0.1}) the
#'       arrowhead is 10\% of the length of the original arrow.
#'       \item{\code{gcm.arrowhead.type}}: Type of the arrowhead. Possible
#'       options are \code{"simple"}, \code{"curved"}, \code{"triangle"}
#'       (default), \code{"circle"}, \code{"ellipse"} and \code{"T"}.
#'       \item{\code{gcm.color_grid}}: Color of the grid lines. The default is
#'       \code{"#333333"}, i.e. dark grey.
#'       \item{\code{gcm.label.{x, y}_coord}}: Label of the x-/y-coordinate
#'       (below / left side of the plot).
#'       \item{\code{gcm.label.{x, y}_id}}: Label of the x-/y-cell ID (above /
#'       right side of the plot).
#'       \item{\code{gcm.plot_{coord, id}_labels}}: Should the coordinate
#'       (bottom and left) / ID (top and right) labels be plotted? The default
#'       is \code{TRUE}.
#'     }
#'   }
#' @references
#'   \itemize{
#'     \item{Kerschke, P., Preuss, M., Hernandez, C., Schuetze, O., Sun, J.-Q.,
#'     Grimme, C., Rudolph, G., Bischl, B., and Trautmann, H. (2014)}:
#'     \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis},
#'     in: EVOLVE -- A Bridge between Probability, Set Oriented Numerics, and
#'     Evolutionary Computation V, pp. 115-131
#'     (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
#'   }
#' @return [\code{plot}].
#' @examples
#' # (1) Define a function:
#' library(smoof)
#' f = makeHosakiFunction()
#' 
#' # (2) Create a feature object:
#' X = cbind(
#'   x1 = runif(n = 100, min = -32, max = 32),
#'   x2 = runif(n = 100, min = 0, max = 10)
#' )
#' y = apply(X, 1, f)
#' feat.object = createFeatureObject(X = X, y = y, blocks = c(4, 6))
#' 
#' # (3) Plot the cell mapping:
#' plotCellMapping(feat.object)
#' @export
plotCellMapping = function (feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  assertList(control)

  approach = control_parameter(control, "gcm.approach", "min")
  assertChoice(x = approach, choices = c("min", "mean", "near"))
  cf.power = control_parameter(control, "gcm.cf_power", 256L)
  gcm.control = list(cf.power = cf.power)
  
  orig.margins = par("mar")
  on.exit(par(mar = orig.margins))
  par(mar = control_parameter(control, "gcm.margin", c(5, 5, 4, 4)))

  blocks = feat.object$blocks
  assertIntegerish(blocks, lower = 1, len = 2)

  yvals = getObjectivesByApproach(feat.object, approach)
  sparse.matrix = calculateSparseMatrix(feat.object, yvals)
  canonical.list = computeCanonical(sparse.matrix)
  fundamental.list = computeFundamental(
    canonical.list = canonical.list, gcm.control = gcm.control)
  fundamental.mat = fundamental.list$fundamental.mat
  permutation.index = fundamental.list$permutation.index
  attractors = seq_len(ncol(fundamental.mat))
  colors = matrix(NA, nrow = blocks[1L], ncol = blocks[2L])

  # To how many basins of attraction do cells belong?
  attr.cells = rowSums(fundamental.mat != 0)
  
  # cells belonging to more than one basin of attraction => col #1
  # cells belonging to exactly one basin of attraction (but which one?) => col #2+i
  color.index = apply(fundamental.mat != 0, 1, function(x) {
    if (sum(x) > 1)
      return (1L)
    else
      return (2L + which(x))
  })
  
  colors[permutation.index] = color.index
  
  # attractors (those cells which belong to themselves) => col #2
  colors[permutation.index[attractors]] = 2L
  
  arrow.mat = NULL  # will be a matrix with 4 rows and one column per arrow
  for (attractor.id in attractors) {
    attractor.cell = celltoz(permutation.index[attractor.id], blocks)
    attracted.cells = which(fundamental.mat[, attractor.id] != 0)

    # remove itself (would result in vector of length 0, which raises warnings)
    attracted.cells = attracted.cells[attracted.cells != attractor.id]
    if (length(attracted.cells) == 0)
      next
    
    arrows.to.attractor = vapply(attracted.cells, FUN = function(attracted.id) {
      attracted.cell = celltoz(permutation.index[attracted.id], blocks)
      direction = normalizeVector(attractor.cell - attracted.cell)
      weighted.direction = direction * fundamental.mat[attracted.id, attractor.id]
      return(c(attracted.cell, weighted.direction))
    }, double(4L))
    
    if (is.null(arrow.mat)) {
      arrow.mat = arrows.to.attractor
    } else {
      arrow.mat = cbind(arrow.mat, arrows.to.attractor)
    }
  }
  rownames(arrow.mat) = c("from.x", "from.y", "component.x", "component.y")

  # prepare colour palette
  col.attr = control_parameter(control, "gcm.color_attractor", "#333333")
  col.uncert = control_parameter(control, "gcm.color_uncertain", "#cccccc")
  col.basin = control_parameter(control, "gcm.color_basin", function(n) topo.colors(n))
  palette = c(col.uncert, col.attr, col.basin(length(attractors)))

  # cell information
  # force `image` to consider `color` as a matrix of discrete values
  # (otherwise, will try to use the full range of colors given)
  image(x = seq_len(blocks[1]), y = seq_len(blocks[2]), z = colors, 
    useRaster = TRUE, col = palette, xlab = "", ylab = "", las = 1,
    breaks = seq(0.5, length(attractors) + 2.5, 1),
    xlim = c(0.5, blocks[1] + 0.5), ylim = c(0.5, blocks[2] + 0.5),
    xaxt = "n", yaxt = "n"
  )

  # grid
  grid.col = control_parameter(control, "gcm.color_grid", "#333333")
  abline(v = seq(0.5, blocks[1] + 0.5), col = grid.col,
    xlim = c(0.5, blocks[1] + 0.5), ylim = c(0.5, blocks[2] + 0.5)
  )
  abline(h = seq(0.5, blocks[2] + 0.5), col = grid.col,
    xlim = c(0.5, blocks[1] + 0.5), ylim = c(0.5, blocks[1] + 0.5)
  )

  # attraction
  if (control_parameter(control, "gcm.plot_arrows", TRUE)) {
    arrow.type = control_parameter(control, "gcm.arrowhead.type", "triangle")
    assertChoice(arrow.type,
      choices = c("simple", "curved", "triangle", "circle", "ellipse", "T"))
    arrow.length_x = control_parameter(control, "gcm.arrow.length_x", 0.9)
    arrow.length_y = control_parameter(control, "gcm.arrow.length_y", 0.9)
    arrowhead.length = control_parameter(control, "gcm.arrowhead.length", 0.1)
    arrowhead.width = control_parameter(control, "gcm.arrowhead.width", 0.1)
    assertNumber(arrow.length_x, lower = 0)
    assertNumber(arrow.length_y, lower = 0)
    assertNumber(arrowhead.length, lower = 0)
    assertNumber(arrowhead.width, lower = 0)
    apply(arrow.mat, 2, FUN = function(arrow) {
      arrow.length = sqrt(sum(arrow[3:4]^2))
      shape::Arrows(x0 = arrow[1], y0 = arrow[2],
        x1 = arrow[1] + arrow[3] * arrow.length_x,
        y1 = arrow[2] + arrow[4] * arrow.length_y,
        arr.length = arrow.length * arrowhead.length, 
        arr.width = arrow.length * arrowhead.width,
        arr.type = arrow.type)
    })  
  }

  if (control_parameter(control, "gcm.plot_coord_labels", TRUE)) {
    xlab_coord = control_parameter(control, "gcm.label.x_coord",
      "Cell Coordinate (1st Dimension)")
    ylab_coord = control_parameter(control, "gcm.label.y_coord",
      "Cell Coordinate (2nd Dimension)")
    axis(1, at = seq_len(blocks[1]), labels = rep("", blocks[1]))
    text(x = seq_len(blocks[1]), y = 0.25, pos = 1, xpd = TRUE,
      sprintf("%.1e", unique(feat.object$cell.centers[[1]])), srt = 45)
    mtext(side = 1, xlab_coord, line = 4, cex = par("cex"))
    axis(2, at = seq_len(blocks[2]), labels = rep("", blocks[2]))
    text(y = seq_len(blocks[2]), x = 0.45, pos = 2, xpd = TRUE,
      sprintf("%.1e", unique(feat.object$cell.centers[[2]])), srt = 45)
    mtext(side = 2, ylab_coord, line = 4, cex = par("cex"))
  }
  if (control_parameter(control, "gcm.plot_id_labels", TRUE)) {
    xlab_id = control_parameter(control, "gcm.label.x_id",
      "Cell ID (1st Dimension)")
    ylab_id = control_parameter(control, "gcm.label.y_id",
      "Cell ID (2nd Dimension)")
    mtext(side = 3, xlab_id, line = 2.5, cex = par("cex"))
    axis(side = 3, at = seq_len(blocks[1]))
    mtext(side = 4, ylab_id, line = 2.5, cex = par("cex"))
    axis(side = 4, at = seq_len(blocks[2]), las = 1)
  }
}
