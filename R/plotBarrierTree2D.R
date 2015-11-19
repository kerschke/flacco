#' @title Plot Barrier Tree in 2D
#'
#' @description
#' Creates a 2D image containing the barrier tree of this cell mapping.
#'
#' @template arg_feat_object
#' @template arg_control
#' @details
#'   Possible \code{control} arguments are:
#'   \itemize{
#'     \item{Computation of Cell Mapping}: \itemize{
#'       \item{\code{gcm.approach}}: Which approach should be used when
#'       computing the representatives of a cell. The default is \code{"min"},
#'       i.e. the observation with the best (minimum) value within per cell.
#'       \item{\code{gcm.cf_power}}: Theoretically, we need to compute the
#'       canonical form to the power of infinity. However, we use this value
#'       as approximation of infinity. The default is \code{256}.
#'     }
#'     \item{Plot Control}: \itemize{
#'       \item{\code{bt.cm_surface}}: Should the underlying surface be based
#'       on a cell mapping plot (default is \code{TRUE})? Alternatively, the
#'       cells would be coloured in shades of grey - according to their
#'       objective values.
#'       \item{\code{bt.margin}}: Margins of the plot as used by
#'       \code{par("mar")}. The default is \code{c(5, 5, 4, 4)}.
#'       \item{\code{bt.color_surface}}: Color of the surface of the
#'       perspective plot. The default is \code{"lightgrey"}.
#'       \item{\code{bt.color_branches}}: Color used for the branches of the
#'       barrier tree. Per default there will be one color per level.
#'       \item{\code{bt.pch_root}}: Symbol used for plotting the root.
#'       The default is \code{17}.
#'       \item{\code{bt.pch_breakpoint}}: Symbol used for plotting a
#'       breakpoint. The default is \code{1}.
#'       \item{\code{bt.pch_basin}}: Symbol used for plotting the leaf (i.e. a
#'       basin) of the barrier tree. The default is \code{19}.
#'       \item{\code{bt.col_root}}: Color of the root symbol. The default is
#'       \code{"red"}.
#'       \item{\code{bt.lwd}}: Width of the lines used for plotting the
#'       branches of a barrier tree. The default is \code{2}.
#'       \item{\code{bt.label.{x, y}_coord}}: Label of the x-/y-coordinate
#'       (below / left side of the plot).
#'       \item{\code{bt.label.{x, y}_id}}: Label of the x-/y-cell ID (above /
#'       right side of the plot).
#'     }
#'   }
#' @return [\code{plot}].\cr
#'   A 2D image, visualizing the barrier tree of this cell mapping.
#' @examples
#' # create a feature object
#' X = createInitialSample(n.obs = 900, dim = 2)
#' f = smoof::makeAckleyFunction(dimensions = 2)
#' y = apply(X, 1, f)
#' feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = c(4, 6))
#' 
#' # plot the corresponing barrier tree
#' plotBarrierTree2D(feat.object)
#' @export
plotBarrierTree2D = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (!feat.object$allows.cellmapping)
    stop("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")
  if (feat.object$dim != 2)
    stop("The barrier trees can currently only be visualized for 2-dimensional problems!")
  X = extractFeatures(feat.object)
  y = extractObjective(feat.object)
  if (missing(control))
    control = list()
  assertList(control)

  approach = control_parameter(control, "gcm.approach", "min")
  assertChoice(approach, choices = c("min", "mean", "near"))
  cf.power = control_parameter(control, "gcm.cf_power", 256L)
  assertInt(cf.power, lower = 1L, upper = Inf)
  gcm.control = list(cf.power = cf.power)

  yvals = getObjectivesByApproach(feat.object, approach)
  yvals[is.infinite(yvals)] = max(yvals[is.finite(yvals)]) * 100
  sparse.matrix = calculateSparseMatrix(feat.object, yvals)
  canonical.list = computeCanonical(sparse.matrix)
  fundamental.list = computeFundamental(
    canonical.list = canonical.list,
    gcm.control = gcm.control)
  barrier.tree = createBarrierTree(feat.object, fundamental.list,
    canonical.list, yvals, control)
  base = barrier.tree$base
  max.node.per.level = cumsum(barrier.tree$base^(0:barrier.tree$max.levels))
  levels = vapply(barrier.tree$tree.index, function(x)
    min(which(x <= max.node.per.level)), integer(1L)) - 1L

  orig.margins = par("mar")
  on.exit(par(mar = orig.margins))
  par(mar = control_parameter(control, "bt.margin", c(5, 5, 4, 4)))

  blocks = feat.object$blocks
  yvals[yvals == Inf] = NA_real_
  attr(yvals, "dim") = c(blocks[1], blocks[2])

  if (control_parameter(control, "bt.cm_surface", TRUE)) {
    control$gcm.plot_arrows = control_parameter(control, "gcm.plot_arrows", FALSE)
    control$gcm.approach = approach
    control$gcm.label.x_coord = control_parameter(
      control, "bt.label.x_coord", "Cell Coordinate (1st Dimension)")
    control$gcm.label.y_coord = control_parameter(
      control, "bt.label.y_coord", "Cell Coordinate (2nd Dimension)")
    control$gcm.label.x_id = control_parameter(
      control, "bt.label.x_id", "Cell ID (1st Dimension)")
    control$gcm.label.y_id = control_parameter(
      control, "bt.label.y_id", "Cell ID (2nd Dimension)")
    plotCellMapping(feat.object, control = control)
  } else {
    image(x = seq_len(blocks[1]), y = seq_len(blocks[2]), z = yvals,
      col = grey(seq(0, 1, length.out = feat.object$total.cells)),
      xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    # additional axes that represent values of original feature dimensions
    xlab_coord = control_parameter(control, "bt.label.x_coord",
      "Cell Coordinate (1st Dimension)")
    ylab_coord = control_parameter(control, "bt.label.y_coord",
      "Cell Coordinate (2nd Dimension)")
    xlab_id = control_parameter(control, "bt.label.x_id",
      "Cell ID (1st Dimension)")
    ylab_id = control_parameter(control, "bt.label.y_id",
      "Cell ID (2nd Dimension)")
    if (control_parameter(control, "gcm.plot_coord_labels", TRUE)) {
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
      mtext(side = 3, xlab_id, line = 2.5, cex = par("cex"))
      axis(side = 3, at = seq_len(blocks[1]))
      mtext(side = 4, ylab_id, line = 2.5, cex = par("cex"))
      axis(side = 4, at = seq_len(blocks[2]), las = 1)
    } 
  }
  col.branches = control_parameter(control,
    "bt.color_branches", topo.colors(max(levels)))

  pch.root = control_parameter(control, "bt.pch_root", 17)
  pch.break = control_parameter(control, "bt.pch_breakpoint", 1)
  pch.basin = control_parameter(control, "bt.pch_basin", 19)
  col.root = control_parameter(control, "bt.col_root", "red")
  checkPch(pch.root)
  checkPch(pch.break)
  checkPch(pch.basin)
  lwd.branches = control_parameter(control, "bt.lwd", 2)
  assertNumber(lwd.branches, lower = 0.1, upper = 10)

  indices = barrier.tree$tree.index
  nodes = barrier.tree$tree.nodes

  for (i in rev(seq_along(indices)[-1])) {
    level = levels[i]
    cur.node = nodes[i]
    cur.coord = celltoz(cur.node, blocks)
    prev.node = nodes[indices == ceiling((indices[i] - 1) / base)]
    prev.coord = celltoz(prev.node, blocks)
    successor.index = sum(base^(0:level)) + 
      base * (indices[i] - 1 - sum(base^(0:(level - 1)))) + seq_len(base)
    cur.node.pch = ifelse(any(indices %in% successor.index), pch.break, pch.basin)
    points(cur.coord[1], cur.coord[2],
      col = col.branches[level], pch = cur.node.pch)
    text(cur.coord[1], cur.coord[2], labels = cur.node,
      pos = 1, col = col.branches[level])
    lines(rbind(cur.coord, prev.coord), lwd = lwd.branches, col = col.branches[level])
  }

  # draw root
  root = barrier.tree$root
  root.coord = celltoz(root, blocks)
  points(root.coord[1], root.coord[2], pch = pch.root, col = col.root)
  text(root.coord[1], root.coord[2],
    labels = sprintf("%i (root)", root), pos = 1, col = col.root)
}
