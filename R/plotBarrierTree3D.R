#' @title Plot Barrier Tree in 3D
#'
#' @description
#' Creates a 3D surface plot containing the barrier tree of this cell mapping.
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
#'       \item{\code{bt.margin}}: Margins of the plot as used by
#'       \code{par("mar")}. The default is \code{c(0.5, 1, 0, 0)}.
#'       \item{\code{bt.color_surface}}: Color of the surface of the
#'       perspective plot. The default is \code{"lightgrey"}.
#'       \item{\code{bt.color_branches}}: Color used for the branches of the
#'       barrier tree. Per default there will be one color per level.
#'       \item{\code{bt.persp_border}}: Color of the lines / borders around
#'       each facet of the perspective plot. The default is \code{"grey"}.
#'       \item{\code{bt.persp_shade}}: A ratio defining the shade of the
#'       surface. The default is \code{0.35}.
#'       \item{\code{bt.persp_{theta, phi}}}: Angles (in degree) defining the
#'       viewing direction of the perspective plot. \code{theta} corresponds to
#'       the azimuthal direction (default: \code{330}) and \code{phi} to the
#'       colatitude (default: \code{15}).
#'       \item{\code{bt.persp_{xlab, ylab, zlab}}}: Labels of the x-, y- and z-
#'       axis. The defaults are \code{expression(x[1])},
#'       \code{expression(x[2])} and \code{expression(f(x[1], x[2]))}
#'       \item{\code{bt.persp_ticktype}}: Should the values of each dimension
#'       be shown in detail (\code{"detailed"}) or just via \code{"simple"}
#'       arrows in direction of increasement along the axes? The default is
#'       \code{"detailed"}.
#'       \item{\code{bt.col_root}}: Color of the root symbol. The default is
#'       \code{"red"}.
#'       \item{\code{bt.pch_root}}: Symbol used for plotting the root.
#'       The default is \code{1}.
#'       \item{\code{bt.pch_breakpoint}}: Symbol used for plotting a
#'       breakpoint. The default is \code{1}.
#'       \item{\code{bt.pch_basin}}: Symbol used for plotting the leaf (i.e. a
#'       basin) of the barrier tree. The default is \code{19}.
#'       \item{\code{bt.lwd}}: Width of the lines used for plotting the
#'       branches of a barrier tree. The default is \code{2}.
#'     }
#'   }
#' @return [\code{plot}].\cr
#'   A 3D-surface plot, visualizing the barrier tree of this cell mapping.
#' @examples
#' # create a feature object
#' X = createInitialSample(n.obs = 900, dim = 2)
#' f = smoof::makeAckleyFunction(dimensions = 2)
#' y = apply(X, 1, f)
#' feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = c(4, 6))
#' 
#' # plot the corresponing barrier tree
#' plotBarrierTree3D(feat.object)
#' @export
plotBarrierTree3D = function(feat.object, control) {
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
  par(mar = control_parameter(control, "bt.margin", c(0.5, 1, 0, 0)))

  # prepare colour palette
  col.surface = control_parameter(control,
    "bt.color_surface", "lightgrey")
  col.branches = control_parameter(control,
    "bt.color_branches", topo.colors(max(levels)))
  
  blocks = feat.object$blocks
  yvals[yvals == Inf] = NA_real_
  attr(yvals, "dim") = c(blocks[1], blocks[2])

  theta = control_parameter(control, "bt.persp_theta", 330)
  phi = control_parameter(control, "bt.persp_phi", 15)
  border = control_parameter(control, "bt.persp_border", "grey")
  xlab = control_parameter(control, "bt.persp_xlab", expression(x[1]))
  ylab = control_parameter(control, "bt.persp_ylab", expression(x[2]))
  zlab = control_parameter(control, "bt.persp_zlab", expression(f(x[1], x[2])))
  ticktype = control_parameter(control, "bt.persp_ticktype", "detailed")
  assertChoice(ticktype, choices = c("detailed", "simple"))
  border = control_parameter(control, "bt.persp_border", "grey")
  shade = control_parameter(control, "bt.persp_shade", 0.35)
  assertNumber(shade, na.ok = TRUE)

  persp.plot = persp(x = seq_len(blocks[1]), y = seq_len(blocks[2]), z = yvals,
    theta = theta, phi = phi, border = border, xlab = xlab, ylab = ylab,
    zlab = zlab, col = col.surface, ticktype = ticktype, shade = shade)

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
    cur.point = trans3d(cur.coord[1], cur.coord[2],
      yvals[cur.node], persp.plot)
    prev.node = nodes[indices == ceiling((indices[i] - 1) / base)]
    prev.coord = celltoz(prev.node, blocks)
    prev.point = trans3d(prev.coord[1], prev.coord[2],
      yvals[prev.node], persp.plot)
    successor.index = sum(base^(0:level)) +
      base * (indices[i] - 1 - sum(base^(0:(level - 1)))) + seq_len(base)
    cur.node.pch = ifelse(any(indices %in% successor.index), pch.break, pch.basin)
    break.point = trans3d(cur.coord[1], cur.coord[2],
      yvals[prev.node], persp.plot)
    points(cur.point$x, cur.point$y,
      col = col.branches[level], pch = cur.node.pch)
    text(cur.point$x, cur.point$y, labels = cur.node,
      pos = 1, col = col.branches[level])
    # upwards
    lines(rbind(cur.point, break.point), lwd = lwd.branches,
      col = col.branches[level])
    # sideways
    lines(rbind(prev.point, break.point), lwd = lwd.branches,
      col = col.branches[level])
  }

  # draw root
  root = barrier.tree$root
  root.coord = celltoz(root, blocks)
  root.point = trans3d(root.coord[1], root.coord[2], yvals[root], persp.plot)
  points(root.point, pch = pch.root, col = col.root)
  text(root.point, labels = sprintf("%i (root)", root), pos = 1, col = col.root)
}
