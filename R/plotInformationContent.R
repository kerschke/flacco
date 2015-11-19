#' @title Plot Information Content
#'
#' @description
#' Creates a plot of the Information Content Features.
#'
#' @template arg_feat_object
#' @template arg_control
#' @details
#'   Possible \code{control} arguments are:
#'   \itemize{
#'     \item{Computation of Information Content Features}: \itemize{
#'       \item{\code{ic.epsilon}}: Epsilon values as described in section V.A
#'       of Munoz et al. (2015). The default is
#'       \code{c(0, 10^(seq(-5, 15, length.out = 1000))}.
#'       \item{\code{ic.sorting}}: Sorting strategy, which is used to define
#'       the tour through the landscape. Possible values are \code{"nn"}
#'       (= default) and \code{"random"}.
#'       \item{\code{ic.sample.generate}}: Should the initial design be created
#'       using a LHS? The default is \code{FALSE}, i.e. the initial design from
#'       the feature object will be used.
#'       \item{\code{ic.sample.dimensions}}: Dimensions of the initial sample,
#'       if created using a LHS. The default is \code{feat.object$dimension}.
#'       \item{\code{ic.sample.size}}: Size of the initial sample, if created
#'       using a LHS. The default is \code{100 * feat.object$dimension}.
#'       \item{\code{ic.sample.lower}}: Lower bounds of the initial sample, if
#'       created with a LHS. The default is \code{100 * feat.object$lower}.
#'       \item{\code{ic.sample.upper}}: Upper bounds of the initial sample, if
#'       created with a LHS. The default is \code{100 * feat.object$upper}.
#'       \item{\code{ic.show_warnings}}: Should warnings be shown, when
#'       possible duplicates are removed? The default is \code{FALSE}.
#'       \item{\code{ic.seed}}: Possible seed, which can be used for making
#'       your experiments reproducable. Per default, a random number will be
#'       drawn as seed.
#'       \item{\code{ic.nn.start}}: Which observation should be used as
#'       starting value, when exploring the landscape with the nearest
#'       neighbour approach. The default is a randomly chosen integer value.
#'       \item{\code{ic.nn.neighborhood}}: In order to provide a fast
#'       computation of the features, we use \code{RANN::nn2} for computing
#'       the nearest neighbors of an observation. Per default, we consider
#'       the \code{20L} closest neighbors for finding the nearest
#'       not-yet-visited observation. If all of those neighbors have been
#'       visited already, we compute the distances to the remaining points
#'       separately.
#'       \item{\code{ic.settling_sensitivity}}: Threshold, which should be
#'       used for computing the \dQuote{settling sensitivity}. The default
#'       is \code{0.05} (as used in the corresponding paper).
#'       \item{\code{ic.info_sensitivity}}: Portion of partial information
#'       sensitivity. The default is \code{0.5} (as used in the paper).
#'     }
#'     \item{Plot Control}: \itemize{
#'       \item{\code{ic.plot.{xlim, ylim, las, xlab, ylab}}}: Settings of the
#'       plot in general, cf. \code{\link[graphics]{plot.default}}.
#'       \item{\code{ic.plot.{xlab_line, ylab_line}}}: Position of \code{xlab}
#'       and \code{ylab}.
#'       \item{\code{ic.plot.ic.{lty, pch, cex, pch_col}}}: Type, width and colour
#'       of the line visualizing the \dQuote{Information Content} \eqn{H(\epsilon)}.
#'       \item{\code{ic.plot.max_ic.{lty, pch, lwd, cex, line_col, pch_col}}}:
#'       Type, size and colour of the line and point referring to the
#'       \dQuote{Maximum Information Content} \eqn{H[max]}.
#'       \item{\code{ic.plot.settl_sens.{pch, cex, col}}}:
#'       Type, size and colour of the point referring to the
#'       \dQuote{Settling Sensitivity} \eqn{\epsilon[s]}.
#'       \item{\code{ic.plot.partial_ic}}: Should the information of the partial
#'       information content be plotted as well? The default is \code{TRUE}.
#'       \item{\code{ic.plot.partial_ic.{lty, pch, lwd, cex, line_col, pch_col}}}:
#'       Type, size and colour of the line and point referring to the
#'       \dQuote{Initial Partial Information} \eqn{M[0]} and the
#'       \dQuote{Partial Information Content} \eqn{M(\epsilon)}.
#'       \item{\code{ic.plot.half_partial.{pch, cex, pch_col}}}:
#'       Type, size and colour of the point referring to the
#'       \dQuote{Relative Partial Information Sensitivity} \eqn{\epsilon[ratio]}.
#'       \item{\code{ic.plot.half_partial.{lty, line_col, lwd}_{h, v}}}:
#'       Type, colour and width of the horizontal and vertical lines referring
#'       to the \dQuote{Relative Partial Information Sensitivity} \eqn{\epsilon[ratio]}.
#'       \item{\code{ic.plot.half_partial.text_{cex, col}}}:
#'       Size and colour of the text at the horizontal line of the
#'       \dQuote{Relative Partial Information Sensitivity} \eqn{\epsilon[ratio]}.
#'       \item{\code{ic.plot.legend_{descr, points, lines, location}}}:
#'       Description, points, lines and location of the legend.
#'     }
#'   }
#' @references
#'   \itemize{
#'     \item{Munoz, M. A., Kirley, M., and Halgamuge, S. K. (2015)}:
#'     \dQuote{Exploratory Landscape Analysis of Continuous Space Optimization
#'     Problems Using Information Content}, in: IEEE Transactions on
#'     Evolutionary Computation (19:1), pp. 74-87
#'     (\url{http://dx.doi.org/10.1109/TEVC.2014.2302006}).
#'   }
#' @return [\code{plot}].\cr
#'   A plot visualizing the Information Content Features.
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))
#' 
#' # (2) plot its information content features:
#' plotInformationContent(feat.object)
#' @export
plotInformationContent = function (feat.object, control) {

  if (missing(control))
    control = list()
  res = computeInfoContentStatistics(feat.object, control)
  epsilon = res$eps
  H = res$H
  M = res$M

  # Information Content H(epsilon)
  ic.xlim = log10(epsilon)
  ic.xlim = control_parameter(control, "ic.plot.xlim", range(ic.xlim[is.finite(ic.xlim)]))
  ic.ylim = control_parameter(control, "ic.plot.ylim", range(H))
  ic.las = control_parameter(control, "ic.plot.las", 1L)
  ic.lty = control_parameter(control, "ic.plot.ic.lty", 1L)
  checkLty(ic.lty)
  ic.lwd = control_parameter(control, "ic.plot.ic.lwd", 1L)
  assertNumber(ic.lwd, lower = 0.1, upper = 10)
  ic.line_col = control_parameter(control, "ic.plot.ic.line_col", "red")
  plot(log10(epsilon), H, type = "l", lty = ic.lty, las = ic.las, lwd = ic.lwd,
    xlim = ic.xlim, ylim = ic.ylim, col = ic.line_col, ylab = "", xlab = "")

  # Maximum Information Content H_max
  max_ic.lty = control_parameter(control, "ic.plot.max_ic.lty", 4L)
  checkLty(max_ic.lty)
  max_ic.pch = control_parameter(control, "ic.plot.max_ic.pch", 1L)
  checkPch(max_ic.pch)
  max_ic.cex = control_parameter(control, "ic.plot.max_ic.cex", 1L)
  assertNumber(max_ic.cex, lower = 0.1, upper = 10)
  max_ic.pch_col = control_parameter(control, "ic.plot.max_ic.pch_col", "red")
  points(log10(epsilon[which(H == res$Hmax)]), res$Hmax, pch = max_ic.pch,
    cex = max_ic.cex, col = max_ic.pch_col)
  max_ic.lwd = control_parameter(control, "ic.plot.max_ic.lwd", 1L)
  assertNumber(max_ic.lwd, lower = 0.1, upper = 10)
  max_ic.line_col = control_parameter(control, "ic.plot.max_ic.line_col", "red")
  abline(v = log10(epsilon[which(H == res$Hmax)]), lty = max_ic.lty,
    lwd = max_ic.lwd, col = max_ic.line_col)

  # Settling Sensitivity epsilon_s
  settl.pch = control_parameter(control, "ic.plot.settl_sens.pch", 0L)
  checkPch(settl.pch)
  settl.cex = control_parameter(control, "ic.plot.settl_sens.cex", 1L)
  assertNumber(settl.cex, lower = 0.1, upper = 10)
  settl.col = control_parameter(control, "ic.plot.settl_sens.col", "black")
  points(res$eps.S, 0, pch = settl.pch, cex = settl.cex, col = settl.col)

  partial.ic = control_parameter(control, "ic.plot.partial_ic", TRUE)
  assertLogical(partial.ic)
  ylab = expression(H(epsilon))
  if (partial.ic) {
    # M(epsilon)
    ylab = expression(H(epsilon) * "  &  " * M(epsilon))
    partial_ic.lty = control_parameter(control, "ic.plot.partial_ic.lty", 2L)
    checkLty(partial_ic.lty)
    partial_ic.line_col = control_parameter(control, "ic.plot.partial_ic.line_col", "blue")
    partial_ic.lwd = control_parameter(control, "ic.plot.partial_ic.lwd", 1L)
    assertNumber(partial_ic.lwd, lower = 0.1, upper = 10)
    lines(log10(epsilon), M, lty = partial_ic.lty, col = partial_ic.line_col,
      lwd = partial_ic.lwd)

    # M_0
    partial_ic.pch = control_parameter(control, "ic.plot.partial_ic.pch", 5L)
    checkPch(partial_ic.pch)
    partial_ic.pch_col = control_parameter(control, "ic.plot.partial_ic.pch_col", "blue")
    partial_ic.cex = control_parameter(control, "ic.plot.partial_ic.cex", 1L)
    assertNumber(partial_ic.cex, lower = 0.1, upper = 10)
    points(log10(min(epsilon[epsilon > 0])), res$M0, pch = partial_ic.pch,
      cex = partial_ic.cex, col = partial_ic.pch_col) # (because log10(0) = -Inf)

    # epsilon_ratio
    half_partial.pch = control_parameter(control, "ic.plot.half_partial.pch", 6L)
    checkPch(half_partial.pch)
    half_partial.pch_col = control_parameter(control, "ic.plot.half_partial.pch_col", "darkgreen")
    half_partial.cex = control_parameter(control, "ic.plot.half_partial.cex", 1L)
    assertNumber(half_partial.cex, lower = 0.1, upper = 10)
    inf.sens = control_parameter(control, "ic.info_sensitivity", 0.5)
    points(res$eps05, M[max(which(M > inf.sens * res$M0))],
      pch = half_partial.pch, cex = half_partial.cex, col = half_partial.pch_col)
    half_partial.lty_v = control_parameter(control, "ic.plot.half_partial.lty_v", 3L)
    checkLty(half_partial.lty_v)
    half_partial.line_col_v =
      control_parameter(control, "ic.plot.half_partial.line_col_v", "darkgreen")
    half_partial.lwd_v = control_parameter(control, "ic.plot.half_partial.lwd_v", 1L)
    assertNumber(half_partial.lwd_v, lower = 0.1, upper = 10)
    abline(v = res$eps05, lty = half_partial.lty_v, lwd = half_partial.lwd_v,
      col = half_partial.line_col_v)
    half_partial.lty_h = control_parameter(control, "ic.plot.half_partial.lty_h", 3L)
    checkLty(half_partial.lty_h)
    half_partial.line_col_h =
      control_parameter(control, "ic.plot.half_partial.line_col_h", "darkgreen")
    half_partial.lwd_h = control_parameter(control, "ic.plot.half_partial.lwd_h", 1L)
    assertNumber(half_partial.lwd_h, lower = 0.1, upper = 10)
    abline(h = M[max(which(M > inf.sens * res$M0))], lty = half_partial.lty_h,
      lwd = half_partial.lwd_v, col = half_partial.line_col_v)
    half_partial.text_cex =
      control_parameter(control, "ic.plot.half_partial.text_cex", 0.8)
    half_partial.text_col =
      control_parameter(control, "ic.plot.half_partial.text_col", "darkgreen")
    assertNumber(half_partial.cex, lower = 0.1, upper = 10)
    text(log10(min(epsilon[epsilon > 0])), res$M0 * inf.sens - 0.05 * diff(ic.ylim),
      sprintf("%.3f * M0", inf.sens), cex = half_partial.text_cex, pos = 4,
      col = half_partial.text_col)
  }

  # axis labels
  xlab = control_parameter(control, "ic.plot.xlab", expression(log[10](epsilon)))
  xlab_line = control_parameter(control, "ic.plot.xlab_line", 3)
  assertNumber(xlab_line)
  mtext(text = xlab, side = 1, line = xlab_line)
  ylab = control_parameter(control, "ic.plot.ylab", ylab)
  ylab_line = control_parameter(control, "ic.plot.ylab_line", 2.5)
  assertNumber(ylab_line)
  mtext(text = ylab, side = 2, line = ylab_line)

  # legend properties
  if (partial.ic) {
    legend.descr = control_parameter(control, "ic.plot.legend_descr",
      c(expression(H(epsilon)), expression(M(epsilon)), expression(H[max]),
        expression(epsilon[s]), expression(M[0]), expression(epsilon[ratio])))
    legend.points = control_parameter(control, "ic.plot.legend_points",
      c(NA_integer_, NA_integer_, max_ic.pch, settl.pch, partial_ic.pch, half_partial.pch))
    legend.lines = control_parameter(control, "ic.plot.legend_lines",
      c(ic.lty, partial_ic.lty, NA_integer_, NA_integer_, NA_integer_, NA_integer_))
    legend.col = c(ic.line_col, partial_ic.line_col, max_ic.pch_col, settl.col,
      partial_ic.pch_col, half_partial.pch_col)
  } else {
    legend.descr = control_parameter(control, "ic.plot.legend_descr",
      c(expression(H(epsilon)), expression(H[max]), expression(epsilon[s])))
    legend.points = control_parameter(control, "ic.plot.legend_points",
      c(NA_integer_, max_ic.pch, settl.pch))
    legend.lines = control_parameter(control, "ic.plot.legend_lines",
      c(ic.lty, NA_integer_, NA_integer_))
    legend.col = c(ic.line_col, max_ic.pch_col, settl.col)
  }
  legend.location = control_parameter(control, "ic.plot.legend_location", "topright")

  # draw legend
  if (length(legend.location) == 1L) {
    assertChoice(legend.location, choices = c("top", "topright", "topleft",
      "center", "bottom", "bottomright", "bottomleft", "left", "right"))
    legend(legend.location, legend = legend.descr, pch = legend.points,
      lty = legend.lines, col = legend.col)
  } else {
    assertNumeric(legend.location, len = 2L)
    legend(x = legend.location[1L], y = legend.location[2L],
      legend = legend.descr, pch = legend.points, lty = legend.lines, col = legend.col)
  }
}

checkLty = function(arg) {
  if (is.character(arg)) {
    assertChoice(x = arg, choices = c("blank", "solid", "dashed", "dotted",
      "dotdash", "longdash", "twodash"))
  } else {
    assertIntegerish(arg, lower = 0L, upper = 6L)
  }
}

checkPch = function(arg) {
  if (is.character(arg)) {
    assertCharacter(arg)
  } else {
    assertIntegerish(arg, lower = 0L, upper = 25L)
  }
}
