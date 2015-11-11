#' @title Feature Importance Plot
#'
#' @description
#' Creates a feature importance plot.
#'
#' @param featureList [\code{list}]\cr
#'   List of vectors of features. One list element is expected to belong to
#'   one resampling iteration / fold.
#' @param control [\code{list}]\cr
#'   A list, which stores additional configuration parameters:
#'   \itemize{
#'     \item{\code{featimp.col_{high/medium/low}}}: Color of the features, which
#'     are used often, sometimes or only a few times.
#'     \item{\code{featimp.perc_{high/low}}}: Percentage of the total number of folds,
#'     defining when a features, is used often, sometimes or only a few times.
#'     \item{\code{featimp.las}}: Alignment of axis labels.
#'     \item{\code{featimp.{xlab/ylab}}}: Axis labels.
#'     \item{\code{featimp.string_angle}}: Angle for the features on the x-axis.
#'     \item{\code{featimp.pch_{active/inactive}}}: Plot symbol of the active and
#'     inactive points.
#'     \item{\code{featimp.col_inactive}}: Color of the inactive points.
#'     \item{\code{featimp.col_vertical}}: Color of the vertical lines.
#'    }
#' @param ... [any]\cr
#'   Further arguments, which can be passed to \code{plot}.
#' @return [\code{plot}].\cr
#'   Feature Importance Plot, indicating which feature was used during which iteration.
#' @examples
#' \dontrun{
#' # At the beginning, one needs a list of features, e.g. derived during a
#' # nested feature selection within mlr (see the following 8 steps):
#' library(mlr)
#' library(mlbench)
#' data(Glass)
#'
#' # (1) Create a classification task:
#' classifTask = makeClassifTask(data = Glass, target = "Type")
#'
#' # (2) Define the model (here, a classification tree):
#' lrn = makeLearner(cl = "classif.rpart")
#'
#' # (3) Define the resampling strategy, which is supposed to be used within 
#' # each inner loop of the nested feature selection:
#' innerResampling = makeResampleDesc("Holdout")
#'
#' # (4) What kind of feature selection approach should be used? Here, we use a
#' # sequential backward strategy, i.e. starting from a model with all features,
#' # in each step the feature decreasing the performance measure the least is
#' # removed from the model:
#' ctrl = makeFeatSelControlSequential(method = "sbs")
#'
#' # (5) Wrap the original model (see (2)) in order to allow feature selection:
#' wrappedLearner = makeFeatSelWrapper(learner = lrn,
#'   resampling = innerResampling, control = ctrl)
#'
#' # (6) Define a resampling strategy for the outer loop. This is necessary in
#' # order to assess whether the selected features depend on the underlying
#' # fold:
#' outerResampling = makeResampleDesc(method = "CV", iters = 10L)
#'
#' # (7) Perform the feature selection:
#' featselResult = resample(learner = wrappedLearner, task = classifTask,
#'   resampling = outerResampling, models = TRUE)
#'
#' # (8) Extract the features, which were selected during each iteration of the
#' # outer loop (i.e. during each of the 5 folds of the cross-validation):
#' featureList = lapply(featselResult$models, 
#'   function(mod) getFeatSelResult(mod)$x)}
#'
#' ########################################################################
#' 
#' # Now, one could inspect the features manually:
#' featureList
#'
#' # Alternatively, one might use visual means such as the feature
#' # importance plot:
#' plotFeatureImportance(featureList)
#' @export
plotFeatureImportance = function(featureList, control = list(), ...) {
  if (!is.list(featureList))
    stop("featureList has to be a list")
  perc.high = control_parameter(control, "featimp.perc_high", 0.8)
  perc.low = control_parameter(control, "featimp.perc_low", 0.2)
  srt = control_parameter(control, "featimp.string_angle", 45)
  xlab = control_parameter(control, "featimp.xlab", "")
  ylab = control_parameter(control, "featimp.ylab", "CV-Iteration")
  las = control_parameter(control, "featimp.las", 1)
  col.high = control_parameter(control, "featimp.col_high", "red")
  col.med = control_parameter(control, "featimp.col_medium", "orange")
  col.low = control_parameter(control, "featimp.col_low", "black")
  col.inactive = control_parameter(control, "featimp.col_inactive", "grey")
  col.vertical = control_parameter(control, "featimp.col_vertical", "grey")
  pch.active = control_parameter(control, "featimp.pch_active", 19)
  pch.inactive = control_parameter(control, "featimp.pch_inactive", 20)
  iters = seq_along(featureList)
  tab = table(unlist(featureList))
  tab = sort(tab, decreasing = TRUE)
  ft.names = names(tab)
  x = seq_along(ft.names)
  if (length(x) < 5) {
    xlim = c(min(x) - 0.5, max(x) + 0.5)
  } else {
    xlim = range(x)
  }
  colors = ifelse(tab >= max(iters) * perc.high, col.high, 
    ifelse(tab >= max(iters) * perc.low, col.med, col.low))
  plot(-99, -99, xlab = xlab, ylab = ylab, las = las,
    xlim = xlim, ylim = range(iters), yaxt = "n", xaxt = "n", ...)
  axis(side = 2, at = min(iters):max(iters), las = las)
  text(x, par("usr")[3], labels = ft.names, srt = srt,
    adj = c(1.1,1.1), xpd = TRUE)
  abline(v = x, col = col.vertical)
  for(i in x) {
    points(rep(i, length(iters)), iters, pch = pch.inactive, col = col.inactive)
    y = vapply(featureList, function(feats) ft.names[i] %in% feats, logical(1))
    points(rep(i, sum(y)), iters[y], pch = pch.active, col = colors[i])
  }
}
