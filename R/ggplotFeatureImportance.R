#' @rdname featureImportance
#' @export
ggplotFeatureImportance = function(featureList, control = list(), ...) {
  if (!is.list(featureList))
    stop("featureList has to be a list")
  perc.high = control_parameter(control, "featimp.perc_high", 0.8)
  perc.low = control_parameter(control, "featimp.perc_low", 0.2)
  lab.feat = control_parameter(control, "featimp.lab_feat", NULL)
  lab.resample = control_parameter(control, "featimp.lab_resample", "CV-Iteration")
  col.high = control_parameter(control, "featimp.col_high", "red")
  col.med = control_parameter(control, "featimp.col_medium", "orange")
  col.low = control_parameter(control, "featimp.col_low", "black")
  col.inactive = control_parameter(control, "featimp.col_inactive", "grey")
  lab.title = control_parameter(control, "featimp.lab_title", NULL)
  lab.strip = control_parameter(control, "featimp.lab_strip", NULL)
  legend.position = control_parameter(control, "featimp.legend_position", "bottom")
  flip.axes = control_parameter(control, "featimp.flip_axes", FALSE)
  plot.tiles = control_parameter(control, "featimp.plot_tiles", FALSE)
  iters = seq_along(featureList)
  tab = table(unlist(featureList))
  tab = sort(tab, decreasing = FALSE)
  ft.names = names(tab)

  ## label for the ticks along the folds-axis
  lab.iter = as.character(iters)
  names(lab.iter) = lab.iter

  ## create all pairwise combinations of features and resampling folds
  df = as.data.frame(expand.grid(iters = iters, feats = ft.names))
  df$feats = factor(as.character(df$feats), levels = ft.names)

  ## add a column for the feature importance category
  df$cat = "medium"
  df$cat[df$feats %in% names(which(tab >= max(iters) * perc.high))] = "high"
  df$cat[df$feats %in% names(which(tab < max(iters) * perc.low))] = "low"

  ## which combinations of feature and fold do not exist?
  for (i in iters) {
    fts = featureList[[i]]
    index = which((df$iters == i) & !(df$feats %in% fts))
    df$cat[index] = "inactive"
  }
  df$cat = factor(df$cat, levels = c("high", "medium", "low", "inactive"))

  ## create color and symbol size vectors (only for existing feature importance categories)
  col = c(col.high, col.med, col.low, col.inactive)[levels(df$cat) %in% unique(as.character(df$cat))]
  pch.size = c(4, 4, 4, 2.5)[levels(df$cat) %in% unique(as.character(df$cat))]

  ## extend feature importance categories by their percentage values
  levels(df$cat) = c(sprintf("high\n(>= %.0f%%)", perc.high * 100), sprintf("medium\n(%.0f-%.0f%%)", perc.low * 100, perc.high * 100), sprintf("low\n(< %.0f%%)", perc.low * 100), "inactive")

  ## add strip.text for facet labels
  if (!is.null(lab.strip)) {
    df$lab.strip = lab.strip
  }

  ## if axes should be flipped, the order of the y-values will also be adjusted
  if (flip.axes) {
    df$feats = factor(as.character(df$feats), levels = rev(ft.names))
  }

  if (plot.tiles) {
    g = ggplot2::ggplot() +
      ggplot2::geom_tile(data = df,
        mapping = ggplot2::aes_string(x = "iters", y = "feats", fill = "cat"), color = "black") +
      ggplot2::scale_fill_manual(values = col) +
      ggplot2::guides(
        fill = ggplot2::guide_legend("Feature\nImportance"))
  } else {
    g = ggplot2::ggplot() +
      ggplot2::geom_point(data = df, mapping = ggplot2::aes_string(x = "iters", y = "feats"), color = eval(col.inactive)) +
      ggplot2::geom_point(data = df, mapping = ggplot2::aes_string(x = "iters", y = "feats", size = "cat", color = "cat")) +
      ggplot2::scale_size_manual(values = pch.size) +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::guides(
        size = ggplot2::guide_legend("none"),
        color = ggplot2::guide_legend("Feature\nImportance", override.aes = list(size = pch.size)))
  }

  g = g +
    ggplot2::scale_x_discrete(breaks = lab.iter, limits = lab.iter) +
    ggplot2::labs(y = lab.feat, x = lab.resample, title = lab.title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(size = 0.5),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
    )

  if (flip.axes) {
    g = g + ggplot2::coord_flip()
  }

  if (legend.position == "bottom") {
    g = g + ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1L)) +
      ggplot2::theme(
        legend.position = "bottom"
    )
  }
  if (!is.null(lab.strip)) {
    g = g + ggplot2::facet_wrap( ~ lab.strip)
  }
  
  return(g)
}
