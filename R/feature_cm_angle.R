calculateAngleFeatures = function(feat.object, control) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    show.warnings = control_parameter(control, "cm_angle.show_warnings", FALSE)
    init.grid = feat.object$init.grid
    ft.names = feat.object$feature.names
    cell.centers = as.matrix(feat.object$cell.centers)[, ft.names]
    obj = feat.object$objective.name
    if (!feat.object$minimize)
      init.grid[, obj] = -1 * init.grid[, obj]
    grid.best = as.matrix(plyr::ddply(init.grid, "cell.ID", function(x) x[selectMin(x[, obj]), ]))
    grid.worst = as.matrix(plyr::ddply(init.grid, "cell.ID", function(x) x[selectMax(x[, obj]), ]))
    y.global.worst = max(grid.worst[, obj], na.rm = TRUE)
    y.global.best = min(grid.best[, obj], na.rm = TRUE)
    non_empty = sort.int(unique(grid.best[, "cell.ID"]), method = "quick")
    no_total = as.integer(prod(feat.object$blocks))
    no_empty = as.integer(no_total - length(non_empty))
    if (no_total == 1) {
      cell.centers = t(cell.centers)
    }
    ## only consider non-empty cells
    cell.values = vapply(seq_len(nrow(grid.worst)), function(i) {
      x.center = as.numeric(cell.centers[i, ft.names])
      x.worst = as.numeric(grid.worst[i, ft.names])
      x.best = as.numeric(grid.best[i, ft.names])
      y.local.worst = as.numeric(grid.worst[i, obj])
      y.local.best = as.numeric(grid.best[i, obj])
      b2w.ratio = (y.local.worst - y.local.best) / 
      (y.global.worst - y.global.best)
      c2b.vect = x.best - x.center
      c2b.dist = as.numeric(sqrt(crossprod(as.numeric(c2b.vect))))
      c2w.vect = x.worst - x.center
      c2w.dist = as.numeric(sqrt(crossprod(as.numeric(c2w.vect))))
      x = as.numeric(crossprod(c2b.vect, c2w.vect)) / (c2b.dist * c2w.dist)
      if (all(x.worst == x.best)) {
        angle = 0
      } else {
        angle = acos(x) * 180 / pi
      }      
      return(c(c2b.dist = c2b.dist, c2w.dist = c2w.dist, 
        angle = angle, b2w.ratio = b2w.ratio))
    }, double(4))
    if (show.warnings && (no_empty > 0L)) {
      warningf("%.2f%% of the cells produce NAs during the feature computation.", 
        100 * no_empty / no_total)
    }
    return(list(cm_angle.dist_ctr2best.mean = mean(cell.values["c2b.dist", ]),
      cm_angle.dist_ctr2best.sd = sd(cell.values["c2b.dist", ]),
      cm_angle.dist_ctr2worst.mean = mean(cell.values["c2w.dist", ]),
      cm_angle.dist_ctr2worst.sd = sd(cell.values["c2w.dist", ]),
      cm_angle.angle.mean = mean(cell.values["angle", ]),
      cm_angle.angle.sd = sd(cell.values["angle", ]),
      cm_angle.y_ratio_best2worst.mean = mean(cell.values["b2w.ratio", ]),
      cm_angle.y_ratio_best2worst.sd = sd(cell.values["b2w.ratio", ])))
  }), "cm_angle")
}
