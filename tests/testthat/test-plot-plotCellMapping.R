context("Plot: Cell Mapping Plot")

test_that("Default Cell Mapping-Plot", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  y = apply(X, 1, function(x) {x[1]^4 + 1000*(x[1]-3)^3 + 1000*x[1] + x[2]})
  feat.object = createFeatureObject(X = X, y = y,
    lower = -1000, upper = 1000, blocks = 5)  

  # this can only happen if no error has forced the execution to stop
  plotCellMapping(feat.object)
  expect_true(TRUE) 
})

test_that("Vary the approaches", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -32, 32)))
  f = smoof::makeHosakiFunction()
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -32, upper = 32, blocks = c(5, 8))
  
  # this can only happen if no error has forced the execution to stop
  plotCellMapping(feat.object, control = list(gcm.approach = "min"))
  plotCellMapping(feat.object, control = list(gcm.approach = "near"))
  plotCellMapping(feat.object, control = list(gcm.approach = "mean"))
  expect_true(TRUE) 
})

test_that("Vary plot arguments", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -32, 32)))
  f = smoof::makeHosakiFunction()
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -32, upper = 32, blocks = c(4, 6))

  plotCellMapping(feat.object,
    control = list(
      gcm.approach = "near",
      gcm.margin = c(6, 6, 5, 4.5),
      gcm.color_attractor = "#a1b2c3",
      gcm.color_uncertain = "#654321",
      gcm.color_grid = "#ff1155",
      gcm.color_basin = function(n) terrain.colors(n),
      gcm.arrow.length_x = 0.95,
      gcm.arrow.length_y = 0.85,
      gcm.arrowhead.length = 0.05,
      gcm.arrowhead.width = 0.15,
      gcm.arrowhead.type = "curved",
      gcm.label.x_coord = "Dim 1",
      gcm.label.y_coord = "Dim 2",
      gcm.label.x_id = "Dim 3",
      gcm.label.x_id = "Dim 4"
    )
  )

  # turn off labels and arrows
  plotCellMapping(feat.object,
    control = list(
      gcm.approach = "near",
      gcm.plot_coord_labels = FALSE,
      gcm.plot_id_labels = FALSE,
      gcm.plot_arrows = FALSE
    )
  )
  expect_true(TRUE) 
})
