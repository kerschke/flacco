context("Plot: Barrier Tree 2D")

test_that("Default Cell Mapping-Plot", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -1000, 1000))
  f = function(x) {x[1]^4 + 1000*(x[1] - 3)^3 + 1000 * x[1] + x[2]}
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y,
    lower = -1000, upper = 1000, blocks = 5)

  # this can only happen if no error has forced the execution to stop
  plotBarrierTree2D(feat.object)
  expect_true(TRUE) 
})

test_that("Vary the approaches", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -32, 32))
  f = smoof::makeHosakiFunction()
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -32, upper = 32, blocks = c(5, 8))

  # this can only happen if no error has forced the execution to stop
  plotBarrierTree2D(feat.object, control = list(gcm.approach = "min"))
  plotBarrierTree2D(feat.object, control = list(gcm.approach = "near"))
  plotBarrierTree2D(feat.object, control = list(gcm.approach = "mean"))
  expect_true(TRUE) 
})

test_that("Vary plot arguments", {
  set.seed(2015 * 03 * 25)
  X = replicate(2, runif(1000, -32, 32))
  f = smoof::makeHosakiFunction()
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -32, upper = 32, blocks = c(4, 6))

  plotBarrierTree2D(feat.object,
    control = list(
      gcm.approach = "near",
      bt.margin = c(6, 6, 5, 4.5),
      bt.cm_surface = FALSE,
      bt.color_branches = "#654321",
      bt.label.x_coord = "Dim 1",
      bt.label.y_coord = "Dim 2",
      bt.label.x_id = "Dim 3",
      bt.label.x_id = "Dim 4"
    )
  )

  expect_true(TRUE) 
})

test_that("Error when BT features can't be computed", {
  ## (1) complete plateau
  set.seed(2015*03*26)
  X = expand.grid(1:3, 1:2)
  y = rep(2, 6)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(3, 2),
    lower = c(0.5, 0.5), upper = c(3.5, 2.5))
  expect_error(plotBarrierTree2D(feat.object))

  ## (2) too many dimensions
  set.seed(2015*03*26)
  X = createInitialSample(n.obs = 100, dim = 3)
  y = rnorm(100)
  feat.object = createFeatureObject(X = X, y = y, blocks = c(3, 2, 4))
  expect_error(plotBarrierTree2D(feat.object))
})
