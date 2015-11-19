context("Plot: Barrier Tree 3D")

test_that("Default BarrierTree-Plot", {
  set.seed(2015*03*25)
  X = replicate(2, runif(1000, -1000, 1000))
  f = function(x) {x[1]^4 + 1000 * (x[1] - 3)^3 + 1000 * x[1] + x[2]}
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000, blocks = 5)  
  plotBarrierTree3D(feat.object)

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})

test_that("Vary Block Sizes", {
  X = createInitialSample(n.obs = 900, dim = 2)
  f = smoof::makeAckleyFunction(dimensions = 2)
  y = apply(X, 1, f)

  feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = c(4, 6))
  plotBarrierTree3D(feat.object)

  feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = c(8, 3))
  plotBarrierTree3D(feat.object)

  feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = c(10, 10))
  plotBarrierTree3D(feat.object)

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})

test_that("Changing Control Arguments", {
  set.seed(2015*03*25)
  X = t(replicate(1000, runif(2, -1000, 1000)))
  f = smoof::makeAckleyFunction(dimensions = 2)
  y = apply(X, 1, f)
  feat.object = createFeatureObject(X = X, y = y, 
    lower = -1000, upper = 1000, blocks = c(10, 10))  

  plotBarrierTree3D(feat.object,
    control = list(
      gcm.approach = "near", gcm.cf_power = 512L,
      bt.color_surface = "hotpink",
      bt.color_branches = terrain.colors(10L),
      bt.persp_border = "orange",
      bt.persp_theta = 250,
      bt.persp_phi = 33,
      bt.persp_ticktype = "simple",
      bt.pch_root = 2,
      bt.pch_breakpoint = 3,
      bt.pch_basin = 4,
      bt.lwd = 3)
  )

  # this can only happen if no error has forced the execution to stop
  expect_true(TRUE) 
})
