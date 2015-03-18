# library(devtools)
# load_all("~/Documents/repos/ela/")
# 
# # Teste cm-features:
# library(mlbench)
# data(BostonHousing)
# mydata = BostonHousing[, c(1, 2, 7:14)]
# blu = createFeatureObject(init=mydata[, -(6:9)], blocks = 6, objective="medv")
# blu
# 
# blu2 = createFeatureObject(init=mydata[, c(1, 2, 10)], blocks = 6, objective="medv")
# a1 = calculateAngle(blu2)
# a2 = calculateCellConvexity(blu2)
# a3 = calculateGradientHomogeneity(blu2)
# a4 = calculateLinModCoefficients(blu2)
# 
# X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
# blu3 = createFeatureObject(X = X, fun = function(x) sum(x^2))
# a5 = calculateAngle(blu3)
# a6 = calculateConvexity(blu3)
# a7 = calculateCurvature(blu3)
# a8 = calculateDistribution(blu3)
# a9 = calculateLevelset(blu3)
# a10 = calculateLocalSearch(blu3)
# a11 = calculateMetaModel(blu3)
# a12 = calculateNearestBetter(blu3)
# a13 = calculatePCA(blu3)
# a14 = calculateDispersion(blu3)
# 
# ### FIXME: Folgender Aufruf wirft Probleme:
# a15 = calculateGCMFeatures(blu3)
# 
# # X = t(replicate(n = 2000, expr = runif(n = 2, min = -10, max = 10)))
# # ## funktioniert nicht, wenn 
# # blu4 = createFeatureObject(X = X, fun = function(x) sum(x^2), blocks = c(15, 15))
# # fun = function(x) sum(x^2)
# # a14 = calculateGCMFeatures(f = fun, x = X, y = apply(X, 1, fun))
# # a14 = calculateGCMFeatures(blu4)
# 
# 
