# set.seed(123)
# library(soobench)
# dim = 2
# f = generate_bbob2009_function(dim, 15, 8)
# X = replicate(n = 200 * dim, expr = runif(dim, lower_bounds(f), upper_bounds(f)))
# y = apply(X, 2, f)
# 
# source("Documents/repos/ela/R/utilities.R")
# source("Documents/repos/ela/R/feature_convexity.R")
# calculateConvexity(f, X, y)
