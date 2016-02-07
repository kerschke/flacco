#' @title Calculate Landscape Features
#'
#' @description
#'   Performs an Exploratory Landscape Analysis of a continuous function and
#'   computes various features, which quantify the function's landscape.
#'   Currently, the following feature sets are provided:
#'   \itemize{
#'     \item{CM}: cell mapping features (\code{"cm_angle"}, \code{"cm_conv"},
#'     \code{"cm_grad"})
#'     \item{ELA}: classical ELA features (\code{"ela_conv"},
#'     \code{"ela_curv"}, \code{"ela_distr"}, \code{"ela_level"},
#'     \code{"ela_local"}, \code{"ela_meta"})
#'     \item{GCM}: general cell mapping features (\code{"gcm"})
#'     \item{BT}: barrier tree features (\code{"bt"})
#'     \item{IC}: information content features (\code{"ic"})
#'     \item{Basic}: basic features (\code{"basic"})
#'     \item{Disp}: dispersion features (\code{"disp"})
#'     \item{LiMo}: linear model features (\code{"limo"})
#'     \item{NBC}: nearest better clustering features (\code{"nbc"})
#'     \item{PC}: principal component features (\code{"pca"})
#'   }
#'
#' @template arg_feat_object
#' @template arg_control
#' @param ... [any]\cr
#'   Further arguments, e.g. handled by \code{\link{optim}} (within the
#'   computation of the ELA local search features) or \code{\link{density}}
#'   (within the computation of the ELA y-distribution features).
#' @param set [\code{character(1)}]\cr
#'   Name of the feature set, which should be computed. All possible feature
#'   sets can be listed using \code{\link{listAvailableFeatureSets}}.
#'
#' @return \code{list} of (\code{numeric}) features:
#'   \itemize{
#'     \item{\code{cm_angle} -- angle features (10)}:\cr
#'     These features are based on the location of the worst and best element
#'     within each cell. To be precise, their distance to the cell center and
#'     the angle between these three elements (at the center) are the
#'     foundation:
#'     \itemize{
#'       \item{\code{dist_ctr2{best, worst}.{mean, sd}}}: arithmetic mean and
#'       standard deviation of distances from the cell center to the best /
#'       worst observation within the cell (over all cells)
#'       \item{\code{angle.{mean, sd}}}: arithmetic mean and standard deviation
#'       of angles (in degree) between worst, center and best element of a cell
#'       (over all cells)
#'       \item{\code{y_ratio_best2worst.{mean, sd}}}: arithmetic mean and
#'       standard deviation of the ratios between the distance of the worst and
#'       best element within a cell and the worst and best element in the
#'       entire initial design (over all cells);\cr
#'       note that the distances are only measured in the objective space
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{cm_conv} -- cell mapping convexity features (6)}:\cr
#'     Each cell will be represented by an observation (of the initial design),
#'     which is located closest to the cell center. Then, the objectives of three
#'     neighbouring cells are compared:\cr
#'     \itemize{
#'       \item{\code{{convex, concave}.hard}}: if the objective of the inner
#'       cell is above / below the two outer cells, there is strong evidence
#'       for convexity / concavity
#'       \item{\code{{convex, concave}.soft}}: if the objective of the inner
#'       cell is above / below the arithmetic mean of the two outer cells,
#'       there is weak evidence for convexity / concavity
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{cm_grad} -- gradient homogeneity features (4)}:\cr
#'     Within a cell of the initial grid, the gradients between each
#'     observation and its nearest neighbour observation are computed. Those
#'     gradients are then directed towards the smaller of the two objective
#'     values and afterwards normalized. Then, the length of the sum of all the
#'     directed and normalized gradients within a cell is computed. Based on
#'     those measurements (one per cell) the following features are computed:\cr
#'     \itemize{
#'       \item{\code{{mean, sd}}}: arithmetic mean and standard deviation of
#'       the aforementioned lengths
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ela_conv} -- ELA convexity features (6)}:\cr
#'     Two observations are chosen randomly from the initial design. Then, a
#'     linear (convex) combination of those observations is calculated -- based
#'     on a random weight from [0, 1]. The corresponding objective value will be
#'     compared to the linear combination of the objectives from the two
#'     original observations. This process is replicated \code{convex.nsample}
#'     (per default \code{1000}) times and will then be aggregated:\cr
#'     \itemize{
#'       \item{\code{{convex_p, linear_p}}}: percentage of convexity / linearity
#'       \item{\code{linear_dev.{orig, abs}}}: average (original / absolute)
#'       deviation between the linear combination of the objectives and the
#'       objective of the linear combination of the observations
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ela_curv} -- ELA curvature features (26)}:\cr
#'     Given a feature object, \code{curv.sample_size} samples (per default
#'     \code{100 * d} with \code{d} being the number of features) are randomly
#'     chosen. Then, the gradient and hessian of the function are estimated
#'     based on those points and the following features are computed:\cr
#'     \itemize{
#'       \item{\code{grad_norm.{min, lq, mean, median, uq, max, sd, nas}}}:
#'       aggregations (minimum, lower quartile, arithmetic mean, median, upper
#'       quartile, maximum, standard deviation and percentage of NAs) of the
#'       gradients' lengths
#'       \item{\code{grad_scale.{min, lq, mean, median, uq, max, sd, nas}}}:
#'       aggregations of the ratios between biggest and smallest (absolute)
#'       gradient directions
#'       \item{\code{hessian_cond.{min, lq, mean, median, uq, max, sd, nas}}}:
#'       aggregations of the ratios of biggest and smallest eigenvalue of the
#'       hessian matrices
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ela_distr} -- ELA y-distribution features (5)}:\cr
#'     \itemize{
#'       \item{\code{skewness}}: skewness of the objective values
#'       \item{\code{kurtosis}}: kurtosis of the objective values
#'       \item{\code{number_of_peaks}}: number of peaks based on an estimation
#'       of the density of the objective values
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ela_level} -- ELA levelset features (20)}:\cr
#'     \itemize{
#'       \item{\code{mmce_{methods}_{quantiles}}}: mean misclassification error
#'       of each pair of classification method and quantile
#'       \item{\code{{method1}_{method2}_{quantiles}}}: ratio of all pairs of
#'       classification methods for all quantiles
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ela_local} -- ELA local search features (15)}:\cr
#'     Based on some randomly chosen points from the initial design, a
#'     pre-defined number of local searches (\code{ela_local.local_searches})
#'     are executed. Their optima are then clustered (using hierarchical
#'     clustering), assuming that local optima that are located close to each
#'     other, likely belong to the same basin. Given those basins, the
#'     following features are computed:\cr
#'     \itemize{
#'       \item{\code{n_loc_opt.{abs, rel}}}: the absolute / relative amount of
#'       local optima
#'       \item{\code{best2mean_contr.orig}}: each cluster is represented by its
#'       center; this feature is the ratio of the objective values of the best
#'       and average cluster
#'       \item{\code{best2mean_contr.ratio}}: each cluster is represented by its
#'       center; this feature is the ratio of the differences in the objective
#'       values of average to best and worst to best cluster
#'       \item{\code{basin_sizes.avg_{best, non_best, worst}}}: average basin
#'       size of the best / non-best / worst cluster(s)
#'       \item{\code{fun_evals.{min, lq, mean, median, uq, max, sd}}}:
#'       aggregations of the performed local searches
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ela_meta} -- ELA meta model features (11)}:\cr
#'     Given an initial design, linear and quadratic models of the form
#'     \code{objective ~ features} are created. Both versions are created
#'     with and without simple interactions (e.g., \code{x1:x2}). Based on
#'     those models, the following features are computed:\cr
#'     \itemize{
#'       \item{\code{lin_simple.{adj_r2, intercept}}}: adjusted R^2 (i.e. model
#'       fit) and intercept of a simple linear model
#'       \item{\code{lin_simple.coef.{min, max, max_by_min}}}: smallest and
#'       biggest (non-intercept) absolute coefficients of the simple linear
#'       model, and their ratio
#'       \item{\code{{lin_w_interact, quad_simple, quad_w_interact}.adj_r2}}:
#'       adjusted R^2 (i.e. the model fit) of a linear model with interactions,
#'       and a quadratic model with and without interactions
#'       \item{\code{quad_simple.cond}}: condition of a simple quadratic model
#'       (without interactions), i.e. the ratio of its (absolute) biggest and
#'       smallest coefficients
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{gcm} -- general cell mapping (GCM) features (75)}:\cr
#'     Computes general cell mapping features based on the Generalized Cell
#'     Mapping (GCM) approach, which interpretes the cells as absorbing Markov
#'     chains. Computations are performed based on three different approaches:
#'     taking the best (\code{min}) or average (\code{mean}) objective value of
#'     a cell or the closest observation (\code{near}) to a cell as
#'     representative. For each of these approaches the following 25 features
#'     are computed:\cr
#'     \itemize{
#'       \item{\code{attractors, pcells, tcells, uncertain}}: relative amount
#'       of attractor, periodic, transient and uncertain cells
#'       \item{\code{basin_prob.{min, mean, median, max, sd}}}: aggregations
#'       of the probabilities of each basin of attraction
#'       \item{\code{basin_certain.{min, mean, median, max, sd}}}: aggregations
#'       of the (relative) size of each basin of attraction, in case only
#'       certain cells are considered (i.e. cells, which only point towards one
#'       attractor)
#'       \item{\code{basin_uncertain.{min, mean, median, max, sd}}}:
#'       aggregations of the (relative) size of each basin of attraction, in
#'       case uncertain cells are considered (i.e. a cell, which points to
#'       multiple attractors contributes to each of its basins)
#'       \item{\code{best_attr.{prob, no}}}: probability of finding the
#'       attractor with the best objective value and the (relative) amount of
#'       those attractors (i.e. the ratio of the number of attractors with the
#'       best objective value and the total amount of cells)
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{bt} -- barrier tree features (90)}:\cr
#'     Computes barrier tree features on 2D (!) problems, based on a
#'     Generalized Cell Mapping (GCM) approach. Computations are performed
#'     based on three different approaches: taking the best (\code{min}) or
#'     average (\code{mean}) objective value of a cell or the closest
#'     observation (\code{near}) to a cell as representative. For each of these
#'     approaches the following 31 features are computed:\cr
#'     \itemize{
#'       \item{\code{levels}}: absolute number of levels of the barrier tree
#'       \item{\code{leaves}}: absolute number of leaves (i.e. local optima)
#'       of the barrier tree
#'       \item{\code{depth}}: range between highest and lowest node of the tree
#'       \item{\code{depth_levels_ratio}}: ratio of depth and levels
#'       \item{\code{levels_nodes_ratio}}: ratio of number of levels and number
#'       of (non-root) nodes of the tree
#'       \item{\code{diffs.{min, mean, median, max, sd}}}:
#'       aggregations of the height differences between a node and its
#'       predecessor
#'       \item{\code{level_diffs.{min, mean, median, max, sd}}}:
#'       aggregations of the average height differences per level
#'       \item{\code{attractor_dists.{min, mean, median, max, sd}}}:
#'       aggregations of the (euclidean) distances between the local and global
#'       best cells (attractors)
#'       \item{\code{basin_ratio.{uncertain, certain, most_likely}}}:
#'       ratios of maximum and minimum size of the basins of attractions; here,
#'       a cell might belong to different attractors (uncertain), exactly one
#'       attractor (certain) or the attractor with the highest probability
#'       \item{\code{basin_intersection.{min, mean, median, max, sd}}}:
#'       aggregations of the intersection between the basin of the global best
#'       value and the basins of all local best values
#'       \item{\code{basin_range}}:
#'       range of a basin (euclidean distance of widest range per dimension)
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{ic} -- information content features (7)}:\cr
#'     Computes features based on the Information Content of Fitness Sequences
#'     (ICoFiS) approach (cf. Munoz et al., 2015). In this approach, the
#'     information content of a continuous landscape, i.e. smoothness,
#'     ruggedness, or neutrality, are quantified. While common analysis methods
#'     were able to calculate the information content of discrete landscapes,
#'     the ICoFiS approach provides an adaptation to continuous landscapes that
#'     accounts e.g. for variable step sizes in random walk sampling:\cr
#'     \itemize{
#'      \item{\code{h.max}}: \dQuote{maximum information content} (entropy) of
#'      the fitness sequence, cf. equation (5)
#'      \item{\code{eps.s}}: \dQuote{settling sensitivity}, indicating the
#'      epsilon for which the sequence nearly consists of zeros only, cf.
#'      equation (6)
#'      \item{\code{eps.max}}: similar to \code{eps.s}, but in contrast to the
#'      former \code{eps.max} guarantees non-missing values
#'      \item{\code{eps.ratio}}: \dQuote{ratio of partial information
#'      sensitivity}, cf. equation (8), where the ratio is \code{0.5}
#'      \item{\code{m0}}: \dQuote{initial partial information}, cf. equation
#'      (7)
#'      \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'      function evaluations and runtime (in seconds), which were needed for
#'      the computation of these features
#'     }
#'     \item{\code{basic} -- basic features (16)}:\cr
#'     Very simple features, which can be read from the feature object (without
#'     any computational efforts):\cr
#'     \itemize{
#'       \item{\code{{dim, observations}}}: number of features / dimensions and
#'       observations within the initial sample
#'       \item{\code{{lower, upper, objective, blocks}_{min, max}}}: minimum
#'       and maximum value of all lower and upper bounds, the objective values
#'       and the number of blocks / cells (per dimension)
#'       \item{\code{cells_{filled, total}}}: number of filled (i.e. non-empty)
#'       cells and total number of cells
#'       \item{\code{{allows_cm, minimize_fun}}}: logical values, indicating
#'       whether this \code{\link{FeatureObject}} allows cell mapping and
#'       whether the optimization function should be minimized
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{disp} -- dispersion features (18)}:\cr
#'     Computes features based on the comparison of the dispersion of pairwise
#'     distances among the 'best' elements and the entire initial design:\cr
#'     \itemize{
#'       \item{\code{{ratio, diff}_{mean, median}_{02, 05, 10, 25}}}: ratio
#'       and difference of the mean / median distances of the distances of the
#'       'best' objectives vs. 'all' objectives
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{limo} -- linear model features (14)}:\cr
#'     Linear models are computed per cell, provided the decision space is
#'     divided into a grid of cells. Each one of the models has the form
#'     \code{objective ~ features}.\cr
#'     \itemize{
#'       \item{\code{avg_length.{reg, norm}}}: length of the average
#'       coefficient vector (based on regular and normalized vectors)
#'       \item{\code{length_{mean, sd}}}: arithmetic mean and standard
#'       deviation of the lengths of all coefficient vectors
#'       \item{\code{cor.{reg, norm}}}: correlation of all coefficient vectors
#'       (based on regular and normalized vectors)
#'       \item{\code{ratio_{mean, sd}}}: arithmetic mean and standard deviation
#'       of the ratios of (absolute) maximum and minimum (non-intercept)
#'       coefficients per cell
#'       \item{\code{sd_{ratio, mean}.{reg, norm}}}: max-by-min-ratio and
#'       arithmetic mean of the standard deviations of the (non-intercept)
#'       coefficients (based on regular and normalized vectors)
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{nbc} -- nearest better (clustering) features (7)}:\cr
#'     Computes features based on the comparison of nearest neighbour and
#'     nearest better neighbour, i.e., the nearest neighbor with a better
#'     performance / objective value value.\cr
#'     \itemize{
#'       \item{\code{nn_nb.{sd, mean}_ratio}}: ratio of standard deviations and
#'       arithmetic mean based on the distances among the nearest neighbours
#'       and the nearest better neighbours
#'       \item{\code{nn_nb.cor}}: correlation between distances of the nearest
#'       neighbours and the distances of the nearest better neighbours
#'       \item{\code{dist_ratio.coeff_var}}: coefficient of variation of the
#'       distance ratios
#'       \item{\code{nb_fitness.cor}}: correlation between fitness value and
#'       count of observations to whom the current observation is the nearest
#'       better neighbour (the so-called \dQuote{indegree}).
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'     \item{\code{pca} -- principal component (analysis) features (10)}:\cr
#'     \itemize{
#'       \item{\code{expl_var.{cov, cor}_{x, init}}}: proportion of the
#'       explained variance when applying PCA to the covariance / correlation
#'       matrix of the decision space (\code{x}) or the entire initial design
#'       (\code{init})
#'       \item{\code{expl_var_PC1.{cov, cor}_{x, init}}}: proportion of
#'       variance, which is explained by the first principal component -- when
#'       applying PCA to the covariance / correlation matrix of the decision
#'       space (\code{x}) or the entire initial design
#'       \item{\code{costs_{fun_evals, runtime}}}: number of (additional)
#'       function evaluations and runtime (in seconds), which were needed for
#'       the computation of these features
#'     }
#'   }
#'
#' @details
#'   Note that if you want to speed up the runtime of the features, you might
#'   consider running your feature computation parallelized. For more
#'   information, please refer to the \code{parallelMap} package or to
#'   \url{http://mlr-org.github.io/mlr-tutorial/release/html/parallelization/index.html}.
#'
#'   Furthermore, please consider adapting the feature computation to your
#'   needs. Possible \code{control} arguments are:
#'   \itemize{
#'     \item{general}: \itemize{
#'       \item{\code{show_progress}}: Show progress bar when computing the
#'       features? The default is \code{TRUE}.
#'       \item{\code{subset}}: Specify a subset of features that should be
#'       computed. Per default, all features will be computed.
#'       \item{\code{allow_cellmapping}}: Should cell mapping features be
#'       computed? The default is \code{TRUE}.
#'       \item{\code{allow_costs}}: Should expensive features, i.e. features,
#'       which require additional function evaluations, be computed? The
#'       default is \code{TRUE} if the feature object provides a function,
#'       otherwise \code{FALSE}.
#'       \item{\code{blacklist}}: Which features should NOT be computed? The
#'       default is \code{NULL}, i.e. none of the features will be excluded.
#'     }
#'     \item{cell mapping angle features}: \itemize{
#'       \item{\code{cm_angle.show_warnings}}: Should possible warnings about
#'       \code{NAs} in the feature computation be shown? The default is
#'       \code{FALSE}.
#'     }
#'     \item{cell mapping convexity features}: \itemize{
#'       \item{\code{cm_conv.diag}}: Should cells, which are located on the
#'       diagonal compared to the current cell, be considered as neighbouring
#'       cells? The default is \code{FALSE}, i.e. only cells along the axes
#'       are considered as neighbours.
#'       \item{\code{cm_conv.dist_method}}: Which distance method should be
#'       used for computing the distance between two observations? All methods
#'       of \code{\link{dist}} are possible options with \code{"euclidean"}
#'       being the default.
#'       \item{\code{cm_conv.minkowski_p}}: Value of \code{p} in case
#'       \code{dist_meth} is \code{"minkowski"}. The default is \code{2}, i.e.
#'       the euclidean distance.
#'       \item{\code{cm_conv.fast_k}}: Percentage of elements that should be
#'       considered within the nearest neighbour computation. The default is
#'       \code{0.05}.
#'     }
#'     \item{cell mapping gradient homogeneity features}: \itemize{
#'       \item{\code{cm_grad.dist_tie_breaker}}: How will ties be broken when
#'       different observations have the same distance to an observation?
#'       Possible values are \code{"sample"}, \code{"first"} and \code{"last"}.
#'       The default is \code{"sample"}.
#'       \item{\code{cm_grad.dist_method}}: Which distance method should be
#'       used for computing the distance between two observations? All methods
#'       of \code{\link{dist}} are possible options with \code{"euclidean"}
#'       being the default.
#'       \item{\code{cm_grad.minkowski_p}}: Value of \code{p} in case
#'       \code{dist_meth} is \code{"minkowski"}. The default is \code{2}, i.e.
#'       the euclidean distance.
#'       \item{\code{cm_grad.show_warnings}}: Should possible warnings about
#'       (almost) empty cells be shown? The default is \code{FALSE}.
#'     }
#'     \item{ELA convexity features}: \itemize{
#'       \item{\code{ela_conv.nsample}}: Number of samples that are drawn for
#'       calculating the convexity features. The default is \code{1000}.
#'       \item{\code{ela_conv.threshold}}: Threshold of the linearity, i.e. the
#'       tolerance to / deviation from perfect linearity, in order to still be
#'       considered linear. The default is \code{1e-10}.
#'     }
#'     \item{ELA curvature features}: \itemize{
#'       \item{\code{ela_curv.sample_size}}: Number of samples used for
#'       calculating the curvature features. The default is \code{100*d}.
#'       \item{\code{ela_curv.{delta, eps, zero_tol, r, v}}}: Parameters used
#'       by \code{\link[numDeriv]{grad}} and \code{\link[numDeriv]{hessian}} within the
#'       approximation of the gradient and hessian. The default values are
#'       identical to the ones from the corresponding functions.
#'     }
#'     \item{ELA distribution features}: \itemize{
#'       \item{\code{ela_distr.smoothing_bandwidth}}: The smoothing bandwidth,
#'       which should be used within the \code{\link{density}} estimation.
#'       The default is \code{"SJ"}.
#'       \item{\code{ela_distr.modemass_threshold}}: Threshold that is used in
#'       order to classify whether a minimum can be considered as a peak.
#'       The default is \code{0.01}.
#'       \item{\code{ela_distr.skewness_type}}: Algorithm type for computing
#'       the \code{\link[e1071]{skewness}}. The default is \code{3}.
#'       \item{\code{ela_distr.kurtosis_type}}: Algorithm type for computing
#'       the \code{\link[e1071]{kurtosis}}. The default is \code{3}.
#'     }
#'     \item{ELA levelset features}: \itemize{
#'       \item{\code{ela_level.quantiles}}: Cutpoints (quantiles of the
#'       objective values) for splitting the objective space. The default is
#'       \code{c(0.10, 0.25, 0.50)}.
#'       \item{\code{ela_level.classif_methods}}: Methods for classifying
#'       the artificially splitted objective space. The default is
#'       \code{c("lda", "qda", "mda")}.
#'       \item{\code{ela_level.resample_method}}: Resample technique for
#'       training the model, cf. \code{\link[mlr]{ResampleDesc}}. The default
#'       is \code{"CV"}.
#'       \item{\code{ela_level.resample_iterations}}: Number of iterations
#'       of the resampling method. The default is \code{10}.
#'       \item{\code{ela_level.resample_info}}: Should information regarding
#'       the resampling be printed? The default is \code{FALSE}.
#'       \item{\code{ela_level.parallelize}}: Should the levelset features be
#'       computed in parallel? The default is \code{FALSE}.
#'       \item{\code{ela_level.parallel.mode}}: Which mode should be used for
#'       the parallelized computation? Possible options are \code{"local"},
#'       \code{"multicore"}, \code{"socket"} (default), \code{"mpi"} and
#'       \code{"BatchJobs"}. Note that in case you are using a windows computer
#'       you can only use the \code{"socket"} mode.
#'       \item{\code{ela_level.parallel.cpus}}: On how many cpus do you want to
#'       compute the features in parallel? Per default, all available cpus are
#'       used.
#'       \item{\code{ela_level.parallel.level}}: On which level should the
#'       parallel computation be performed? The default is
#'       \code{"mlr.resample"}, i.e. the internal resampling (performed using
#'       \code{mlr}) will be done in parallel.
#'       \item{\code{ela_level.parallel.logging}}: Should slave output be
#'       logged? The default is \code{FALSE}.
#'       \item{\code{ela_level.parallel.show_info}}: Should verbose output of
#'       function calls be printed on the console? The default is \code{FALSE}.
#'     }
#'     \item{ELA local search features}: \itemize{
#'       \item{\code{ela_local.local_searches}}: Number of local searches. The
#'       default is \code{50 * d} with \code{d} being the number of features
#'       (i.e. the dimension).
#'       \item{\code{ela_local.optim_method}}: Local search algorithm. The
#'       default is \code{"L-BFGS-B"}.
#'       \item{\code{ela_local.optim.{lower, upper}}}: Lower and upper bounds
#'       to be considered by the local search algorithm. Per default, the
#'       boundaries are the same as defined within the feature object
#'       (in case of \code{"L-BFGS-B"}) or infinity (for all others).
#'       \item{\code{ela_local.optim_method_control}}: Control settings of the
#'       local search algorithm. The default is an empty list.
#'       \item{\code{ela_local.sample_seed}}: Seed, which will be set before
#'       the selection of the initial start points of the local search. The
#'       default is \code{sample(1:1e6, 1)}.
#'       \item{\code{ela_local.clust_method}}: Once the local searches
#'       converge, basins have to be assigned. This is done using hierarchical
#'       clustering methods from \code{\link{hclust}}. The default is
#'       \code{"single"}, i.e. \emph{single linkage clustering}.
#'       \item{\code{ela_local.clust_cut_function}}: A function of a
#'       hierarchical clustering \code{cl}, which defines at which height the
#'       dendrogramm should be splitted into clusters
#'       (cf. \code{\link{cutree}}). The default is
#'       \code{function(cl) as.numeric(quantile(cl$height, 0.1))}, i.e. the
#'       \code{10\%}-quantile of all the distances between clusters.
#'     }
#'     \item{GCM features}: \itemize{
#'       \item{\code{gcm.approaches}}: Which approach(es) should be used when
#'       computing the representatives of a cell. The default are all three
#'       approaches, i.e. \code{c("min", "mean", "near")}.
#'       \item{\code{gcm.cf_power}}: Theoretically, we need to compute the
#'       canonical form to the power of infinity. However, we use this value
#'       as approximation of infinity. The default is \code{256}.
#'     }
#'     \item{barrier tree features}: \itemize{
#'       \item{\code{gcm.approaches}}: Which approach(es) should be used when
#'       computing the representatives of a cell. The default are all three
#'       approaches, i.e. \code{c("min", "mean", "near")}.
#'       \item{\code{gcm.cf_power}}: Theoretically, we need to compute the
#'       canonical form to the power of infinity. However, we use this value
#'       as approximation of infinity. The default is \code{256}.
#'       \item{\code{bt.base}}: Maximum number of basins, which are joined at a
#'       single breakpoint. The default is \code{4L}.
#'       \item{\code{bt.max_depth}}: Maximum number of levels of the barrier
#'       tree. The default is \code{16L}.
#'     }
#'     \item{information content features}: \itemize{
#'       \item{\code{ic.epsilon}}: Epsilon values as described in section V.A
#'       of Munoz et al. (2015). The default is
#'       \code{c(0, 10^(seq(-5, 15, length.out = 1000))}.
#'       \item{\code{ic.sorting}}: Sorting strategy, which is used to define
#'       the tour through the landscape. Possible values are \code{"nn"}
#'       (= default) and \code{"random"}.
#'       \item{\code{ic.sample.generate}}: Should the initial design be created
#'       using a LHS? The default is \code{FALSE}, i.e. the initial design from
#'       the feature object will be used.
#'       \item{\code{ic.sample.dimensions}}: Dimensions of the initial design,
#'       if created using a LHS. The default is \code{feat.object$dimension}.
#'       \item{\code{ic.sample.size}}: Size of the initial design, if created
#'       using a LHS. The default is \code{100 * feat.object$dimension}.
#'       \item{\code{ic.sample.lower}}: Lower bounds of the initial design, if
#'       created with a LHS. The default is \code{100 * feat.object$lower}.
#'       \item{\code{ic.sample.upper}}: Upper bounds of the initial design, if
#'       created with a LHS. The default is \code{100 * feat.object$upper}.
#'       \item{\code{ic.aggregate_duplicated}}: How should observations, which
#'       have duplicates in the decision space, be aggregated? The default is
#'       \code{mean}.
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
#'     \item{dispersion features}: \itemize{
#'       \item{\code{disp.quantiles}}: Quantiles, which should be used for
#'       defining the "best" elements of the entire initial design. The default
#'       is \code{c(0.02, 0.05, 0.1, 0.25)}.
#'       \item{\code{disp.dist_method}}: Which distance method should be
#'       used for computing the distance between two observations? All methods
#'       of \code{\link{dist}} are possible options with \code{"euclidean"}
#'       being the default.
#'       \item{\code{disp.minkowski_p}}: Value of \code{p} in case
#'       \code{dist_meth} is \code{"minkowski"}. The default is \code{2}, i.e.
#'       the euclidean distance.
#'     }
#'     \item{nearest better clustering features}: \itemize{
#'       \item{\code{nbc.dist_method}}: Which distance method should be
#'       used for computing the distance between two observations? All methods
#'       of \code{\link{dist}} are possible options with \code{"euclidean"}
#'       being the default.
#'       \item{\code{nbc.minkowski_p}}: Value of \code{p} in case
#'       \code{dist_meth} is \code{"minkowski"}. The default is \code{2}, i.e.
#'       the euclidean distance.
#'       \item{\code{nbc.dist_tie_breaker}}: How will ties be broken when
#'       different observations have the same distance to an observation?
#'       Possible values are \code{"sample"}, \code{"first"} and \code{"last"}.
#'       The default is \code{"sample"}.
#'       \item{\code{nbc.cor_na}}: How should NA's be handled when computing
#'       correlations? Any method from the argument \code{use} of the function
#'       \code{\link{cor}} is possible. The default is
#'       \code{"pairwise.complete.obs"}.
#'       \item{\code{nbc.fast_k}}: In case of euclidean distances, the method
#'       can find neighbours faster. This parameter controls the percentage of
#'       observations that should be considered when looking for the nearest
#'       better neighbour, i.e. the nearest neighbour with a better objective
#'       value. The default is \code{0.05}, i.e. the 5% nearest neighbours.
#'     }
#'     \item{principal component features}: \itemize{
#'       \item{\code{pca.{cov, cor}_{x, init}}}: Which proportion of the
#'       variance should be explained by the principal components given a
#'       principal component analysis based on the covariance / correlation
#'       matrix of the decision space (\code{x}) or the entire initial
#'       design (\code{init})? The defaults are \code{0.9}.
#'     }
#'   }
#'
#' @references
#'   \itemize{
#'     \item{Kerschke, P., Preuss, M., Hernandez, C., Schuetze, O., Sun, J.-Q.,
#'     Grimme, C., Rudolph, G., Bischl, B., and Trautmann, H. (2014)}:
#'     \dQuote{Cell Mapping Techniques for Exploratory Landscape Analysis},
#'     in: EVOLVE -- A Bridge between Probability, Set Oriented Numerics, and
#'     Evolutionary Computation V, pp. 115-131
#'     (\url{http://dx.doi.org/10.1007/978-3-319-07494-8_9}).
#'     \item{Kerschke, P., Preuss, M., Wessing, S., and Trautmann, H. (2015)}:
#'     \dQuote{Detecting Funnel Structures by Means of Exploratory Landscape
#'     Analysis}, in: Proceedings of the 17th Annual Conference on Genetic and
#'     Evolutionary Computation (GECCO '15), pp. 265-272
#'     (\url{http://dx.doi.org/10.1145/2739480.2754642}).
#'     \item{Lunacek, M., and Whitley, D. (2006)}:
#'     \dQuote{The dispersion metric and the CMA evolution strategy}, in:
#'     Proceedings of the 8th Annual Conference on Genetic and Evolutionary
#'     Computation (GECCO '06), pp. 477-484
#'     (\url{http://dx.doi.org/10.1145/1143997.1144085}).
#'     \item{Mersmann, O., Bischl, B., Trautmann, H., Preuss, M., Weihs, C.,
#'     and Rudolph, G. (2011)}: \dQuote{Exploratory Landscape Analysis}, in:
#'     Proceedings of the 13th Annual Conference on Genetic and Evolutionary
#'     Computation (GECCO '11), pp. 829-836
#'     (\url{http://dx.doi.org/10.1145/2001576.2001690}).
#'     \item{Munoz, M. A., Kirley, M., and Halgamuge, S. K. (2015)}:
#'     \dQuote{Exploratory Landscape Analysis of Continuous Space Optimization
#'     Problems Using Information Content}, in: IEEE Transactions on
#'     Evolutionary Computation (19:1), pp. 74-87
#'     (\url{http://dx.doi.org/10.1109/TEVC.2014.2302006}).
#'   }
#'
#' @examples
#' # (1) create a feature object:
#' X = t(replicate(n = 2000, expr = runif(n = 5, min = -10, max = 10)))
#' \dontrun{feat.object = createFeatureObject(X = X, fun = function(x) sum(x^2))}
#'
#' # (2) compute all non-cellmapping features
#' ctrl = list(allow_cellmapping = FALSE)
#' \dontrun{features = calculateFeatures(feat.object, control = ctrl)}
#'
#' # (3) in order to allow the computation of the cell mapping features, one
#' # has to provide a feature object that has knowledge about the number of
#' # cells per dimension:
#' f = function(x) sum(x^2)
#' feat.object = createFeatureObject(X = X, fun = f, blocks = 3)
#' \dontrun{features = calculateFeatures(feat.object)}
#'
#' # (4) if you want to compute a specific feature set, you can use
#' # calculateFeatureSet:
#' features.angle = calculateFeatureSet(feat.object, "cm_angle")
#'
#' # (5) as noted in the details, it might be useful to compute the levelset
#' # features parallelized:
#' \dontrun{
#' library(parallelMap)
#' library(parallel)
#' n.cores = detectCores()
#' parallelStart(mode = "multicore", cpus = n.cores,
#'   logging = FALSE, show.info = FALSE)
#' system.time((levelset.par = calculateFeatureSet(feat.object, "ela_level")))
#' parallelStop()
#' system.time((levelset.seq = calculateFeatureSet(feat.object, "ela_level")))}
#'
#' @name calculateFeatures
#' @rdname features
#' @export 
calculateFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  possible = as.character(unlist(listAllFeatureSets()))
  if (missing(control))
    control = list()
  assertList(control)
  prog = control_parameter(control, "show_progress", TRUE)
  subset = control_parameter(control, "subset", possible)
  assertSubset(subset, choices = possible)
  allow.cellmapping = control_parameter(control, "allow_cellmapping", TRUE)
  assertLogical(allow.cellmapping)
  allow.costs = control_parameter(control, "allow_costs",
    !is.null(feat.object$fun))
  assertLogical(allow.costs)
  blacklist = control_parameter(control, "blacklist", NULL)
  if (feat.object$dim > 2)
    blacklist = unique(c(blacklist, "bt"))
  assertSubset(blacklist, choices = possible)

  pure_cm = setdiff(listAvailableFeatureSets(allow.cellmapping = TRUE),
    listAvailableFeatureSets(allow.cellmapping = FALSE))
  if (allow.cellmapping && !feat.object$allows.cellmapping && any(pure_cm %in% setdiff(subset, blacklist)))
    stop ("This feature object does not support cell mapping. You first need to define the number of cells per dimension before computing these features.")

  expensive = setdiff(listAvailableFeatureSets(allow.additional_costs = TRUE),
    listAvailableFeatureSets(allow.additional_costs = FALSE))  
  if (allow.costs & is.null(feat.object$fun) & any(expensive %in% setdiff(subset, blacklist)))
    stop("The local search features require the exact function!")
  sets = listAvailableFeatureSets(subset, allow.cellmapping, allow.costs, blacklist)

  if (prog) {
    bar = makeProgressBar(min = 0, max = length(sets), label = "")
    no_chars = max(nchar(sets))
    features = lapply(sets, function(set) {
      txt = paste0(set, paste(rep(" ", no_chars - nchar(set)), collapse = ""))
      bar$inc(1L, txt)
      calculateFeatureSet(feat.object, set, control, ...)
    })
  } else {
    features = lapply(sets, function(set) calculateFeatureSet(feat.object, set, control, ...))    
  }
  do.call(c, features)
}
