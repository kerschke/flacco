#' Run the flacco-GUI based on Shiny
#'
#' \code{runFlaccoGUI} starts a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}}
#' application, which allows the user to compute the flacco features and also visualize
#' the underlying functions.
#'
#' A \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} application is a web-app
#' which can be accessed through a browser. 
#'
#' @references
#'   \itemize{
#'     \item{Hanster, C., and Kerschke, P. (2017)}:
#'     \dQuote{flaccogui: Exploratory Landscape Analysis for Everyone},
#'     in: Proceedings of the 19th Annual Conference on Genetic an
#'     Evolutionary Computation (GECCO) Companion, pp. 1215-1222, ACM.
#'     (\url{http://dl.acm.org/citation.cfm?doid=3067695.3082477}).
#'     \item{Kerschke, P., and Trautmann, H. (2019)}:
#'     \dQuote{Comprehensive Feature-Based Landscape Analysis of Continuous
#'     and Constrained Optimization Problems Using the R-package flacco},
#'     in: Applications in Statistical Computing -- From Music Data Analysis
#'     to Industrial Quality Improvement, pp. 93-123, Springer.
#'     (\url{https://link.springer.com/chapter/10.1007/978-3-030-25147-5_7}).
#'   }
#'
#'@export
runFlaccoGUI = function() {
  # the app requires the smoof package
  if (!requireNamespace("smoof", quietly = TRUE)) {
    stop("smoof needed for this function to work. Please install it.", call. = FALSE)
  }
  appDir = system.file("flaccogui", package = "flacco")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny needed for this function to work. Please install it.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
