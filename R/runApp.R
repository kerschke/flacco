#' Run the flacco-GUI based on Shiny
#'
#' \code{runFlaccoGUI} starts a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}}
#' application, which allows the user to compute the flacco features and also visualize
#' the underlying functions.
#'
#' A \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} application is a web-app
#' which can be accessed through a browser. 
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
