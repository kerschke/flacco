#' Run the flacco-GUI based on shiny
#'
#' \code{runFlaccoGUI} starts a shiny application integrating the features of flacco. 
#'
#' A shiny application is a web-app which can be accessed through an browser. 
#'
#'@export
runFlaccoGUI <- function() {
  appDir <- system.file("flaccogui", package = "flacco")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(" shiny needed for this function to work. Please install it.",
         call. = FALSE)
  }
  #smoof::filterFunctionsByTags("single-objective")
  shiny::runApp(appDir, display.mode = "normal")
}
