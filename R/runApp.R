#' @export
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
