#' @export
runFlaccoGUI <- function() {
  appDir <- system.file("flaccogui", package = "flacco")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
