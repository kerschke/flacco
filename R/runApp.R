#' @export
runFlaccoGUI <- function() {
  appDir <- system.file("shiny-app", package = "flaccogui")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
