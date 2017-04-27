#' Shiny UI-Module for Batch Import of BBOB Functions
#'
#' \code{BBOBImportPage} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} component which can be added to
#' your \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} app so that you get a batch import for several BBOB
#' functions.
#'
#' It will load a CSV-file with BBOB parameters (the function ID, instance ID and
#' problem dimension) and then calculate the selected features for the specific
#' function(s).
#'
#' @template arg_ui_id
#' 
#' @export
BBOBImportPage = function(id) {
  # Create a namespace function using the provided id
  ns = shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("BBOB_import_file"), label = "File to import"),
      shiny::numericInput(ns("BBOB_import_replication"), label = "Replications", value = 1),
      shiny::selectInput(ns("BBOB_import_featureSet"),label = "Feature Set", choices = listAvailableFeatureSets()),
      shiny::textInput(ns("BBOB_block_input"), label = "Blocks (comma sperated per dimension)", value = 2),
      shiny::sliderInput(ns("BBOB_ssize"), "Sample size", min = 10, max = 5000, value = 100),
      shiny::downloadButton(ns('BBOB_import_downloadData'), 'Download')
    ),
    # Show a table with the generated features
    shiny::mainPanel(
      shiny::tableOutput(ns("BBOB_import_FeatureTable"))
    )
  )
}


#' Shiny server function for BBOB import page module
#'
#' \code{BBOBImport} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} server function which will
#' control all aspects of the \code{\link{BBOBImportPage}} UI Module. It
#' will be called with \code{\link[shiny]{callModule}}.
#'
#' @template arg_ui_input
#' @template arg_ui_output
#' @template arg_ui_session
#' @template arg_ui_stringsAsFactors
#'
#' @export
BBOBImport = function(input, output, session, stringsAsFactors) {
  #function for controlling the file input app
  BBOB_import_createFeatures = shiny::reactive({
    features = data.frame()
    # load values from uploaded file
    importdata = utils::read.csv(input$BBOB_import_file$datapath, sep = ",", header = TRUE)
    # calculate features for all rows of input file
    for (i in seq_row(importdata)) {
      for (r in seq_len(input$BBOB_import_replication)) {
        X = createInitialSample(n.obs = input$BBOB_ssize, dim = importdata[i, 3L])
        f = smoof::makeBBOBFunction(dimension = importdata[i, 3L], fid = importdata[i, 1L], iid = importdata[i, 2L])
        y = apply(X, 1L, f)
        # check if input for blocks is available
        if (input$BBOB_block_input != ""){
          # validate the input for block
          shiny::validate(
            shiny::need(try({blocks = eval(parse(text = paste("c(", input$BBOB_block_input, ")")))}),
              "Please insert valid block defintion") %then%
            shiny::need(try({feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = blocks)}),
              "Please insert valid funciton values")
          )
        } else {
          feat.object = createFeatureObject(X = X, y = y, fun = f)
        }
        # calculate the features
        features_l = data.frame(fid = importdata[i, 1L], iid = importdata[i, 2L],
          dim = importdata[i, 3L], rep = r,
          calculateFeatureSet(feat.object, set = input$BBOB_import_featureSet,
            control = list(ela_curv.sample_size = min(200L, feat.object$n.obs))))
        features = rbind(features, features_l)
      }
    }
    features
  })

  output$BBOB_import_FeatureTable = shiny::renderTable({
    features = BBOB_import_createFeatures()
  }, rownames = FALSE, colnames = TRUE)

  output$BBOB_import_downloadData = shiny::downloadHandler(
    filename = function() {
      paste0(input$BBOB_import_featureSet, ".csv")
    },
    content = function(file) {
      utils::write.csv(BBOB_import_createFeatures(), file)
    }
  )
}
