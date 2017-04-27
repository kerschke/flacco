#' Shiny UI-Module for Batch Import of Smoof Functions
#'
#' \code{SmoofImportPage} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} UI-component which can be
#' added to your \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} app so that you get a batch import for a
#' specific \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}} function but different parameters.
#'
#' It will load a CSV-file with parameters for the \href{https://CRAN.R-project.org/package=smoof}{\code{smoof}} function
#' and calculate the selected features for the specific function.
#'
#' @template arg_ui_id
#' @export
SmoofImportPage = function(id) {
  # Create a namespace function using the provided id
  ns = shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("smoof_import_file"), label = "File to import"),
      shiny::numericInput(ns("smoof_import_replication"), label = "Replications", value = 1),
      shiny::selectInput(ns("smoof_import_function"), label = "Function name",
        choices = smoof::filterFunctionsByTags("single-objective")),
      shiny::selectInput(ns("smoof_import_featureSet"), label = "Feature Set",
        choices = listAvailableFeatureSets()),
      shiny::textInput(ns("smoof_block_input"), label = "Blocks (comma sperated per dimension)", value = 2),
      shiny::sliderInput(ns("smoof_ssize"), "Sample size", min = 10, max = 5000, value = 100),
      shiny::downloadButton(ns('smoof_import_downloadData'), 'Download')
    ),
    # Show a table with the generated features
    shiny::mainPanel(
      shiny::tableOutput(ns("smoof_import_FeatureTable"))
    )
  )
}

#' Shiny Server Function for BBOB Import Page Module
#'
#' \code{SmoofImport} is a \href{https://CRAN.R-project.org/package=shiny}{\code{shiny}}
#' server function which will control all aspects of the \code{SmoofImportPage}-UI Module.
#' It will be called with \code{\link[shiny]{callModule}}.
#'
#' @template arg_ui_input
#' @template arg_ui_output
#' @template arg_ui_session
#' @template arg_ui_stringsAsFactors
#'
#' @export
SmoofImport = function(input, output, session, stringsAsFactors) {
  # function for controlling the file input app
  smoof_import_createFeatures = shiny::reactive({
    features = data.frame()
    # load values from uploaded file
    importdata = utils::read.csv(input$smoof_import_file$datapath, sep = ",", header = TRUE)
    # calculate features for all rows of input file
    for (i in seq_row(importdata)) {
      for (r in seq_len(input$BBOB_import_replication)) {
        X = createInitialSample(n.obs = input$smoof_ssize, dim = importdata[i, 1L])
        f = smoof::makeFunctionsByName(input$smoof_import_function, dimensions = importdata[i, 1L])[[1L]]
        y = apply(X, 1, f)
        feat.object = createFeatureObject(X = X, y = y, fun = f)
        # calculate the features
        if (input$smoof_block_input != ""){
          #validate the input for block
          shiny::validate(
            shiny::need(try({blocks = eval(parse(text = paste("c(", input$smoof_block_input, ")")))}),
              "Please insert valid block defintion") %then%
            shiny::need(try({feat.object = createFeatureObject(X = X, y = y, fun = f, blocks = blocks)}),
              "Please insert valid funciton values")
          )
        } else {
          feat.object = createFeatureObject(X = X, y = y, fun = f)
        }
        # calculate the features
        features_l = data.frame(dim = importdata[i, 1L], rep = r,
          calculateFeatureSet(feat.object, set = input$smoof_import_featureSet,
            control = list(ela_curv.sample_size = min(200L, feat.object$n.obs))))
        features = rbind(features, features_l)
      }
    }
    features
  })

  output$smoof_import_FeatureTable = shiny::renderTable({
    features = smoof_import_createFeatures()
  }, rownames = TRUE, colnames = TRUE)

  output$smoof_import_downloadData = shiny::downloadHandler(
    filename = function() {
      paste0(input$smoof_import_featureSet, '.csv')
    },
    content = function(file) {
      utils::write.csv(smoof_import_createFeatures(), file)
    }
  )
}
