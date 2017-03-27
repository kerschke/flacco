

#' Shiny ui-module for Smoof Batch import
#'
#' \code{SmoofImportPage} is a shiny ui-component which can be added to your shiny app
#' so that you get a Batch import for a specific Smoof function but different parameters
#'
#' It will load a CSV-file with parameters for the smoof function and calculate the selected features
#' for the specific function.
#'
#'@param id ID for the shiny component
#'@export
SmoofImportPage <- function(id) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  # Sidebar with a slider input for the number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("smoof_import_file"), label = "File to import"),
      shiny::selectInput(ns("smoof_import_function"),label="Function name",choices=smoof::filterFunctionsByTags("single-objective")),
      shiny::selectInput(ns("smoof_import_featureSet"),label="Feature Set",choices=listAvailableFeatureSets()),
      shiny::downloadButton(ns('smoof_import_downloadData'), 'Download')
    ),
    # Show a table with the generated features
    shiny::mainPanel(
      shiny::tableOutput(ns("smoof_import_FeatureTable"))
    )
  )
}


#' Shiny server function for BBOB import page module
#'
#' \code{SmoofImport} is a Shiny server function which will control all aspects
#' of the SmoofImportPage UI Module. Will be called with \code{callModule()}
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringsAsFactors How to treat strings in application (for shiny internally)
#'
#' @export
#'
SmoofImport <- function(input, output, session, stringsAsFactors) {
  `%then%` <- shiny:::`%OR%`
  # BBOB functions is using the smoof package for implementing them
  if (!requireNamespace("smoof", quietly = TRUE)) {
    stop("smoof needed for this function to work. Please install it.",
         call. = FALSE)
  }

  #function for controlling the file input app
  smoof_import_createFeatures <- shiny::reactive({
    features <- data.frame()
    importdata <- utils::read.csv(input$smoof_import_file$datapath,sep = ",",header=TRUE) #load values from uploaded file
    # calculate features for all rows of input file
    for (i in 1:nrow(importdata))
    {
      #X <- expand.grid(seq(0, 1, length.out = 50), seq(0, 1, length.out = 50))
      X <- flacco::createInitialSample(n.obs=2500, dim=importdata[i,1])
      f <- smoof::makeFunctionsByName(input$smoof_import_function, dimensions = importdata[i,1])
      y <- apply(X, 1, f[[1]])
      feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f[[1]])
      features_l <- data.frame(flacco::calculateFeatureSet(feat.object, set = input$smoof_import_featureSet)) #calculate the features
      features <- rbind(features , features_l)
    }
    features
  })

  output$smoof_import_FeatureTable <- shiny::renderTable({
    features<- smoof_import_createFeatures()
  },rownames = TRUE,colnames=TRUE)


  output$smoof_import_downloadData <- shiny::downloadHandler(
    filename = function() { paste(input$smoof_import_featureSet, '.csv', sep='') },
    content = function(file) {
      utils::write.csv(smoof_import_createFeatures(), file)
    }
  )
}
