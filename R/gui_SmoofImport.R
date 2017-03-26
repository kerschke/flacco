library(flacco)
library(shiny)
`%then%` <- shiny:::`%OR%`


#' Shiny component for Smoof Batch import
#'
#' \code{SmoofImportPage} is a shiny component which can be added to your shiny app
#' so that you get a Batch import for a specific Smoof function but different parameters
#'
#' It will load a CSV-file with parameters for the smoof function and calculate the selected features
#' for the specific function.
#'
#'@param id ID for the shiny component
#'@export
SmoofImportPage <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("smoof_import_file"), label = "File to import"),
      selectInput(ns("smoof_import_function"),label="Function name",choices=filterFunctionsByTags("single-objective")),
      selectInput(ns("smoof_import_featureSet"),label="Feature Set",choices=listAvailableFeatureSets()),
      downloadButton(ns('smoof_import_downloadData'), 'Download')
    ),
    # Show a table with the generated features
    mainPanel(
      tableOutput(ns("smoof_import_FeatureTable"))
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
#' @param stringAsFactors
#'
#' @export
#'
SmoofImport <- function(input, output, session, stringsAsFactors) {
  # BBOB functions is using the smoof package for implementing them
  if (!requireNamespace("smoof", quietly = TRUE)) {
    stop("smoof needed for this function to work. Please install it.",
         call. = FALSE)
  }

  #function for controlling the file input app
  smoof_import_createFeatures <- reactive({

    features<-data.frame()
    importdata=read.csv(input$smoof_import_file$datapath,sep = ",",header=TRUE) #load values from uploaded file
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

  output$smoof_import_FeatureTable <- renderTable({
    features<- smoof_import_createFeatures()
  },rownames = TRUE,colnames=TRUE)


  output$smoof_import_downloadData <- downloadHandler(
    filename = function() { paste(input$smoof_import_featureSet, '.csv', sep='') },
    content = function(file) {
      write.csv(smoof_import_createFeatures(), file)
    }
  )
}
