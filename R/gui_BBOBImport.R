library(flacco)
library(shiny)
`%then%` <- shiny:::`%OR%`


#' Shiny component for BBOB Batch import
#'
#' \code{BBOBImportPage} is a shiny component which can be added to your shiny app
#' so that you get a Batch import for several BBOB functions.
#'
#' It will load a CSV-file with BBOB parameters and calculate the selected features
#' for the specific function.
#'
#'@param id ID for the shiny component
#'@export
BBOBImportPage <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("BBOB_import_file"), label = "File to import"),
      numericInput(ns("BBOB_import_replication"), label="Replications", value=1),
      selectInput(ns("BBOB_import_featureSet"),label="Feature Set",choices=listAvailableFeatureSets()),
      textInput(ns("BBOB_block_input"), label="Blocks (comma sperated per dimension)"),
      sliderInput(ns("BBOB_ssize"),
                  "Sample size",
                  min = 100,
                  max = 10000,
                  value = 30),
      downloadButton(ns('BBOB_import_downloadData'), 'Download')
    ),
    # Show a table with the generated features
    mainPanel(
      tableOutput(ns("BBOB_import_FeatureTable"))
    )
  )
}


#' Shiny server function for BBOB import page module
#'
#' \code{BBOBImport} is a Shiny server function which will control all aspects
#' of the BBOBImportPage UI Module. Will be called with \code{callModule()}
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringAsFactors
#'
#' @export
#'
BBOBImport <- function(input, output, session, stringsAsFactors) {
  # BBOB functions is using the smoof package for implementing them
  if (!requireNamespace("smoof", quietly = TRUE)) {
    stop("smoof needed for this function to work. Please install it.",
         call. = FALSE)
  }

  #function for controlling the file input app
  BBOB_import_createFeatures <- reactive({
    if (!requireNamespace("smoof", quietly = TRUE)) {
      stop("smoof needed for this function to work. Please install it.",
           call. = FALSE)
    }
    features<-data.frame()
    importdata=read.csv(input$BBOB_import_file$datapath,sep = ",",header=TRUE) #load values from uploaded file
    # calculate features for all rows of input file
    for (i in 1:nrow(importdata))
    {
      for (r in 1:input$BBOB_import_replication)
      {
        #X <- expand.grid(seq(0, 1, length.out = 50), seq(0, 1, length.out = 50))
        X <- flacco::createInitialSample(n.obs=input$BBOB_ssize, dim=importdata[i,3])
        f <- smoof::makeBBOBFunction(dimension = importdata[i,3], fid = importdata[i,1], iid = importdata[i,2])
        y <- apply(X, 1, f)
        if (input$BBOB_block_input!=""){ #check if input for blocks is available
          #validate the input for block
          validate(
            need(try( blocks <- eval(parse(text=paste("c(",input$BBOB_block_input,")")))), "Please insert valid block defintion") %then%
              need(try(feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f, blocks=blocks)), "Please insert valid funciton values")
          )
        } else {
          feat.object <- flacco::createFeatureObject(X = X, y = y, fun = f)
        }
        features_l <- data.frame(fid = importdata[i,1], iid = importdata[i,2], rep = r, flacco::calculateFeatureSet(feat.object, set = input$BBOB_import_featureSet)) #calculate the features
        features <- rbind(features, features_l)
      }
    }
    features
  })

  output$BBOB_import_FeatureTable <- renderTable({
    features<- BBOB_import_createFeatures()
  },rownames = FALSE, colnames=TRUE)


  output$BBOB_import_downloadData <- downloadHandler(
    filename = function() { paste(input$BBOB_import_featureSet, '.csv', sep='') },
    content = function(file) {
      write.csv(BBOB_import_createFeatures(), file)
    }
  )
}
