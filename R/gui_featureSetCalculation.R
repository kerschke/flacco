library(flacco)
library(shiny)
`%then%` <- shiny:::`%OR%`


#' Shiny component for Calculating and displaying flacco FeatureSets
#'
#' \code{FeatureSetCalculationComponent} is a shiny component which can be added to your shiny app
#' so that you can calculate and display different Feature Sets.
#'
#' It will get a flacco feature.object for the next steps
#'
#'@param id ID for the shiny component
#'@export
FeatureSetCalculationComponent <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Sidebar with a slider input for the number of bins
  div(
  selectInput(ns("FeatureSet_function"),label="Feature Set",choices=c("all Features",listAvailableFeatureSets()), selected = "cm_angle"),
  tableOutput(ns("FeatureTable_function")),
  downloadButton(ns('downloadData_function'), 'Download'))
}


#' Shiny server function for FeatureSet Component
#'
#' \code{FeatureSetCalculation} is a Shiny server function which will control all aspects
#' of the FeatureSetCalculationComponent UI Module. Will be called with \code{callModule()}
#'
#' @param input Shiny input variable for the specific UI module
#' @param output Shiny output variable for the specific UI module
#' @param session Shiny session variable for the specific UI module
#' @param stringAsFactors
#'
#' @export
#'
FeatureSetCalculation <- function(input, output, session, stringsAsFactors, feat.object) {

  # wrap the feature calculation in a reactive so it will only recalculated, when user input has changed
  features <- reactive({
    if (input$FeatureSet_function == "all Features")
    {
      features <- flacco::calculateFeatures(feat.object()) #calculate all the features
      features <- data.frame(t(data.frame(features)),stringsAsFactors=FALSE) #flip data.frame around
    } else {
      print(feat.object)
      features <- flacco::calculateFeatureSet(feat.object(), set = input$FeatureSet_function) #calculate the features
      features <- data.frame(t(data.frame(features)),stringsAsFactors=FALSE) #flip data.frame around
    }
    return(features)
  })

  #render Table with features
  output$FeatureTable_function <- renderTable({
    features()
  },rownames = TRUE,colnames=FALSE)

  #click Function for the download button
  output$downloadData_function <- downloadHandler(
    filename = function() { paste(input$FeatureSet_function, '.csv', sep='') },
    content = function(file) {
      write.csv(features(), file)
    }
  )
}
